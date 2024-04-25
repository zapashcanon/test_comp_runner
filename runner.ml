(* #use "topfind";; *)
(* #require "yaml";; *)
(* #require "unix";; *)

[@@@ocaml.warning "-37"]

type filename = string
type path = string

type property = {
  file : string;
  expected : bool;
}

type problem = {
  input_files: string;
  properties: property list;
  language: string;
}

type tree =
  { files : (filename * problem) list; dirs : (filename * tree) list }

type runnable =
  | Dune_exec of path
  | In_path of string

type config =
  { problems_root : path;
    runnable : runnable;
  }

let good_problem (_name, problem) =
  problem.language = "C" &&
  List.exists (function { file; expected = false } ->
      Filename.check_suffix file "/unreach-call.prp" | _ -> false)
    problem.properties

let rec filter_test tree =
  let files = List.filter good_problem tree.files in
  let dirs =
    List.filter_map (fun (name, tree) ->
        let tree = filter_test tree in
        match tree.files, tree.dirs with
        | [], [] -> None
        | _, _ -> Some (name, tree))
      tree.dirs
  in
  { files; dirs }

let (let*) = Option.bind
let (let+) v f = Option.map f v

let object_field (yml : Yaml.value) (field : string) =
  match yml with
  | `O l ->
    List.assoc_opt field l
  | _ -> None

let array_map f (yml : Yaml.value) =
  match yml with
  | `A l ->
    Some (List.filter_map f l)
  | _ -> None

let bool = function
  | `Bool b -> Some b
  | _ -> None

let string = function
  | `String s -> Some s
  | _ -> None

let property (yml : Yaml.value) =
  let* file = object_field yml "property_file" in
  let* expected = object_field yml "expected_verdict" in
  let* file = string file in
  let+ expected = bool expected in
  { file; expected }

let problem (yml : Yaml.value) =
  let* input_files = object_field yml "input_files" in
  let* properties = object_field yml "properties" in
  let* options = object_field yml "options" in
  let* language = object_field options "language" in
  let* language = string language in
  let* properties = array_map property properties in
  let+ input_files = string input_files in
  { input_files; properties; language }

let parse_file path =
  let ic = In_channel.open_bin path in
  let str = In_channel.input_all ic in
  let r = Yaml.of_string str in
  In_channel.close ic;
  r

let list_yml_files ?(max=max_int) dir =
  let count = ref 0 in
  let to_get = max in
  let rec list_yml_files dir =
    assert(Sys.is_directory dir);
    let files = Sys.readdir dir in
    Array.fold_left (add dir) {files = []; dirs = []} files

  and add dir acc name =
    if !count > to_get then acc else
    let path = Filename.concat dir name in
    if Sys.is_directory path then
      let () = Printf.eprintf "Enter: %s\n%!" name in
      let subdir = list_yml_files path in
      let () = Printf.eprintf "Leave: %s\n%!" name in
      { acc with dirs = (name, subdir) :: acc.dirs }
    else if Filename.check_suffix name ".yml" then
      let yml = parse_file path in
      incr count;
      match yml with
      | Ok yml -> begin
        Printf.eprintf "OK: %s\n%!" name;
        match problem yml with
        | None ->
          Printf.eprintf "Not a good problem file: %s\n%!" name;
          acc
        | Some problem ->
          { acc with files = (name, problem) :: acc.files }
      end
      | Error (`Msg msg) ->
        Printf.eprintf "Couldn't parse file:\n%s\nreason: %s\n%!" path msg;
        acc
    else
      acc
  in
  list_yml_files dir

let timeout = 2.

type result =
  | Timeout
  | Ok of int
  | Killed

(* let rec wait_pid pid timeout = *)
(*   let t = Unix.gettimeofday () in *)
(*   let timeout = t +. timeout in *)
(*   let n, status = Unix.waitpid [WNOHANG] pid in *)
(*   if n = 0 then *)
(*     let t = Unix.gettimeofday () in *)
(*     if t >= timeout then *)
(*       let () = Unix.kill pid 9 in *)
(*       let _n, _status = Unix.waitpid [] pid in *)
(*       Timeout *)
(*     else *)
(*       let () = Unix.sleepf 0.1 in *)
(*       wait_pid pid timeout *)
(*   else *)
(*     match status with *)
(*     | WEXITED code -> Ok code *)
(*     | _ -> Killed *)

exception Sigchld

let wait_pid pid timeout =
  let did_timeout = ref false in
  let () =
    try
      let _ =
        Sys.set_signal Sys.sigchld (Signal_handle (fun _ -> raise Sigchld)) in
      Unix.sleepf timeout;
      did_timeout := true;
      let () = Unix.kill pid 9 in
      let _ = Sys.set_signal Sys.sigchld Signal_default in
      ()
    with Sigchld -> ()
  in
  let _ = Sys.set_signal Sys.sigchld Signal_default in
  let n, status = Unix.waitpid [] pid in
  assert(n = pid);
  if !did_timeout then Timeout else
    match status with
    | WEXITED code -> Ok code
    | _ -> Killed

let dune_run_on_file owi_project_dir file =
  Unix.chdir owi_project_dir;
  Unix.execvp "dune" [|"dune"; "exec"; "src/bin/owi.exe"; "--"; "c"; "--testcomp"; file |]

let path_run_on_file owi file =
  Unix.execvp owi [| owi; "c"; "--testcomp"; file |]

let run_on_file config file =
  match config.runnable with
  | In_path owi -> path_run_on_file owi file
  | Dune_exec owi_project_dir -> dune_run_on_file owi_project_dir file

let fork_and_run_on_file config file =
  let pid = Unix.fork () in
  let result =
    if pid = 0 then
      run_on_file config file
    else
      wait_pid pid timeout
  in
  match result with
  | Timeout -> Printf.eprintf "Timeout\n%!"
  | Ok code -> Printf.eprintf "Ok %i\n%!" code
  | Killed -> Printf.eprintf "Killed\n%!"

let rec tree_to_list dir tree =
  let files = List.map (fun (_name, problem) ->
      Filename.concat dir problem.input_files) tree.files in
  let dirs = List.map (fun (name, tree) -> tree_to_list (Filename.concat dir name) tree) tree.dirs in
  List.flatten (files :: dirs)

let config = {
  problems_root = "/home/chambart/test/Test-Comp/sv-benchmarks/c";
  runnable = Dune_exec "/home/chambart/code/woi_branches/main";
}

let t =
  let t = list_yml_files ~max:3 config.problems_root in
  filter_test t

let l = tree_to_list config.problems_root t
let () = List.iter (fork_and_run_on_file config) l
