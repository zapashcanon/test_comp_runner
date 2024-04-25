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
  { timeout: float;
    problems_root : path;
    runnable : runnable;
    output_dir : path;
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

let ( let* ) = Option.bind
let ( let+ ) v f = Option.map f v

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
    if !count >= to_get then acc else
    let path = Filename.concat dir name in
    if Sys.is_directory path then
      (* let () = Printf.eprintf "Enter: %s\n%!" name in *)
      let subdir = list_yml_files path in
      (* let () = Printf.eprintf "Leave: %s\n%!" name in *)
      { acc with dirs = (name, subdir) :: acc.dirs }
    else if Filename.check_suffix name ".yml" then
      let yml = parse_file path in
      match yml with
      | Ok yml -> begin
        (* Printf.eprintf "OK: %s\n%!" name; *)
        match problem yml with
        | None ->
          Printf.eprintf "Not a good problem file: %s\n%!" name;
          acc
        | Some problem ->
          incr count;
          (* Printf.eprintf "problem: %s\n%!" problem.input_files; *)
          { acc with files = (name, problem) :: acc.files }
      end
      | Error (`Msg msg) ->
        Printf.eprintf "Couldn't parse file:\n%s\nreason: %s\n%!" path msg;
        acc
    else
      acc
  in
  list_yml_files dir

type times = {
  clock : float;
  user : float;
  system : float;
}

type process_result =
  | Timeout of times
  | Nothing of times
  | Reached of times
  | Other of int * times
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
  let start_time = Unix.gettimeofday () in
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
  let waited = Wait4.wait4 ~pid in
  let end_time = Unix.gettimeofday () in
  assert(waited.pid = pid);
  let times = {
    clock = end_time -. start_time;
    user = waited.user_time;
    system = waited.system_time;
  } in
  if !did_timeout then Timeout times else
    match waited.status with
    | WEXITED code ->
      if code = 0 then Nothing times
      else if code = 13 then Reached times
      else Other (code, times)
    | _ -> Killed

let dune_run_on_file owi_project_dir ~out_dir file =
  Unix.chdir owi_project_dir;
  Unix.execvp "dune" [|"dune"; "exec"; "src/bin/owi.exe"; "--"; "c"; "-o"; out_dir; file |]

let path_run_on_file owi ~out_dir file =
  Unix.execvp owi [| owi; "c"; "-o"; out_dir; file |]

let run_on_file config ~out_dir file =
  let new_stdout = Unix.openfile (Filename.concat out_dir "stdout") [O_CREAT; O_WRONLY] 0o666 in
  let new_stderr = Unix.openfile (Filename.concat out_dir "stderr") [O_CREAT; O_WRONLY] 0o666 in
  Unix.dup2 new_stdout Unix.stdout;
  Unix.dup2 new_stderr Unix.stderr;
  Unix.close new_stdout;
  Unix.close new_stderr;
  let out_dir = Filename.concat out_dir "owi" in
  match config.runnable with
  | In_path owi -> path_run_on_file owi ~out_dir file
  | Dune_exec owi_project_dir -> dune_run_on_file owi_project_dir ~out_dir file

let rec clear dir =
  if Sys.file_exists dir then begin
    let names = Sys.readdir dir in
    Array.iter (fun name ->
        let path = Filename.concat dir name in
        if Sys.is_regular_file path then Unix.unlink path
        else if Sys.is_directory path then clear path)
      names;
    try Unix.rmdir dir with _ ->
      Printf.eprintf "Failed to remove temp dir %s" dir
  end

let _ = ignore clear

let rec mkdir dir =
  let dirname = Filename.dirname dir in
  if not (Sys.is_directory dirname) then
    mkdir dirname;
  Unix.mkdir dir 0o777

let fork_and_run_on_file i config file =
  (* let out_dir = Filename.temp_dir "owi_result" "tmp" in *)
  let out_dir = Filename.concat config.output_dir (string_of_int i) in
  Unix.mkdir out_dir 0o777;
  let pid = Unix.fork () in
  let result =
    if pid = 0 then
      run_on_file config ~out_dir file
    else
      wait_pid pid config.timeout
  in
  let () =
    match result with
    | Timeout times -> Printf.eprintf "Timeout in %g %g %g\n%!" times.clock times.user times.system
    | Nothing times -> Printf.eprintf "Nothing in %g %g %g\n%!" times.clock times.user times.system
    | Reached times -> Printf.eprintf "Reached in %g %g %g\n%!" times.clock times.user times.system
    | Other (code, times) -> Printf.eprintf "Other %i in %g %g %g\n%!" code times.clock times.user times.system
    | Killed -> Printf.eprintf "Killed\n%!"
  in
  (* clear temp_dir; *)
  result

let rec tree_to_list dir tree =
  let files = List.map (fun (_name, problem) ->
      Filename.concat dir problem.input_files, problem) tree.files in
  let dirs = List.map (fun (name, tree) -> tree_to_list (Filename.concat dir name) tree) tree.dirs in
  List.flatten (files :: dirs)

type result = {
  problem : problem;
  result : process_result;
}

let results = ref []

let quick_results results =
  let nothing = ref 0 in
  let reached = ref 0 in
  let timeout = ref 0 in
  let bad = ref 0 in
  List.iter (fun res ->
      ignore res.problem;
      match res.result with
      | Nothing _ -> incr nothing
      | Reached _ -> incr reached
      | Timeout _ -> incr timeout
      | Other _ | Killed -> incr bad) results;
  Printf.printf "Nothing: %6i    Reached: %6i    Timeout: %6i    Bad: %6i\n%!" !nothing !reached !timeout !bad

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "USE: runner [PROBLEMS_DIR] [OUTPUT_DIR] [TIMEOUT]\n%!";
    exit 1
  end
let problems_root = Sys.argv.(1)
let runnable = In_path "owi"
let output_dir = Sys.argv.(2)
let timeout =
  if Array.length Sys.argv >= 5 then
    float_of_string Sys.argv.(3)
  else
    5.
let max_explo =
  if Array.length Sys.argv >= 5 then Some (int_of_string Sys.argv.(4))
  else None

let config = {
  timeout; problems_root; runnable; output_dir
}

(* let config = { *)
(*   timeout = 5.; *)
(*   problems_root = "/home/chambart/test/Test-Comp/sv-benchmarks/c"; *)
(*   runnable = Dune_exec "/home/chambart/code/woi_branches/main"; *)
(*   output_dir = "/tmp/ploup/"; *)
(* } *)


let t =
  let t = list_yml_files ?max:max_explo config.problems_root in
  filter_test t

let l = tree_to_list config.problems_root t
let () = Printf.printf "RUNNING %i problems\n%!" (List.length l)
let () = clear config.output_dir
let () = mkdir config.output_dir
let () = List.iteri (fun i (file, problem) ->
    Printf.printf "Run %s\n%!" file;
    let result = fork_and_run_on_file i config file in
    results := {problem; result} :: !results;
    quick_results !results
  ) l

