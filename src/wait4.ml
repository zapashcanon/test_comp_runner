type wait_result =
  { pid : int
  ; status : Unix.process_status
  ; user_time : float
  ; system_time : float
  }

external wait4 : pid:int -> wait_result = "caml_wait4"

(*
   let pid =
  (* Unix.create_process "ls" [|"ls"|] Unix.stdin Unix.stdout Unix.stderr *)
  let pid = Unix.fork () in
  if pid = 0 then begin
    for _i = 0 to 3_000_000_000 do
      ()
    done;
    (* Unix.sleepf 0.5; *)
    exit 0
  end
  else begin
    Unix.sleepf 0.5;
    Unix.kill pid Sys.sigkill;
    pid
  end

let { Wait4.pid = pid'; status; user_time; system_time } = Wait4.wait4 ~pid

let () = Printf.printf "pid %i %i\n%!" pid pid'
let () = Printf.printf "user time %f\n%!" user_time
let () = Printf.printf "system time %f\n%!" system_time
let () =
  match status with
  | WEXITED i -> Printf.printf "exited %i" i
  | WSIGNALED i -> Printf.printf "signaled %i" i
  | WSTOPPED i -> Printf.printf "stopped %i" i
*)
