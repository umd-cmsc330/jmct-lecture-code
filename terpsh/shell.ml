open Unix;;

type exec = { name: string;
              args: string list;
              redirect: string option
            }

type command =
  | Empty
  | CD of string
  | Prog of exec
  | Quit


let run_exec {name; args; redirect} =
  match Unix.fork () with
  | 0 -> (* We are in the child *)
         Unix.execvp name (Array.of_list (name::args))
         (* we'll never return from this :'( *)
         (* To do this properly, we should `dup2` the stdout + stdin *)
  | pid -> (* the parent! *)
      let (_, status) = Unix.waitpid [] pid in
      status

let get_exit_code es =
  match es with
  | WEXITED(c)   -> c
  | WSIGNALED(c) -> c
  | WSTOPPED(c)  -> c

let rec batch_shell commands =
  match commands with
  | []          -> exit 0
  | Quit::cs    -> exit 0
  | Empty::cs   -> batch_shell cs
  | CD(dir)::cs -> Unix.chdir dir
  | Prog(e)::cs ->
      let exit_status = run_exec e in
      (match exit_status with
       | WEXITED(0) -> batch_shell cs
       | _          -> exit (get_exit_code exit_status))


