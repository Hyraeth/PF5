
type command =
| Line of int
| Move of int
| Turn of int
| Store
| Restore

type position = {
  x: float;      (** position x *)
  y: float;      (** position y *)
  a: int;        (** angle of the direction *)
}

(** Put here any type and function implementations concerning turtle *)

let calc_pos (oldpos : position) (cmd : command) : position =
  match cmd with
  | Line x -> let z = float_of_int x in {x = z*.(cos (float_of_int oldpos.a)); y = z*.(sin (float_of_int oldpos.a)); a = oldpos.a}
  | Move x -> let z = float_of_int x in {x = z*.(cos (float_of_int oldpos.a)); y = z*.(sin (float_of_int oldpos.a)); a = oldpos.a}
  | Turn o -> {x = oldpos.x; y = oldpos.y; a = o}
  | _ -> oldpos
;;

let exec_cmd (curpos : position) (cmd : command) (mempos : position) : position = 
  let newpos = calc_pos curpos cmd in
  match cmd with
  | Line x -> Graphics.lineto (int_of_float newpos.x) (int_of_float newpos.y); newpos
  | Move x -> Graphics.moveto (int_of_float newpos.x) (int_of_float newpos.y); newpos
  | Turn a -> newpos
  | Store -> curpos
  | Restore -> Graphics.moveto (int_of_float mempos.x) (int_of_float mempos.y); mempos
;;

let rec exec_cmd_list (curpos : position) (cmd_l : command list) (mempos : position) =
  match cmd_l with
  | [] -> failwith "Command list empty"
  | [c] -> exec_cmd curpos c mempos
  | c::l -> let newpos = exec_cmd curpos c mempos in
    match c with
    | Store -> exec_cmd_list newpos l newpos
    | _ -> exec_cmd_list newpos l mempos
;;