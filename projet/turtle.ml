
type command =
| Line of int
| Move of int
| Turn of int
| Store
| Restore

type position = {
  x: float;      (** position x *)
  y: float;      (** position y *)
  a: int;      (** angle of the direction *)
}

(** Put here any type and function implementations concerning turtle *)

let pi = 4. *. atan 1.
let degrees_to_rad degrees = (float_of_int degrees) *. (pi /. 180.);;

let calc_pos (oldpos : position) (cmd : command) : position =
  match cmd with
  | Line x -> let z = float_of_int x in 
  {x = oldpos.x +. z*.(cos (degrees_to_rad oldpos.a));
   y = oldpos.y +. z*.(sin (degrees_to_rad oldpos.a));
   a = oldpos.a}
  | Move x -> let z = float_of_int x in 
  {x = oldpos.x +. z*.(cos (degrees_to_rad oldpos.a));
   y = oldpos.y +. z*.(sin (degrees_to_rad oldpos.a));
   a = oldpos.a}
  | Turn o -> {x = oldpos.x; y = oldpos.y; a = o + oldpos.a}
  | _ -> oldpos
;;

let exec_cmd (curpos : position) (cmd : command) (mempos : position list) : position = 
  let newpos = calc_pos curpos cmd in
  match cmd with
  | Line x -> Graphics.lineto (int_of_float newpos.x) (int_of_float newpos.y); newpos
  | Move x -> Graphics.moveto (int_of_float newpos.x) (int_of_float newpos.y); newpos
  | Turn a -> newpos
  | Store -> curpos
  | Restore -> (
    match mempos with
    | [] -> failwith "No position to restore"
    | m::ml -> Graphics.moveto (int_of_float m.x) (int_of_float m.y); m
  )
;;

let rec exec_cmd_list (curpos : position) (cmd_l : command list) (mempos : position list) =
  match cmd_l with
  | [] -> failwith "Command list empty"
  | [c] -> exec_cmd curpos c mempos
  | c::l -> 
    match c with
    | Restore -> (
      match mempos with
      | [] -> failwith "No position to restore"
      | m::ml -> let newpos = exec_cmd curpos c mempos in 
        exec_cmd_list newpos l ml
    )
    | Store -> exec_cmd_list curpos l (curpos::mempos)
    | _ -> let newpos = exec_cmd curpos c mempos in 
      exec_cmd_list newpos l mempos
;;

let draw_sys (curpos : position) (cmd_l : command list) = 
  Graphics.moveto (int_of_float curpos.x) (int_of_float curpos.y);
  let endpos = exec_cmd_list curpos cmd_l [] in
  Graphics.moveto (int_of_float curpos.x) (int_of_float curpos.y)
