
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
  s: float;      (** scale to keep the window from growing too large*)  
}

(** Put here any type and function implementations concerning turtle *)

let pi = 4. *. atan 1.
let degrees_to_rad degrees = (float_of_int degrees) *. (pi /. 180.);;

let calc_pos (oldpos : position) (cmd : command) : position =
  match cmd with
  | Line x -> let z = float_of_int x in 
  {x = oldpos.x +. oldpos.s*.z*.(cos (degrees_to_rad oldpos.a));
   y = oldpos.y +. oldpos.s*.z*.(sin (degrees_to_rad oldpos.a));
   a = oldpos.a;
   s = oldpos.s}
  | Move x -> let z = float_of_int x in 
  {x = oldpos.x +. oldpos.s*.z*.(cos (degrees_to_rad oldpos.a));
   y = oldpos.y +. oldpos.s*.z*.(sin (degrees_to_rad oldpos.a));
   a = oldpos.a;
   s = oldpos.s}
  | Turn o -> {x = oldpos.x; y = oldpos.y; a = o + oldpos.a; s = oldpos.s}
  | _ -> oldpos
;;

let exec_cmd (curpos : position) (cmd : command) (mempos : position list) : position = 
  let newpos = calc_pos curpos cmd in
  match cmd with
  | Line x -> Graphics.lineto (int_of_float newpos.x) (int_of_float newpos.y); Unix.sleepf 0.01; newpos
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
;;

let expand_bounds_pos (xmax1, xmin1, ymax1, ymin1, pos1) (xmax2, xmin2, ymax2, ymin2, pos2) = 
  (max xmax1 xmax2, min xmin1 xmin2, max ymax1 ymax2, min ymin1 ymin2, pos2)
;;

let rec find_size_pos curpos cmd_l res mempos= 
  match cmd_l with
  | [] -> res
  | x::xl -> let nextpos = calc_pos curpos x in 
    match x with
    | Store -> find_size_pos curpos xl res (curpos::mempos)
    | Restore -> (
      match mempos with
      | [] -> failwith "No position to restore"
      | m::ml -> find_size_pos m xl (expand_bounds_pos res (m.x, m.x, m.y, m.y, m)) ml
    )
    | _ -> find_size_pos nextpos xl (expand_bounds_pos res (nextpos.x, nextpos.x, nextpos.y, nextpos.y, nextpos)) mempos
;;

let find_window_size curpos cmd_l = 
  find_size_pos curpos cmd_l (curpos.x,curpos.x,curpos.y,curpos.y,curpos) []
;;
