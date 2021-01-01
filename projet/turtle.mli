
(** Turtle graphical commands *)
type command =
| Line of int      (** advance turtle while drawing *)
| Move of int      (** advance without drawing *)
| Turn of int      (** turn turtle by n degrees *)
| Store            (** save the current position of the turtle *)
| Restore          (** restore the last saved position not yet restored *)

(** Position and angle of the turtle *)
type position = {
  x: float;        (** position x *)
  y: float;        (** position y *)
  a: int;        (** angle of the direction *)
}

(** Put here any type and function signatures concerning turtle *)

val calc_pos : position -> command -> position

val exec_cmd : position -> command -> position list -> position 

val exec_cmd_list : position -> command list -> position list -> position

val draw_sys : position -> command list -> unit

val find_size :
  position ->
  command list ->
  float ->
  float -> float -> float -> position list -> float * float * float * float

val find_window_size :
  position -> command list -> float * float * float * float