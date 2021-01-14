(** Words, rewrite systems, and rewriting *)

type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word

type 's rewrite_rules = 's -> 's word

type 's system = {
    axiom : 's word;
    rules : 's rewrite_rules;
    interp : 's -> Turtle.command list }


val interp_direct_n :
  's word ->
  ('s -> 's word) ->
  ('s -> Turtle.command list) -> int -> int -> Turtle.position -> Turtle.position list -> float ->Turtle.position

val interp_direct_sys_n : 's system -> int -> Turtle.position -> int -> unit

val find_size_n :
  's word ->
  ('s -> 's word) ->
  ('s -> Turtle.command list) ->
  int ->
  int ->
  Turtle.position ->
  Turtle.position list ->
  float * float * float * float * Turtle.position ->
  float * float * float * float * Turtle.position

val find_window_size_n :
  Turtle.position -> 's system -> int -> float * float * float * float * Turtle.position

val read_lines : string -> string * string list * string list 

val string_to_char_word : string -> char word 

val string_to_rules : string list -> char -> char word 

val string_to_turtle_cmd : string -> Turtle.command list 

val string_to_interp : string list -> char -> Turtle.command list 

val file_to_char_sys : string -> char system 

 