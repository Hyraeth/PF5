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

(** Put here any type and function interfaces concerning systems *)

(** 
val iterate : 's word -> 's rewrite_rules -> 's word

val iterate_word_n : 's word -> 's rewrite_rules -> int -> int -> 's word

val iterate_n : 's system -> int -> 's word

val interp_word : 's word -> ('s -> Turtle.command list) -> Turtle.command list
*)

(** 
val interp_n :
  's word ->
  's rewrite_rules -> ('s -> Turtle.command list) -> int -> int -> Turtle.command list 

val interp_sys_n : 's system -> int -> Turtle.command list
*)

val interp_direct_n :
  's word ->
  ('s -> 's word) ->
  ('s -> Turtle.command list) -> int -> int -> Turtle.position -> Turtle.position list -> Turtle.position

val interp_direct_sys_n : 's system -> int -> Turtle.position -> unit

val find_size_n :
  'a word ->
  ('a -> 'a word) ->
  ('a -> Turtle.command list) ->
  int ->
  int ->
  Turtle.position ->
  Turtle.position list ->
  float * float * float * float * Turtle.position ->
  float * float * float * float * Turtle.position

val find_window_size_n :
  Turtle.position -> 'a system -> int -> float * float * float * float * Turtle.position
 