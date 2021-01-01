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

(** Put here any type and function implementations concerning systems *)

let rec iterate (word : 's word) (rules : 's rewrite_rules) : 's word = 
  let rec iterate_list (l : 's word list) (rules : 's rewrite_rules) : 's word list =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> [iterate x rules]
    | x :: xs -> (iterate x rules) :: iterate_list xs rules
  in
  match word with
  | Symb s -> rules s
  | Seq l -> Seq (iterate_list l rules)
  | Branch b -> Branch (iterate b rules)
;;

let rec iterate_word_n (word : 's word) (rules : 's rewrite_rules) (k : int) (n : int) : 's word = 
  if(k < n) then
    iterate_word_n (iterate word rules) (rules) (k + 1) (n)
  else 
    word
;;

let iterate_n (system : 's system) (n : int) : 's word = 
  iterate_word_n (system.axiom) (system.rules) 0 n
;;

let rec interp_word (word : 's word) (interp : 's -> Turtle.command list) : Turtle.command list =
  let rec interp_word_list (l : 's word list) : Turtle.command list =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> interp_word x interp
    | x :: xs -> List.append (interp_word x interp)  (interp_word_list xs)
  in
  match word with
  | Symb s -> interp s
  | Seq l -> interp_word_list l
  | Branch b -> Store :: List.append (interp_word b interp) ([Restore])
;;