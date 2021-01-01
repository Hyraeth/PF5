(** Words, rewrite systems, and rewriting *)
open Turtle

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

(** Premiere version avec mise en mémoire, maintenant inutile
let rec iterate word rules = 
  let rec iterate_list l rules =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> [iterate x rules]
    | x::xs -> (iterate x rules)::(iterate_list xs rules)
  in
  match word with
  | Symb s -> rules s
  | Seq l -> Seq (iterate_list l rules)
  | Branch b -> Branch (iterate b rules)
;;

let rec iterate_word_n word rules k n = 
  if(k < n) then
    iterate_word_n (iterate word rules) (rules) (k + 1) (n)
  else 
    word
;;

let iterate_n system n = 
  iterate_word_n (system.axiom) (system.rules) 0 n
;;

let rec interp_word word interp =
  let rec interp_word_list l =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> interp_word x interp
    | x::xs -> List.append (interp_word x interp) (interp_word_list xs)
  in
  match word with
  | Symb s -> interp s
  | Seq l -> interp_word_list l
  | Branch b -> Store::(List.append (interp_word b interp) ([Restore]))
;;
*)

let rec interp_n word rules interp k n =
  let rec interp_word_list (l : 's word list) : Turtle.command list =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> interp_n x rules interp k n
    | x::xs -> List.append (interp_n x rules interp k n) (interp_word_list xs)
  in
  match word with
  | Symb s -> (
    if (k<n) then 
      interp_n (rules s) rules interp (k+1) n
    else 
      interp s
  )
  | Seq l -> (
    match l with
    | [] -> failwith "Empty sequence"
    | [x] -> interp_n x rules interp k n
    | x::xl -> List.append (interp_n x rules interp k n) (interp_word_list xl)
  )
  | Branch b -> Store::(List.append (interp_n b rules interp k n) ([Restore]))
;;

let interp_sys_n sys n =
  interp_n sys.axiom sys.rules sys.interp 0 n
;;

let rec interp_direct_n word rules interp k n curpos mempos=
  let rec interp_word_list l curpos =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> interp_direct_n x rules interp k n curpos mempos
    | x::xs -> let newpos = interp_direct_n x rules interp k n curpos mempos in interp_word_list xs newpos
  in
  match word with
  | Symb s -> (
    if (k<n) then 
      interp_direct_n (rules s) rules interp (k+1) n curpos mempos
    else 
      Turtle.exec_cmd_list curpos (interp s) mempos
  )
  | Seq l -> (
    match l with
    | [] -> failwith "Empty sequence"
    | [x] -> interp_direct_n x rules interp k n curpos mempos
    | x::xl -> let newpos = interp_direct_n x rules interp k n curpos mempos in interp_word_list xl newpos
  )
  | Branch b -> interp_direct_n b rules interp k n curpos (curpos::mempos)
;;

let interp_direct_sys_n sys n curpos = 
  Graphics.moveto (int_of_float curpos.x) (int_of_float curpos.y);
  let newpos = interp_direct_n sys.axiom sys.rules sys.interp 0 n curpos [] in
  Graphics.moveto (int_of_float curpos.x) (int_of_float curpos.y);
;;

let rec find_size_n word rules interp k n curpos mempos res =
  let rec find_size_list l curpos =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> find_size_n x rules interp k n curpos mempos res
    | x::xs -> (
      let (xmax1, xmin1, ymax1, ymin1, newpos1) = (find_size_n x rules interp k n curpos mempos res) in 
      let (xmax2, xmin2, ymax2, ymin2, newpos2) = (find_size_list xs newpos1) in
      Turtle.expand_bounds_pos (xmax1, xmin1, ymax1, ymin1, newpos1) (xmax2, xmin2, ymax2, ymin2, newpos2)
    )
  in
  match word with
  | Symb s -> (
    if (k<n) then 
      find_size_n (rules s) rules interp (k+1) n curpos mempos res
    else 
      Turtle.find_size_pos curpos (interp s) (res) mempos
  )
  | Seq l -> (
    match l with
    | [] -> res
    | [x] -> find_size_n x rules interp k n curpos mempos res
    | x::xl -> (
      let (xmax1, xmin1, ymax1, ymin1, newpos1) = (find_size_n x rules interp k n curpos mempos res) in 
      let (xmax2, xmin2, ymax2, ymin2, newpos2) = (find_size_list xl newpos1) in 
      Turtle.expand_bounds_pos (xmax1, xmin1, ymax1, ymin1, newpos1) (xmax2, xmin2, ymax2, ymin2,newpos2)
    )
  )
  | Branch b -> find_size_n b rules interp k n curpos (curpos::mempos) res
;;

let find_window_size_n curpos sys n= 
  find_size_n sys.axiom sys.rules sys.interp 0 n curpos [] (curpos.x,curpos.x,curpos.y,curpos.y, curpos)
;;
