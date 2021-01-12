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

(** Inutile maintenant qu'on execute directement les commandes à la volée
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
*)

let rec interp_direct_n word rules interp k n curpos mempos anim=
  let rec interp_word_list l curpos =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> interp_direct_n x rules interp k n curpos mempos anim
    | x::xs -> let newpos = interp_direct_n x rules interp k n curpos mempos anim in interp_word_list xs newpos
  in
  match word with
  | Symb s -> (
    if (k<n) then 
      interp_direct_n (rules s) rules interp (k+1) n curpos mempos anim
    else 
      if (anim>0.0) then (Unix.sleepf anim; Turtle.exec_cmd_list curpos (interp s) mempos)
      else Turtle.exec_cmd_list curpos (interp s) mempos
  )
  | Seq l -> (
    match l with
    | [] -> failwith "Empty sequence"
    | [x] -> interp_direct_n x rules interp k n curpos mempos anim
    | x::xl -> let newpos = interp_direct_n x rules interp k n curpos mempos anim in interp_word_list xl newpos 
  )
  | Branch b -> let newpos = interp_direct_n b rules interp k n curpos (curpos::mempos) anim in exec_cmd newpos Restore (curpos::mempos)
;;

let interp_direct_sys_n sys n curpos anim = 
  let sleep_time = if (anim>0) then (1.0 /. 10.0 ** (float_of_int anim)) else 0.0 in
  Graphics.moveto (int_of_float curpos.x) (int_of_float curpos.y);
  let newpos = interp_direct_n sys.axiom sys.rules sys.interp 0 n curpos [] sleep_time in
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
      Turtle.expand_bounds_pos (xmax1, xmin1, ymax1, ymin1, newpos1) (xmax2, xmin2, ymax2, ymin2, newpos2)
    )
  )
  | Branch b -> 
    let (xmax1, xmin1, ymax1, ymin1, newpos1) = find_size_n b rules interp k n curpos (curpos::mempos) res in
    let newpos2 = exec_cmd newpos1 Restore (curpos::mempos) in
    Turtle.expand_bounds_pos (xmax1, xmin1, ymax1, ymin1, newpos1) (newpos2.x, newpos2.x, newpos2.y, newpos2.y, newpos2)
;;

let find_window_size_n curpos sys n= 
  find_size_n sys.axiom sys.rules sys.interp 0 n curpos [] (curpos.x,curpos.x,curpos.y,curpos.y, curpos)
;;

let read_lines file=
  let in_ch = open_in file in
  let rec readline axiom rules interp n =
    let line = try input_line in_ch with End_of_file -> print_string "End file with \\n\n"; exit 1 in 
    if (String.length line = 0 && (n+1) = 3) then (axiom, rules, interp)
    else if (String.length line = 0) then readline axiom rules interp (n+1)
    else if (line.[0] = '#') then readline axiom rules interp n
    else (
      if (n = 0) then readline (line) rules interp n
      else if (n = 1) then readline axiom (line::rules) interp n
      else readline axiom rules (line::interp) n
    )
    
  in readline "tmp" [] [] 0
;;

let string_to_axiom string =
  if (String.length string = 1) then Symb string.[0]
  else (
    let rec string_to_axiom_aux string n branch i = 
      if (String.length string = n) then []
      else(
        match string.[n] with
        | '[' -> string_to_axiom_aux string (n+1) true (n+1) 
        | ']' -> 
          let list_branch = (string_to_axiom_aux (String.sub string i (n-i)) 0 false i) in
          if (List.length list_branch = 1) then (Branch (List.hd list_branch)) ::(string_to_axiom_aux string (n+1) false 0)
          else (Branch (Seq list_branch)) ::(string_to_axiom_aux string (n+1) false 0)
        | x -> 
          if (branch = true) then string_to_axiom_aux string (n+1) branch i
          else (Symb x) :: (string_to_axiom_aux string (n+1) branch i)
      )
    in
    let word_list = string_to_axiom_aux string 0 false 0 in
    if (List.length word_list = 1) then List.hd word_list
    else Seq word_list
  )
;;

let rec string_to_rules string_list c =
  match string_list with
  | [] -> Symb c
  | x :: xs -> (
    if (x.[0] = c) then string_to_axiom (String.sub x 2 (String.length x - 2))
    else string_to_rules xs c
  )
;;

let string_to_turtle_cmd string = 
  match string.[0] with
  | 'L' -> [Line (int_of_string (String.sub string 1 (String.length string - 1)))]
  | 'M' -> [Move (int_of_string (String.sub string 1 (String.length string - 1)))]
  | 'T' -> [Turn (int_of_string (String.sub string 1 (String.length string - 1)))]
  | _ -> failwith "idk what this is dude"
 ;;

let rec string_to_interp string_list c =
  match string_list with
  | [] -> []
  | x :: xs -> (
    if (x.[0] = c) then string_to_turtle_cmd (String.sub x 2 (String.length x - 2))
    else string_to_interp xs c
  )
;;

let print_str string = print_string string;;

let file_to_char_sys file = 
  let (axiomstr, rulesstr, interpstr) = read_lines file in
  {axiom = string_to_axiom axiomstr; rules = (string_to_rules rulesstr); interp = (string_to_interp interpstr)}
;;