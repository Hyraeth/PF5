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
    

let rec interp_direct_n word rules interp k n curpos mempos anim=
  let rec interp_word_list l curpos =
    match l with
    | [] -> failwith "Empty list"
    | [x] -> interp_direct_n x rules interp k n curpos mempos anim
    | x::xs -> 
      let newpos = interp_direct_n x rules interp k n curpos mempos anim in 
        interp_word_list xs newpos 
  in
  match word with
  | Symb s -> (  
    if (k<n) then  (*Si on n'a pas encore atteint l'itération souhaité *)
      interp_direct_n (rules s) rules interp (k+1) n curpos mempos anim 
    else 
      if (anim>0.0) then 
        (Unix.sleepf anim; Turtle.exec_cmd_list curpos (interp s) mempos)
      else (*Renvoie la position à la fin de l'exécutionde la commande *)
        Turtle.exec_cmd_list curpos (interp s) mempos 
  )
  | Seq l -> ( 
    match l with
    | [] -> failwith "Empty sequence"
    | [x] -> interp_direct_n x rules interp k n curpos mempos anim
    | x::xl -> 
      let newpos = interp_direct_n x rules interp k n curpos mempos anim in 
        interp_word_list xl newpos 
  )
  | Branch b -> (*Si on doit gérer un branchement *)
    let newpos = interp_direct_n b rules interp k n curpos (curpos::mempos) anim in 
      exec_cmd newpos Restore (curpos::mempos)
;;

(**interprete directement un system à l'itération n depuis une certaine position et une vitesse d'animation*)
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
    else (*On détermine les bornes inf gauche, sup droit de la fenêtre et la position à la fin de la commande *)
      Turtle.find_size_pos curpos (interp s) (res) mempos
  )
  | Seq l -> (
    match l with
    | [] -> res
    | [x] -> find_size_n x rules interp k n curpos mempos res
    | x::xl -> (
      (*On récupere les bornes à la fin de l'execution des commandes et on élargie les bornes *)
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

(**détermine la taille de la fenêtre *)
let find_window_size_n curpos sys n= 
  find_size_n sys.axiom sys.rules sys.interp 0 n curpos [] (curpos.x,curpos.x,curpos.y,curpos.y, curpos)
;;

(**Lis un fichier et renvoie un triplet (string axiom, list de string de rules, list de string d'interp) *)
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

(**Transforme un string en char word *)
let string_to_char_word string =
  (*On détermine où se trouve la fermeture pour un branchement donné *)
  let rec find_nex_pos string n acc =
    let char = try string.[n] with Invalid_argument x -> raise Not_found in
    match char with
    | ']' -> if acc = 0 then n else find_nex_pos string (n+1) (acc-1)
    | '[' -> find_nex_pos string (n+1) (acc+1)
    | x -> find_nex_pos string (n+1) acc
  in
  if (String.length string = 1) then Symb string.[0]
  else (
    let rec string_to_axiom_aux string n = 
      if (String.length string = n) then []
      else(
        match string.[n] with
        | '[' -> 
        let lastpos = try find_nex_pos string (n+1) 0 with Not_found -> failwith("Branchement mal fermé") in
        let list = string_to_axiom_aux (String.sub string (n+1) (lastpos - n -1)) 0 in
        if(List.length list = 1) then 
          (Branch (List.hd list))::(string_to_axiom_aux string (lastpos + 1))
        else
          (Branch (Seq list))::(string_to_axiom_aux string (lastpos + 1))
        | x -> 
            (Symb x)::(string_to_axiom_aux string (n+1))
      )
    in
    let word_list = string_to_axiom_aux string 0 in
    if (List.length word_list = 1) then List.hd word_list
    else Seq word_list
  )
;;

(**regarde si un char c est dans la liste de rules et renvoie un char word associe à ce char *)
let rec string_to_rules string_list c =
  match string_list with
  | [] -> Symb c
  | x :: xs -> (
    if (x.[0] = c) then string_to_char_word (String.sub x 2 (String.length x - 2))
    else string_to_rules xs c
  )
;;

(**Renvoie la turtle commande associée à un string *)
let string_to_turtle_cmd string = 
  match string.[0] with
  | 'L' -> [Line (int_of_string (String.sub string 1 (String.length string - 1)))]
  | 'M' -> [Move (int_of_string (String.sub string 1 (String.length string - 1)))]
  | 'T' -> [Turn (int_of_string (String.sub string 1 (String.length string - 1)))]
  | _ -> failwith "idk what this is dude"
 ;;

(**Renvoie la turtle commande associée à un char *)
let rec string_to_interp string_list c =
  match string_list with
  | [] -> []
  | x :: xs -> (
    if (x.[0] = c) then string_to_turtle_cmd (String.sub x 2 (String.length x - 2))
    else string_to_interp xs c
  )
;;

(**Renvoie le char system associé à un fichier *)
let file_to_char_sys file = 
  let (axiomstr, rulesstr, interpstr) = read_lines file in
  {axiom = string_to_char_word axiomstr; rules = (string_to_rules rulesstr); interp = (string_to_interp interpstr)}
;;