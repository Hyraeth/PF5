open Lsystems (* Librairie regroupant le reste du code. Cf. fichier dune *)
open Systems (* Par exemple *)
open Turtle
open Graphics
open Examples
open Unix

(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)

let anim_enable = ref true
let anim = ref 0
let file = ref "examples/br1.sys"
let iter = ref 0
let maxsize = ref 1000
let angle = ref 0

let create_window w h =
  open_graph (" " ^ string_of_int w ^ "x" ^ string_of_int h);
  auto_synchronize (!anim_enable)
;;

let close_after_event () =
  ignore (wait_next_event [Button_down ; Key_pressed])
;;

let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"

let set_sys sys = file := sys
let set_iter n = if (n>=0) then iter := n else failwith ("Nombre d'iteration negatif")
let set_maxsize n = if (n>=0) then  maxsize := n else failwith ("Taille negative")
let set_angle n = angle := n
let set_anim_speed n = if (n>=0) then anim := (min n 10) else anim_enable := false

let cmdline_options = [
("--sys" , Arg.String set_sys, "path to the L-system to display");
("--n" , Arg.Int set_iter, "Number of iteration");
("--size" , Arg.Int set_maxsize, "Maximum size for the edges of the window");
("--angle" , Arg.Int set_angle, "Angle for drawing the L-system");
("--animation" , Arg.Int set_anim_speed, "Animation speed from 1 to 10, 0 for no lag when drawing, -1 for no animations");
]

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s)

let main () = 
  Arg.parse cmdline_options extra_arg_action usage;
  let sys = file_to_char_sys (!file) in
  let iter_rank = (!iter) in
  let maxsize = float_of_int (!maxsize) in
  let startpos = {x=0.0; y=0.0; a=(!angle); s=1.0} in
  let (xmax, xmin, ymax, ymin, nextpos) = find_window_size_n startpos sys iter_rank in
  let height = (abs_float(ymax-.ymin)) in
  let width = (abs_float(xmax-.xmin)) in
  let scale = if (max height width) > maxsize then (maxsize)/.(max height width) else 1.0 in
  let new_height = int_of_float (scale *. height +. 100.0) in
  let new_width = int_of_float (scale *.  width +. 100.0) in
  let new_startpos = {x=scale*.(abs_float xmin)+.50.0; y=scale*.(abs_float ymin)+.50.0; a=startpos.a; s=scale} in
  create_window (new_width) (new_height);
  Unix.sleepf 0.1;
  interp_direct_sys_n sys iter_rank new_startpos (!anim);
  synchronize();
  close_after_event ()
  

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
