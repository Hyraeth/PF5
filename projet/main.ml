
open Lsystems (* Librairie regroupant le reste du code. Cf. fichier dune *)
open Systems (* Par exemple *)
open Turtle
open Graphics
open Examples

(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)

let create_window w h =
  open_graph (" " ^ string_of_int w ^ "x" ^ string_of_int h);
  auto_synchronize false
;;

let close_after_event () =
  ignore (wait_next_event [Button_down ; Key_pressed])
;;

let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"

let action_what () = Printf.printf "%s\n" usage; exit 0

let cmdline_options = [
("--what" , Arg.Unit action_what, "description");
]

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s)

let main () = 
  let cmdl = interp_sys_n snow 6 in
  let (xmax, xmin, ymax, ymin) = find_window_size {x=0.0; y=0.0; a=90; s=1.0} cmdl in
  let height = int_of_float (abs_float(ymax-.ymin)) in
  let width = int_of_float (abs_float(xmax-.xmin)) in
  let scale = if (max height width) > 1000 then (1000.0)/.(float_of_int (max height width)) else 1.0 in
  create_window (int_of_float (scale *. (float_of_int width) +. 100.0)) (int_of_float (scale *. (float_of_int height) +. 100.0));
  draw_sys {x=scale*.(abs_float xmin)+.50.0; y=scale*.(abs_float ymin)+.50.0; a=90; s=scale} cmdl;
  synchronize();
  close_after_event ()
  

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
