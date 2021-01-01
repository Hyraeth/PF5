
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
  create_window 500 500;
  let snow_cmdl = interp_sys_n snow 2 in
  draw_sys {x=100.; y=100.; a=0} snow_cmdl;
  synchronize();
  close_after_event ()
  

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
