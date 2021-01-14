# Programmation fonctionnel : Lindenmayer system

## Identifiants

- KHIEU William 71806172 [@Hyraeth](https://gaufre.informatique.univ-paris-diderot.fr/Hyraeth)
- Didelot Yannis 71801118 [ydidel28](https://gaufre.informatique.univ-paris-diderot.fr/ydidel28)
  
## Fonctionnalités

Le sujet minimal est totalement fait, y compris ce qui est mieux. Il n'y a normalement pas de problèmes.

## Compilation et exécution

Pour compiler le projet, se placer dans le dossier projet du dépôit puis faire

```bash
make
```

Pour exécuter le programme, se placer dans le dossier projet et faire

```bash
./run -help
```

On a utilisé la bibliothèque Unix pour fair les animations (Unix.sleepf) : https://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html.
La gestion des arguments de la ligne de commande ne se fait pas entièrement en fonctionnel dû à l'usage de la bibliothèque Args.

## Découpage modulaire

Dans le fichier systems.ml se trouve les fonctions pour interpréter à la volée un L-system, déterminer la taille de la fenêtre et parser un fichier en char system.
Dans le fichier turtle.ml se trouve les fonctions pour tracer un liste de commande donnée.

## Organisation du travail

La majorité du travail s'est fait le jour du premier commit qui était juste après la deadline pour le projet de programmation C.
Le reste : Parser pour lire les fichiers .sys et la gestion des arguments de la ligne de commande s'est fait pendant le temps libre entre les examens.
Le projet était pas très dur et très largement faisable sur une journée.

## Misc

S'il avait plus de temps j'aurais voulu essayer de faire les fractals en 3D avec projection mais il faudra que je remanie tout le code.
Le format des fichiers .sys a été changé pour faciliter la lecture : il faut finir le fichier par un retour à la ligne.
Ne pas dépasser la limite de 80 lines était difficile.
