# Travail pratique 1 - Graphes séries-parallèles

## Description

Ceci est le premier Travail pratique du cours INF6120-au2019.
Dans ce premier travail je devais compléter l'implémentation de modules Haskell permettant de manipuler des graphes séries-parallèles. En particulier, je devais résoudre deux problèmes sur ces graph est :

- Trouver un 3-coloriage d'un graphe série-parallèle
- Trouver une couverture par sommets (en anglais, vertex cover) de cardinalité minimale


## Auteur

Yacin

## Fonctionnement

Pour faire fonctionner le projet, il faut tout d'abord compiler le programme.

Pour cela, il faut taper au terminal (dans le répertoire du projet) la commande `make`.

Ensuite, pour pouvoir exécuter le projet, il suffit de taper la commande `ghci VcSpGraoh.hs`.
Ce module inclut tous les autres modules du programme, donc il n'est pas nécessaire de les charger.

Pour générer la documentation, on utilise `make doc`.

Finalement, pour lancer les tests automatiques, on utilise `make test`.

**Note : Certaines dépendances doivent être installées**

***


## Contenu du projet

Ce projet contient les fichiers suivants :
- **InfInt** (Module contenant des fonctionnalités pour gérer des valeurs infinies)
- **Graph** (Module contenant de multiples fonctions sur les graph SP)
- **SpTree** (Module contenant de multiples fonctions sur les arbres SP)
- **ColorSpGraph** (Mdule permettant de colorier un Graph SP)
- **VcSpGraph** (incomplet)
- **Makefile** (permet d'automatiser des commandes Shell )
- **.gitignore** (permet d'ignorer les fichiers inutiles)
- **.gitlab-ci.yml** (permet de lancer une suite de tests de façon automatique)


## Dépendances


- [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
- [ghci](https://wiki.haskell.org/GHC/GHCi)
- [Doctest](https://github.com/sol/doctest#readme)
- [graphviz](https://www.graphviz.org/)


## Références

Voici les références qui m'ont aidé compléter le projet

- [Stackoverflow](https://stackoverflow.com)
- [Wikipedia](https://en.wikipedia.org/wiki/Series-parallel_graph)


## Statut

VcSpGraph est actuellement incomplet dû a un manque de temps.





