# Fonctionnalités implémentées :

- domaines abstraits : constantes et intervalles
- analyse interprocédurale (sans récursivité)

# Détails sur l'implémentation

Le frontend est inchangé. Ont été modifiés domain/value_domain.ml, domain/domain.ml, iterator/iterator.ml, analyzer.ml et frontend/options.ml ont été modifiés. 

Sur la gestion des erreurs : j'ai fait le choix de ne pas détecter les divisions par zéro, car un intervalle associé à une variable peut "facilement" contenir zéro même si la variable ne peut jamais être nulle en pratique.
Inversement, est considérée comme insatisfaite toute assertion dont au moins une des valeurs concrètes représentées par la valeur abstraite ne la satisfait pas. Cela conduit à ce que même des assertions "raisonnables" (cf exemples) soit invalidées. 

Dans le graphe du flot de contôle, pour chaque fonction, les sommets où le "widening" est appliqué sont déterminés en faisant un parcours en profondeur à partir du sommet initial, et en choisissant les sommets où les branches du parcours se recoupent. Une amélioration possible serait de dérouler les boucles sur une ou deux itérations, ce qui permettrait d'avoir de meilleurs invariants.

Pour que l'analyse inter-prodécurale fonctionne, plutôt que de maintenir une seule table associant les sommets du CFG aux valeurs abstraites, on maintient une table différente pour chaque sommet de l'arbre d'appel du programme (par exemple, à une même fonction appelée à deux endroits différents de main() correspondra deux tables, ainsi que deux tables des assertions invalides).

# Discussion des exemples

Le script test.sh exécute l'analyseur statique sur tous les exemples dans examples/ et pour chacun d'eux, sauvegarde les résultats de l'analyse et le CFG (.dot et .pdf).
La commande `make clean-ex` efface les résultats des analyses.

## Exemple 1
Repris tel quel du projet initial. Les bonnes valeurs sont affectées aux variables par l'analyseur.

## Exemple 2
Ajout de deux assertions à l'exemple initial.
L'analyseur arrive bien à valider l'assertion `x >= -1` mais n'est pas assez précis pour valider l'assertion `i == 10`

## Exemple 3
Repris du cours.
On obtient les mêmes résultats. On n'a pas d'information sur la tête de la boucle mais à l'intérieur on sait que x est dans ]-oo;10000].
On arrive donc à valider l'assertion `x <= 10000` mais pas `x >= -10000`.

## Exemple 4
Test de l'analyse interprocédurale.
On arrive à des bornes correctes sur la valeur finale de i (+- 1064000).
Remarque : en pratique si i est non nul `foo(2*i,i) == 8`, mais en l'absence de contraintes relationnelles le programme ne parvient pas à le savoir.
Aucune information n'est donnée sur les divisions par zéro potentielles.

## Exemple 5
Test de l'analyse interprocédurale.
La valeur correcte de i (-44) est calculée.

# Bug dans le frontend :

Voir le fichier `ex_bug.c`
Il y a une confusion lors de la conversion de l'AST en CFG entre la variable globale x(1) et le paramètre x(4) de foo
Dans le CFG calculé, l'instruction portée par l'arc 13->18 devrait être `x(2) = x(1)` et non `x(2) = x(4)`
Le bug disparaît si on renomme "a" la variable globale x.
Je n'ai pas cherché à le corriger car je l'ai remarqué assez tardivement.


