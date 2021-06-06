# Fonctionnalités implémentées :

- domaines abstraits : constantes et intervalles
- analyse interprocédurale (sans récursivité)

# Détails de l'implémentation



# Bug dans le frontend :

Voir le fichier `ex_bug.c`
Il y a une confusion lors de la conversion de l'AST en CFG entre la variable globale x(1) et le paramètre x(4) de foo
Dans le CFG calculé, l'instruction portée par l'arc 13->18 devrait être `x(2) = x(1)` et non `x(2) = x(4)`
Le bug disparaît si on renomme "a" la variable globale x.
Je n'ai pas cherché à le corriger car je l'ai remarqué assez tardivement
