# Autowp

**AutoWP** est un petit programme pour calculer automatiquement les [*Weakest Preconditions*](https://www.wikiwand.com/en/Predicate_transformer_semantics) de Dijkstra sur des petits programmes impératifs. L'objectif de cet outil est de proposer un guide d'introduction à la notion de preuve formelle à destination des étudiants de licence. AutoWp n'est absolument pas destiné à fonctionner sur des exemples compliqués, mais il suffira largement pour montrer l'intérêt du calcul des WP sur des exemples pédagogiques.

## Programmes, spécifications, exemple détaillé

Pour faire la preuve (mathématique) de correction d'un programme, deux ingrédients sont absolument nécessaires. Tout d'abord une [spécification](https://www.wikiwand.com/fr/M%C3%A9thode_formelle_(informatique)#/Sp%C3%A9cification) du programme, c'est à dire la description précise de ce qu'est censé faire le programme du début à la fin. Il faut également une description précise du programme en lui même. **AutoWp** est fournis avec un langage de programmation qui permet d'exprimer à la fois un **programme** et sa **specification**.

Voici par exemple la specification et la preuve d'un programme qui calcul le maximum de deux entiers positifs `a` et `b`:

```
pre: a >= 0 and b >= 0
post: (max >= a and max >= b) and (max = a or max = b)

if (a <= b) {
  m = b;
} else {
  m = a;
}
```
**Explications**

1. Le mot clé **Pre** désigne la précondition du programme. Il s'agit des hypothèses que l'ont fait sur les données manipulées. En d'autres termes, la précondition c'est tout ce qui doit absolument être vrai avant que le programme soit exécuté. Puisque qu'on a imposé que notre programme travail sur deux nombres `a` et `b` positifs, la précondition est donc `a >= 0 and b >= 0`. Il n'est pas nécessaire de préciser que `a` et `b` sont des entiers car **AutoWp** ne connaît pas d'autres types de données. Quand on ne fait aucune hypothèse sur les données manipulées, on peut remplacer la précondition par `True` (aussi parle alors de la **précondition triviale**) pour qu'elle soit toujours vraie.

2. Le mot clé **Post** désigne la post-condition du programme. C'est la propriété qui doit absolument être vraie après l'éxecution. C'est cette propriété qui permet d'expliquer rigoureusement ce que le programme est censé calculer. Ici, on veut calculer `max(a, b)`. Par définition le `max` vérifie deux propriétés :
   + premièrement `max(a, b) >= a` et `max(a, b) >= b` 
   + deuxièmement `max(a, b)` est l'une des deux valeurs `a` ou `b`
Traduit en langage mathématique celà nous donne directement la post-condition : `(result >= a and result >= b) and (result = a or result = b)`.

1. Si on lance **AutoWp** sur cet exemple, voici le résultat proposé :