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

### Explications de l'exemple

1. Le mot clé **Pre** désigne la précondition du programme. Il s'agit des hypothèses que l'ont fait sur les données manipulées. En d'autres termes, la précondition c'est tout ce qui doit absolument être vrai avant que le programme soit exécuté. Puisque qu'on a imposé que notre programme travail sur deux nombres `a` et `b` positifs, la précondition est donc `a >= 0 and b >= 0`. Il n'est pas nécessaire de préciser que `a` et `b` sont des entiers car **AutoWp** ne connaît pas d'autres types de données.

2. Le mot clé **Post** désigne la post-condition du programme. C'est la propriété qui doit absolument être vraie après l'éxecution. C'est cette propriété qui permet d'expliquer rigoureusement ce que le programme est censé calculer. Ici, on souhaite calculer `max(a, b)`. Par définition le `max` vérifie deux propriétés :
   + premièrement `max(a, b) >= a` et `max(a, b) >= b` 
   + deuxièmement `max(a, b)` est l'une des deux valeurs `a` ou `b`
Traduit en langage mathématique cela nous donne directement la post-condition : `(max >= a and max >= b) and (max = a or max = b)`.

Si on lance **AutoWp** sur l'exemple proposé, voici le résultat affiché :

```
Precondition  :      
   a >= 0 and b >= 0

Postcondition :
   max >= a and max >= b and (max = a or max = b)

What remains to prove after wp computations :
[1. partial correction]
   a >= 0 and b >= 0 -> (a <= b -> b >= a and b >= b and (b = a or b = b)) and (not a <= b -> a >= a and a >= b and (a = a or a = b))
```

### Explication des résultats

1. **AutoWp** commence par rappeler les spécifications fournies (**pre** et **post**).
2. Ensuite, le résultat du calcul des *Weakest Preconditions* est donné. Il s'agit d'un énoncé mathématique qui, si on parvient à le prouver, nous assure que le programme est correcte. Surtout, il ne faut pas avoir peur de cette étape, la formule paraît grosse, mais elle est en réalité très simple. Décomposons là ! D'abord on reconnaît notre précondition `a >= 0 and b >= 0 -> ...`. Ensuite plusieurs sous formules doivent être démontrées :
   1. Le premier cas du if : `a <= b -> b >= a and b >= b and (b = a or b = b)` Il s'agit de vérifier que si la première branche du `if` a été prise, alors la post-condition est vérifiée
   2. Le deuxième cas du if : `not a <= b -> a >= a and a >= b and (a = a or a = b)`, même chose mais en supposant que la condition du `if` était fausse.

En utilisant les propriétés classiques d'arithmétiques, on parvient simplement à prouver les deux cas. Ce qui achève la preuve formelle de notre fonction `max`.

### Limites de l'exemple

L'exemple choisi montre bien que prouver formellement un algorithme n'est pas une tâche simple. Toutefois, c'est une tâche qui peut être partiellement réalisée par un ordinateur. Ici, c'est `AutoWp` qui a simplifié un peu le travail en calculant pour nous un énoncé mathématique suffisant pour prouver la correction de l'algorithme. Une étape supplémentaire pourrait être d'envoyer le résultat de **AutoWp** à un programme capable de prouver automatiquement un théorème. *Spoiler Alert*, il existe des programmes capable de réaliser une telle chose. Dans un futur proche, **AutoWp** sera capable d'envoyer ses résultats à un prouveur automatique et ainsi réaliser en autonomie complète la preuve de petits programmes.

## Contribuer

La version **AutoWp** est une version très expérimentale. De nombreuses fonctionnalités sont en cours d'ajouts, notamment pour clarifier et détailler les étapes nécessaires pour prouver un algorithme. Toute contribution est donc bienvenue. N'hésitez pas à me contacter à l'adresse [arthur.correnson@ens-rennes.fr](arthur.correnson@ens-rennes.fr) en cas de question !