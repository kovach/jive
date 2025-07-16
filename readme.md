# Intro
The project implements a parsing algorithm for a neat query syntax.
This readme describes how to arrive at it by taking a series of small steps starting from traditional logic program syntax.

`main2.hs` has the reference implementation.

# Prolog Syntax
I want to know about all the cats that are on shelves:

```
cat(X), on(X, Y), shelf(Y)
```

# ML-flavored Prolog Syntax

```
cat X, on X Y, shelf Y
```

## Record arities, elide commas

```
# cat/1 shelf/1 on/2
cat X on X Y shelf Y
```

You could call a set of predicates and their arities a *schema*.

# Allow unary predicates in place of variables

```
on cat shelf
```

How: Distinguish variables and predicates using case.
Scan left to right and insert variables:

- When you encounter a predicate with arity > 1, put it on the stack [**push step**].
- When the next word is unary, generate a fresh variable and bind it to that word and the top element of the stack [**join step**].
- When the stack predicate becomes nullary, pop it [**reduction step**].

The main step is summarized by a rule for rewriting the stack and the next word: `p q -> p X q X`.

```
on
on cat
  -> cat X on X
cat X on X shelf
  -> cat X shelf Y on X Y
```

The final result is a series of *atoms*: fully applied predicates.

# Allow argument re-ordering

```
cat X X on Y shelf Y
```

or

```
cat on shelf
```

How: when you encounter a new word of arity > 1, if the top stack element is unary,
apply [**join step**] instead of [**push step**].

```
# telescope/1
# with/2
# sees/3

cat sees cat with telescope
  cat
  cat sees
    -> cat X sees X
  cat X sees X cat
    -> cat X cat Y sees X Y
  cat X cat Y sees X Y with
    -> cat X cat Y sees X Y Z with Z
  cat X cat Y sees X Y Z with Z telescope
    -> cat X cat Y sees X Y Z with Z W telescope W
cat X cat Y sees X Y Z with Z W telescope W
```

Note that a pair of unary predicates (e.g. `cat telescope`, `telescope cat`) both unwind into the same atoms, just in a different order, so they have the same meaning under this algorithm.
This property is necessary for the validity of re-ordering.
However, `on X Y` and `on Y X` are still different queries.
The algorithm has an issue: `on X Y`, `X on Y` denote the same thing, but `X Y on` is instead equivalent to `on Y X`.
It also allows for various strange expressions like `on on cat cat = on cat Y, on Y cat` ("a cat is on something that is on a cat").
These two problem sentences share the property that they require more than one entry of stack space for parsing; single-stack sentences are more local.
