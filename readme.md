# Intro
The project implements a parsing algorithm for a neat query syntax.
This readme describes how to arrive at it by taking a series of small steps starting from traditional logic program syntax.

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

# Allow infix predicates

```
cat X X on Y shelf Y
```

How: distinguish predicates from arguments using case.

# Allow unary predicates in place of variables

```
cat on shelf
```

How: parse this by scanning left to right and inserting variables.

- Compare the next word and the "top of the parse stack".
- If at least one of them is unary, generate a fresh variable and bind it to both.

We can picture this as `p q -> p X q X`.

```
cat
cat on
  -> cat X on X
cat X on X shelf
  -> cat X on X Y shelf Y
```

```
telescope/1
with/2
sees/3

cat sees cat with telescope
  cat
  cat sees
    cat X sees X
  cat X sees X cat
    cat X sees X Y cat Y
  cat X sees X Y cat Y with
    cat X sees X Y Z cat Y with Z
  cat X sees X Y Z cat Y with Z telescope
    cat X sees X Y Z cat Y with Z W telescope W
cat X sees X Y Z cat Y with Z W telescope W
```
