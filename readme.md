# Prolog Syntax

```
cat(X), sees(X, Y), cat(Y)
```

# ML-flavored Prolog Syntax

```
cat X, sees X Y, cat Y
```

## Record arities, elide commas

```
cat/1 sees/2
cat X sees X Y cat Y
```

# Allow infix predicates

```
cat X  X sees Y  cat Y
```

# Allow unary predicates in place of variables

```
cat sees cat
```

Parse this by inserting variables: compare the next word and the "top of the parse stack".
If at least one of them is unary, generate a fresh variable and bind it to both.
We can picture this as `p q -> p X q X`.

```
cat
cat sees
  -> cat X sees X
cat X sees X cat
  -> cat X sees X Y cat Y
```
