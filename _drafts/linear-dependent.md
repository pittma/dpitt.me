---
layout: post
title: "Linear and Dependent Types, Inextricably Interwined"
tags:
  - type theory
  - math
---

## Introduction

### Intuition

In the beginning, there were intuitionistic types. And they were good. Below you will find a rule for function application for an intuitionistically typed system.

$$
\cfrac
  {\Gamma \vdash x : A,\ f : A \rightarrow B}
  {\Gamma \vdash f\ x : B }
  {APP}
$$

How you could read this: "Given our context, we know that $x$ has type $A$, and $f$ has type $A \rightarrow B$. From that, we can deduce that, if we apply $f$ to $x$, we'll get something of type $B$."

This precept sets the rails for the rest of intuitionistic type theory. Here's a few things we need to note here about the above typing rule.

__1. These types have an implicit universal quantification.__

This is denoted with $\forall$. If we were to write the above types in Haskell, we could write them like this:

```haskell
x :: forall a. a
f :: forall a b. a -> b
```

__2. Universal quantification separates types from terms in a running program.__

When a type is universally quantified, it has no dependency or prerequisite. When thinking about types as logical propositions, we consider a definition that proposition's proof. A type which has a proof is _inhabited_. To _inhabit_ a plain ol' intuitionistic type, we needn't any other context; our proof will have no dependencies.

### Walk the Line

Linear types are all about _consumption_. When we introduce a variable, we must consume it, and may do that exactly and _only_ once. Consider the following Haskell programs:

```haskell
double :: Int -> Int
double x = x + x
```
and
```haskell
projl :: (a, b) -> a
projl (x, y) = x
```

Neither of these programs would be valid in a linear type system. The first uses `x` twice, while the second absconds without so much as considering `y`. If we're being fair and honest with ourselves, we'd know that these examples are reductive and not proper simulacrums for a program. There's another way to consider what, semantically, constitutes a linear type, and it's closer to introduce and remove, or _open_ and _close_, like a file handle.

```haskell
writeToFile :: Path -> String -> IO ()
writeToFile path msg = open path >>= write msg
```

<!-- TODO here:
  * verify my haskell is any good
  * We still need find a place for the APP ruke for linear types
  * If there's time / room, cover Krishnaswami's $F \dashv G$ adjunction for lifting intuitioninstic things into linear types land, not unlike our haskell example.
  * No matter what, we need to add references here for sessions as LT.
-->

In the preceding program, we forgot to close the file we wrote `msg` to, preventing this is exactly the type of thing that linear types can help with. There's been work to tie the more general notion of session types to linear types, and we could use those here like so:
```haskell
data Open
data Closed

data LinearFile s file

type OpenFile file = LinearFile Open file
type ClosedFiled file = LinearFile Closed file

-- Now we could write new versions of our open and write functions which use our `LinearFile` type
-- synonyms.
openLinear :: Path -> IO (OpenFile)
openLinear path = OpenFile (open path)

-- And we can say, explicitly, "I want this file to be closed when I exit".
main :: IO (ClosedFile)
main = do
  path <- getPath
  openFile <- openLinear path -- <-+
  writeLinear openFile        --   | this relationship, the open / close, is "linear".
  closeLinear openFile        -- <-+
```

<!-- TODO here:
  * Dependent types write up
    Some thoughts:
      "families", notation, and the app rule. Remember that I covered "no dependencies" and the separation of types and terms in the intuitionistic section.
-->

{% include math.html %}
