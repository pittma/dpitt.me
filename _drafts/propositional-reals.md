---
layout: post
title: "The Real Numbers, Propositionally"
---

## 1. Introduction

If some framework for reasoning about mathematical objects posits that it
exists as a _foundation_ of mathematics, what good would it be if something as
critical as the real numbers couldn't be reasoned about, much less even
constructed, in that framework? When Homotopy Type Theory states that it is
such a framework, we must measure such a postulation against the same
requirements for any other foundations of mathematics. For HoTT to be
"foundational" we're going to need to be able to reason about the real numbers
_through_ it.

The goal of this document is to first build up an intuition for the requisite
peices of understanding the reals in HoTT. We begin by giving a whirlwind
introduction to Homotopy Type Theory and the mathematical toolbox it provides
us. Next, we'll explore the mathematical structure we need to construct the
real numbers, namely Cauchy Sequences. And finally, I'll elucidate how we can
use the tools provided to us by HoTT to construct, and then reason about the
reals.

## 2. Homotopy Type Theory

Homotopy Type Theory is many things, and its existence has many elegant
applications, but for the purpose at hand, I'm going to cover just the parts
which we need to begin to think about the real numbers. To begin, why
_Homotopy_ Type Theory?

### 2.1 Homotopies

In topology, a _homotopy_ is a mathematical object which _bends_ one function
into another. More formally:

Given two functions which map between spaces $X$ and $Y$:

$$
f,\ g : X \rightarrow Y
$$

A homotopy $H$ is a function:

$$
H : [0, 1] \times X \rightarrow Y
$$

(Where $[0, 1]$ denotes the _real interval_ — the continuous and transfinite
sequence of the real numbers between $0$ and $1$.)

Such that

$$
H(0, X) = f
$$

And

$$
H(1, X) = g
$$

The intuition here is that, in a sense, our homotopy $H$ is a functions
_between functions_.

We can take this notion a step further too, and use a homotopy as an
equivalence between two topological spaces:

Given two functions:

$$
f : X \rightarrow Y \\
g : Y \rightarrow X
$$

We can use our homotopy to say, "if the composition of $f$ and $g$ is homotopic
to the function $Id_x$, then the spaces $X$ and $Y$ are homotopy equivalent."

$$
H(0, X) = Id_x \\
H(1, X) = g \circ f
$$

In topology, it is reasonable to say that a homotopy equivalence is an
_isomorphism_. That is, an identity preserving map between our two spaces $X$
and $Y$.

There are two take aways that I want to make sure we don't abscond from this
section without taking them with us.

1. __A homotopy can, more generally, be thought of as a function _between_
   functions.__
2. __A homotopy can been seen as an isomorphism in the mathematical field of
   topology, because it is _identity preserving_.__

Now, this notion of functions, their inverses, and functions between functions
can be generalized in category theory in an object called a _groupoid_.

### 2.2 Groupoids

In the land of category theory, a groupoid is an object in which the usual
axioms hold — namely composition, the associativity of composition, an identity
morphism, & c. However, in a groupoid, all morphisms also have an inverse:

$$
\begin{xy}
\xymatrix{
  A \ar[r]_f & B \ar@{.>}@/_1pc/[l]_{f^{-1}} \ar[d]_g \\
  & C \ar@{.>}@/_1pc/[u]_{g^{-1}}
}
\end{xy}
$$

In the same way we think of homotopy equivalence as an isomorphism in topology,
we can also think of these morphisms and their inverses as isomorphisms.
Given the diagram above, the composition:

$$
f^{-1} \circ f \simeq Id_A
$$

is identity preserving. Through this lens, we can start to think as a groupoid
as a category whose morphisms are equivalences.

### 2.3 $\infty$-Groupoids

Like our homotopy was a function between functions, an $\infty$-groupoid is
governed by the precept that not only can we have morphisms between objects,
but also we can have morphisms between morphisms, and morphisms between
morphisms between morphisms, ad infinitum. However, another way to think of
this, given what we've stated before about the composition of a morphism and
its inverse as an isomorphism, this would correspond to stating isomorphisms of
isomorphisms.

For example:

If we have the following diagram:

$$
\begin{xy}
\xymatrix{
  A \ar@/^/[r]^f \ar@/_/[r]_g & B \ar@{.>}@/_2pc/[l]_{f^{-1}} \ar@{.>}@/^2pc/[l]^{g^{-1}}
}
\end{xy}
$$

Another way we could state this is like so:

$$
p : A \simeq B \\
p = f^{-1} \circ f \\ \\
$$

and

$$
q : A \simeq B \\
q = g^{-1} \circ g
$$

Now, with our higher-groupoid structure, we can say things like:

$$
p \simeq q
$$

## 3 The Univalence Axiom

Secondarily to this notion of morphisms of morphisms, concomitant to HoTT's
conception came the idea of univalence. The univalence axiom states the
following:

$$
(A \simeq B) = (A = B)
$$

Before we can cover the implications of such an axiom, we need first to cover
a few prerequisites.

### 3.1 Intensional vs Extensional Type Theories

In an _intensional_ type theory, the notion of equality is definitional, not
one that can be expressed as a proposition. This concept of intensionality is
how Per Martin-Löf originally conceived of type theory. In an
intensional type theory, for two objects to be equivalent, they must,
by definition, be the same thing. While Martin-Löf stated that intensional
equality was an equality of meaning, i.e. synonymy, its implications are such
that two objects must be syntactically equivalent.

However, in an _extensional_ type theory, equivalences can be expressed as
simply another type—a proposition of equality.

This would allow one to, say, state that in our type of topological spaces, a
homotopy equivalence is an equivalence for this type, and to prove two spaces'
equivalence, we merely need to define an inhabitant of that type.

This idea of being able to define our own equivalences can plausibly be seen as
a weakening of equality, as we're now providing a way in which to arbitrarily
state equivalences for our types.

### 3.2 Univalence as an Extensional to Intensional Bridge

Lets look again at the definition of the Univalence Axiom:

$$
(A \simeq B) = (A = B)
$$

What this axiom is stating is this: If two objects are isomorphic, then they
are equivalent.

This axiom elides the need for an explicitly extensional type theory, as those
propositional isomorphisms we've defined for our types like the type of
topological spaces, are in fact, universally, equivalences. Univalence
_universalizes_ domain specific isomorphisms stating that they are all
equivalent to an equality.

## 4 Higher Inductive Types

{% include math.html %}
