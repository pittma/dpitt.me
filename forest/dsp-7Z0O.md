---
title: Internal ∞-Categorical Models of Dependent Type Theory
subtitle: Towards 2LTT eating HoTT
published: 2020-09-14
tagged: true
tags: math, category theory, type theory, paper
math: true
---

# Introduction

- Semicategory :: All the category things mod identities (like a
                  semigroup).

**Other models of type theory:**
- Setoid (Palmgren 2019)
- Parametric (Bernardy 2015)
- Groupoids (Hofmann & Striecher 1996)
  - Sozeau and Tabareau implementation (2014)

## Initiality

- Syntactic $$\equiv$$ term
- CwF refresher: A category $$\mathcal{C}$$ with a presheaf of families
  where the presheaf is split into two “special” functors: $$Ty$$ and
  $$Tm$$. $$\mathcal{C}$$ is the category of contexts and substitutions
  where the terminal object is the empty context.
  - Terminal object :: That's the thing that everyone can reach,
       uniquely (up to blah blah blah).
    - The singleton set in $$\mathbf{Set}$$.

  - **NOTE**: What's it mean in terms of relationships between a
    category with initial and terminal objects and order theory?
- A Generalized Algebraic Theory (GAT) is apparently where /sorts/
  come from. The PTS work is creating a GAT out of type theories?
- What it means to have an “initial” model.
  
  > We have a morphism of models from the initial model to any other
  > model, and in particular to the standard model. This means that a
  > universe in the host theory is a model of type theory itself.
  
### The initial model is syntax
They have this rule:


$$$$
(A B : Ty\ \Gamma) \rightarrow (A = B) + \neg(A = B)
$$$$

Which is to say, it's decidable because it's either true or its
not. Since this decidable equality is in the host theory, it's
intensional! It's a part of the metatheory, and is therefore
syntactic.

We should, when doing proofs in type theory, think of intensional
equality as /syntactic/.

**NOTE**: This really cements the intuition from set theory of what
exactly extensional equality means in a type theory. It also gives way
to the obvious extensional meaning of funext.

## Uniqueness of Identity Proofs (UIP)
- HoTT rejects UIP (think about it, because univalence turns
  not-so-unique equalities into equivalences).
