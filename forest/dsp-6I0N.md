---
title: W-Types, and how?
published: 2022-09-20
tagged: true
tags: math, category theory
math: true
---

<!--
```
{-# OPTIONS --without-K #-}

module W where

open import Data.Unit
open import Data.Empty
```
-->

This is lifted from McBride's [_W-types: good news and bad
news_](https://mazzo.li/epilogue/index.html%3Fp=324.html). See the
link in the footnote to follow along. Probably as I make progress I'll
update here. But also maybe not.

```agda
data Bool : Set where
  true : Bool
  false : Bool

data W (S : Set) (P : S → Set) : Set where
  _◂_ : (s : S) (f : P s → W S P) → W S P

natf : Bool → Set
natf true = ⊤
natf false = ⊥

ℕ : Set
ℕ = W Bool natf

zero : ℕ
zero = false ◂ λ () 

succ : ℕ → ℕ
succ n = true ◂ (λ _ → n)

```

Turns out this probably isn't possible. I didn't take it as far as
McBride did, but I think his conclusion is relevant here:

> W-types are a powerful conceptual tool, but they’re no basis for an
  implementation of recursive datatypes in decidable type
  theories. **To encode a first-order structure by a function space is
  to throw away canonical choice of representation.** When you care
  about what things are (because they show up in types), not just what
  they do (when you run them), that price isn’t worth paying.

(emphasis mine)

```agda
_+_ : ℕ → ℕ → ℕ
(false ◂ _) + m = m
(t ◂ f) + m = succ (n + m)
  where
    n = f {!!}
```
