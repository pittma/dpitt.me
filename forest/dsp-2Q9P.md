---
title: Coherence
published: 2020-11-11
tagged: true
tags: math, category theory
math: true
---

# Origin

HoTT Zulip: [terminology
discussion](https://hott.zulipchat.com/#narrow/stream/228519-general/topic/terminology.3A.20.22wild.22.20vs.20.22incoherent.22.20vs.20.22H.22]).

# References

## Wikipedia

> [A] coherence condition is a collection of conditions requiring that
> various compositions of elementary morphisms are equal.

Wikipedia has a hilariously bad example that makes no sense, but then
they give a diagram that… actually does? Recapitulating it:

$$$$
\xymatrix{
    ((A \otimes B) \otimes C) \otimes D \ar[r] \ar[d] &
    (A \otimes (B \otimes C)) \otimes D \ar[r] &
    A \otimes ((B \otimes C) \otimes D) \ar[d] \\
    (A \otimes B) \otimes (C \otimes D) \ar[rr] &&
    A \otimes (B \otimes (C \otimes D))
}
$$$$

Where the point is that if you have this diagram, then you /have
coherence/—you don't need to prove it $$\forall c \in C$$.

## nLab

> While associativity and uniticity of composition of k-morphisms
> holds only up to choices of higher morphisms, coherence is the
> demand that the collection of these choices forms a contractible
> \infty-groupoid.

jfc

A little more than half-way down the page and we're talking about
Trimble $$\omega$$ categories and yeah, I'm out.

# Intuition

## Induction

What is coherence's relationship to induction? Going back to diagram
given in the Wikipedia section of this document and I'm thinking…
could there be a way to express that diagram inductively? A type with
four parameters where each constructor is a different associative
configuration and then a proof that $$\forall (m\ n : \text{MCoherence})
\rightarrow m \equiv n$$?
