---
title: A test note, which does all the things
subtitle: Should be discarded before publishing the forest.
related: dsp-0007 dsp-0018
origin: '<a href="https://pittma.substack.com/p/how-to-pitch-a-tent">substack</a>'
tagged: true
tags: self, god
math: true
garage: closed
published: 2024-05-27
---

# A H1 title

:::{.poem}
When all of a sudden
I am unaware of these afflictions, eclipsed by glory
And I realize just how beautiful You are
And how great your affections are for me
:::

<span class="para-begin">A few years ago</span>, as my first foray
into philosophy as a Real Adult, I attempt to power my way through
Deleuze and Guattari's _AntiOedipus: Capitalism and Schizophrenia_. It
didn't go particularly well. I think I understood that there was
something revolutionary about it, about escaping oppressive
hierarchies imposed on us by the Oedipal Triangle, but the lyrical
language can be, at times, nearly impossible to follow. I gave up
about half-way through.

1. This is a
1. Numbered
1. list.

* this is an unordered list I guess if it's long enough to become a
  paragraph it's different? I don't get it.
* unordered
* list.

$transclude("dsp-0005")$

## Homotopy Type Theory

Homotopy Type Theory is many things, and its existence has many
elegant applications, but for the purpose at hand, I'm going to cover
just the parts which we need to begin to think about the real
numbers. To begin, why _Homotopy_ Type Theory?

### 2.1 Homotopies

In topology, a _homotopy_ is a mathematical object which _bends_ one
function into another. More formally:

Given two functions which map between spaces $$X$$ and $$Y$$:

$$$$
f,\ g : X \rightarrow Y
$$$$

A homotopy $$H$$ is a function[^ri]:

[^ri]: Where $$[0, 1]$$ denotes the _real interval_, the continuous
      and transfinite sequence of the real numbers between $$0$$ and
      $$1$$.


$$$$
H : [0, 1] \times X \rightarrow Y
$$$$


Such that

$$$$
H(0, X) = f
$$$$

And

$$$$
H(1, X) = g
$$$$

$$$$
\xymatrix{
  A \ar[r]_f & B \ar@{.>}@/_1pc/[l]_{f^{-1}} \ar[d]_g \\
  & C \ar@{.>}@/_1pc/[u]_{g^{-1}}
}
$$$$

:::{.multiline-quote}
[A] functor $$F: C \to D$$ between categories is called a fibration if
for every morphism $$f: X \to Y$$ in the codomain category D and for
every object $$X$$ in the domain category $$C$$, there exists:

1. An object $$\text{fib}(f)$$ in $$C$$, called the "fiber" or "preimage"
   of $$f$$ under $$F$$, and
1. A morphism $$p(f): \text{fib}(f) \to X$$ in $$C$$, called the 
   "lifting" or "pullback" of $$f$$ along $$F$$, such that $$F(p(f)) = f$$.
:::

:::{.quote-ref}
This is the source for the quote.
:::


<p class="pull">What is a computer besides a territorializer of
electrons, anyway?</p>

```haskell
inv :: Int -> Int -> Int
inv a m = f 0 m 1 a
  where
  f t r newt newr
    | newr == 0 = abs t
    | otherwise =
      let q = r `div` newr
      in f newt newr (t - (q * newt)) (r - (q * newr))
```

> The truth is, no matter how I try and metaphorize it, Chris’
> experience and mine are the self-same. The reason watching a
> particularly chaotic set—even when displaced by the abstraction of a
> screen—can move me to tears is because it is literally scratching my
> soul. I lost God at church, but I found him in hardcore.

I'll add that link by typing <kbd>ctrl</kbd> +
<kbd>c</kbd><kbd>ctrl</kbd> + <kbd>l</kbd> (`C-c C-l`) then start to
type `file` which gives me this menu (I'm using
[Helm](https://emacs-helm.github.io/helm/)):

:::{.insight}
Here's an insight!
:::

What do I put inbetween here?

:::{.note}
And this is a note. Not sure what it's for.
:::
