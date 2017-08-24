---
layout: post
title: "Homotopy Type Theory Desiderata"
tags:
  - math
  - HoTT
---

This is a thing I do. I like to learn things by finding the thing I want to learn, and then work my way down as I encounter unknowns, exposing layers like an epistemological onion — a truth vegetable.

The current outermost layer is Homotopy Type Theory which is put forth by [the HoTT Book](https://homotopytypetheory.org/book/). Per usual, as I dig down beneath the _tunic_ I'm reminded that, for all intents and purposes, I know nothing and I spiral out of control wiki link after wiki link, my only respite a term I actually understand. It could be argued that this is called "learning", but that label is dubious.

### Homotopy

Digressions aside, Homotopy is, in my best attempt at lucidity, a continuous function that can map between two functions, say $f,g: A \rightarrow B$ mapping from the same space to another same space. If this function exists, and we'll call it $H$, then those two maps are homotopic. Formally, it looks like this:

$$\
f, g: A \rightarrow B \\
H: A \times [0, 1] \rightarrow B \\
H(x, 0) = f(x) \\
H(x, 1) = g(x)
$$

To put this in English: We've convered the first line.  That's our two maps: $f$ and $g$. The second line is illustrating the what $H$ maps: the _product_ of $X$ and the unit interval to $Y$. A product in this case means _and_. As a quick explanation of product in this context, we can refer to some Haskell:

{% highlight haskell %}

-- This is a sum type, i.e. an or. Meaning a Maybe' can be either a Just' a or
-- Nothing.
data Maybe' a = Just' a | Nothing'

-- While this is a product, an and. The tuple is the product of something of
-- type a and something of type b.
data Tuple' a b = (a, b)
{% endhighlight %}

The unit interval however, is another scale to the onion.

The unit interval is the set of all the real numbers between $0$ and $1$. We use the bounds of this interval to be equal to our two maps given some $x$. This means that our $H$, our homotopy, is a function which through a transfinite series of inputs can "bend" $f$ into $g$.

That, in a nutshell is homotopy. But, as I delved deeper, like anything worth learning, that nascent intuition was simply a prerequisite.

### Univalence

__Univalence Axiom__: $(A = B) \approx (A \approx B)$.

The book says this about Univalence:

> Thinking of types as spaces, [...] the points of which are spaces[. T]o understand its identity type, we must ask, what is a path $p : A \leadsto B$ between spaces in $U$? The univalence axiom says that such paths correspond to homotopy equivalences $A \approx B$[.] (Univalent Foundations of Mathematics, 4)

It goes on to say that Univalence is informally an equivalence, and more specifcally _homotopy equivalence_. Homotopy equivalence is given by an isomorphism between two spaces where the following holds:

Given spaces $X$ and $Y$ and maps $f: X \rightarrow; Y$ and $g: Y \rightarrow X$, the composition $f \circ g$ is homotopic to $Id_X$ and the composition $g \circ f$ is homotopic to $Id_Y$.

To put this is another way, and how I actually wrote this down in my notebook when I was searching for the intuition: Univalence is a topological bijection and that bijection is a valid isomorphism in the domain of pure topology.  There's a inside joke about topologists: Toplogists can't tell the difference between an donut and a coffee mug.  This is precisely why!  The torus (donut) is homotopy equivalent, i.e. isomorphic, to a coffee mug.

So, when we talk about univalence in the context of Homotopy, we're talking about a form of equality, an isomorphism.

This concludes the initial desiderata for grappling with Homotopy Type Theory, or at least its introduction.  There's more to come, for the both of us.

{% include stats.html %}
