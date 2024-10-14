---
title: Petri Nets, Graphs, and Concurrency
published: 2021-02-03
tagged: true
tags: math, category theory
math: true
---

This is borne from a conversation on the CT Zulip about graphs,
process calculi, and, eventually, Petri nets. It started
[here](https://categorytheory.zulipchat.com/#narrow/stream/235484-theory.3A-concurrency/topic/proarrow.20equipments.20in.20concurrency),
but that lead to
[here](https://categorytheory.zulipchat.com/#narrow/stream/235484-theory.3A-concurrency/topic/Nets.20Within.20Nets.20From.20Grothendieck),
which is where I'll start.


# Petri Nets

A graphical way to represent states, distributed systems, &c. They're
bipartite, consisting of /places/ and /transitions/. Places are
represented as circles while transitions are rectangles. There's a
third part to these graphs, called /arcs/ that are the edges
connecting places and transitions, or vice versa.

- tokens :: marks in a place that represent  a “current state” of that place.
- markings :: describes or captures the state of all the places w/r/t
  to their tokens in a given instance of a net.

## Grothendieck Contruction

The idea is that you've got a $$\textbf{Cat}$$ valued functor, $$F$$ for
some category $$\mathcal{C}$$: $$F: \mathcal{C} \rightarrow \textbf{Cat}$$
which you use to create a new category whose objects are pairs $$(c,
x)$$ where $$c \in \mathcal{C}$$ and $$x$$ is in $$\textbf{Cat}$$ via $$F(c)$$
and morphisms are pairs which work the same way, completing the
square. This is why it's called gluing, because it's the same idea.

$$$$
\xymatrix{
  F(c) \ar[r]^{F(f)} & F(c') \\
  c \ar[r]_f \ar[u]^F & c' \ar[u]_F\\
}
$$$$

Type theoretically, this is a sigma: $$\text{Groth}: \Sigma_{(c :
\mathcal{C})}F(c)$$ where $$\Pi_{(c : \mathcal{C})}F(c)$$ (of
course). The inhabitants of $$\text{Groth}$$ are the usual dependent
pairs: $$(x, F(x))$$.

~~An intuition that occurred to me while reading the blog post above:+
The purpose of the grothendieck contstruction is to create categories
from arbitrary objects from some other category.~~

It's actually that it glues things together into one big category,
$$\textbf{Cat}$$, because…, as seen above, $$F(c)$$ and $$F(c')$$ aren't
necessarily in the same category, until they are, in $$\textbf{Cat}$$.

Or maybe they are. The Petri net construction in Jade Masters post
below seems to rely on that.


# Nets within Nets within…

A [Jade Master blog
post](https://jadeedenstarmaster.wordpress.com/2021/01/29/nets-within-nets-from-the-grothendieck-construction/)
to cover hierarchical nets where a net's tokens are themselves marked
nets. This arose after a discussion about representing the two types
of graphs that one may choose to model concurrent actors. Namely, the
timeline view, illustrating interacting actors, and the transition
view, that illustrates the causal relation from event to event.

Masters gives a construction for an /object system/, which consists of
a hierarchy of nets where each place in a global net is its own local
net, made possible through category constructed through Grothendieck
which she calls $$\textbf{MkPetri}$$ whose objects are pairs of nets and
their firing sequences and morphisms are morphisms between those nets
and their corresponding sequences. A functor $$E$$ is then given to take
an arbitrary net category, $$F P$$, and bring it into $$\textbf{MkPetri},
effectively flattening the places in $$P$$. Something has to retain the
relation for the nested nets and their parents, the intuition I could
come up with was that $$E$$ acts as a rewriter, rewriting the places in
the parent net with the child net.

# The Structure of Concurrent Process Histories
