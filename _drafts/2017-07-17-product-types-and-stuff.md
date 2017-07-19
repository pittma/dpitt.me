---
layout: post
title: "[WIP] Product Types and Stuff"
tags:
  - type theory
---

Like Hemingway, I'm writing drunk.  Unlike Hemingway, though, I have no intention to edit sober.

This is an exposition on a thing that was once an object of rote memorization for me, but its purpose, its _why_ was recently elucidated through innocent math conversation bystandership.  This type of learning, this type of a new _why_ to add to my collection is frankly, intellectually, why  get I up in the morning.

What is fun is an existence without asking why?

To begin, we'll explicate the analogies used to rotely memorize what sum and product types are:
* __A sum type is a logical OR__, a _disjunction_ notated with  `∨` or `∪`.
* __A product type is a logical AND__, notated with `∧` or `∩` — A _conjunction_.

Okay, so disjunction, conjunction, and our last one, an exponent type, we'll get to later.  Before we can go much further on clairifing these terms, we need to first understand the term _cardinality_. Cardinality is roughly synonymous with size in mathematics, and is notated with double bars: `|S|` would read as "the cardinality of set `S`. If `S` were to look like this: `S := {1, 2, 3}`, then `|S| = 3`.

Cardinality is not limited to sets. It can be used with other mathematical structures, the most salient for our journey is of course a _type_.

Types are a complicated beast.

### What, exactly, is a type?

Should we start at the beginning?  Type Theory is the discovery of Per Martin-Löf, from the [seminal, collected works of intuitionistic type theory](http://people.csail.mit.edu/jgross/personal-website/papers/academic-papers-local/Martin-Lof80.pdf). Martin-Löf used propositional logic to _construct_ proofs, and then reuse these proofs via a name. Notatively, we'd see something like this: `a : A`, or `a` has type `A`. Now, who cares, what does this mean? Let's bear through a few examples.

Let's say we'd defined a proof that could prove a term, a thing, was a natural number, and we'll call that proof `Nat`, now when we come across a natural number, rather than carrying with us the entire proof for natural numbers, we can just say that this number, this _term_, has type `Nat`: `42 : Nat`. Cool, but not cool enough. What's cool is yet to come.  In the previous paragraph I used, and even accentuated the word _construct_; that was quite intentional. These proofs-as-types are constructable. Like building your own bikeshed, each component is constructed of its own constituents, down to pigment in the blue or red paint you pick for its exterior walls. Mathematical objects can be constructed similarly. We can begin to surmise further relationships between types, and then use _those_ types to construct more structures.

For instance, say we have an two objects, `A` and `B`, and two maps: <code>f(x<sub>i</sub>) = b<sub>i</sub> &in; B</code> where x is some member of `A` indexed by _i_, and its dual, <code>g(x<sub>i</sub>) = a<sub>i</sub> &in; A</code>. What we have here is a 1:1 mapping between `A` and `B`; a bijection. We also have, via `g`, a bijection between `B` and `A`. If we compose `f` and `g`, `g . f`, we have a round trip. This works both ways, too, insofar that `f . g` yields the opposite round trip. These maps compose to form an isomorphism between our objects `A` and `B`. Let's now add some details to our objects `A` and `B`, let's say `A : G` and `B : M`, and heretofore we've known `f : A -> B`, and `g : B -> A`. However, if were to say that `f` and `g` could operate on _any_ `G` or `M`: `f : forall a b. (G a, M b) => a -> b` and `g : forall a b. (G a, M b) => b -> a`, then we can now say that we _know_, and can name and carry with us, the type of isomorphisms between objects of type `G` and `M`.

In Haskell, these constructions come out as typeclass constraints. As a program grows, its built atop countless constructed proofs, some implicit, given to us via Prelude for example, or literals like `Num` or `String`, and others of our own devising. As we give types to our functions, and then layer other functions and types on top of those using the same semantics as we discussed before we are constructing a proof for our program. The code itself is its own proof.

{% highlight haskell %}
f :: Nat a => a -> a -> a
{% endhighlight %}

### Types and Cardinality

taking into consideration now the two things we've learned so far: cardinality and types, you can begin to imagine what a type's cardinality would look like.  If we were to revisit our `Nat` example before, type `Nat` would have a cardinality of [aleph-null](https://en.wikipedia.org/wiki/Aleph_number#Aleph-naught). The `Bool` type would have a cardinality of 2: The values which inhabit type `Bool` are just `True` or `False`. It's combination of these two items which can begin to make sense of what, exactly, a sum or product type may be.
