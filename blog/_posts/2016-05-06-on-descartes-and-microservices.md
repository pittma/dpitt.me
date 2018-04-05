---
layout: post
title:  "On Descartes and Microservices"
date:   2016-05-06 15:43:05
---

And other annoying shit I've just made up.

This post is about dependency injection, a decoupling technique used by programmers often with wanton abandon.

I'm not going to apologize for the above non-sequitur, but rather to attempt to contextualize it.  To begin, let's talk about Descartes:

### René Descartes

Descartes is responsible for what is probably the penultimate existential statement, eclipsed only by Shakespeare's _"to be or not to be"_.  Descartes's goes like this:

> Cogito ergo sum

Or

> I think, therefore I am.

Descartes's whole spiel was I can't doubt that I exist, because there must be an "I" to doubt "I".  That is, doubting my existence is dependent on me existing and then doubting that I exist.  An interesting strange loop indeed.

### Computers

I recently found myself in an analgous existential argument about when a component in a program commenced to exist.

This component is _dependent_ on another, and my coworker was of the school of thought that this component's constructor should be _decoupled_ from the injection of its dependency. Something like this:

```go
func main() {
	t1 := thing1.New()
	t2 := thing2.New()

	t1.BindThing2(t2)
}
```

However, _I_ believed that there was no reason for `t1` to even be constructed if it there was not yet a t2 to meet its dependency.  Like this:


```go
func main() {
	t2 := thing2.New()
	t1 := thing1.New(thing2)
}
```

My coworker had a compelling argument.  For us to reach optimal clarity on his argument, we first must be _live_.  That is, __consistent__ on our understanding of a few things, so I must digress.

---- Begin Digression ----

#### Microservices

A overloaded term which, to me, means Domain Driven Design, where each "domain" is composed of subdomains (read: microservices) of functionality.

For example, you have a group of services which mutate `Foo`s, and a group of services which mutate `Bar`s.  Each of these "domains" have independent services which have discrete responsibilities such as `paint-bar` or `flip-foo`. A service in the `Foo` domain has no knowledge of a `Bar` whatsoever.  Ideally not even through a dependency like an ORM with both `Foo` and `Bar` declarations within it.  This complete, physical delineation protects each domain from the other's faults.

#### The Go Programming Language

An interesting thing about the design of the Go programming language is that the language itself gently leads you down the path towards something microservice-like even though your software may exist completely in a single runtime. This is done through goroutines, channels, and long-running loops. With Go, you find yourself writing a distributed system, passing messages back and forth between disparate, long-running, reactive loops all within a single runtime. It's important to consider this mindset when considering my coworker's argument.

---- End Digression ----


So, given this context, when does a service commence existence? Is it at instantiation? Or, when it becomes _useful_, with all its subsystems in line and ready to take on the world?

What do you think?

{% include stats.html %}
