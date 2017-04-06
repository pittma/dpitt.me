---
layout: post
title:  "CRDTs, Bounds, SyncFree, and Invariants"
date:   2015-02-02 23:38:42
---

I finally got around to watching the SyncFree consortium's [talk about their work on CRDTs](https://www.youtube.com/watch?v=1KP_pxFhlVU) at RICON 2014.  Or at least half of it, rather, and I was really struck by their Bounded CRDT<sup>1</sup>s idea.  The idea of a bounded CRDT is this: using a predefined _invariant_, you allow a certain count of "interactions" per replica.  They are likened to leases or reservations in the talk. I like reservations better:

![Okay](https://dl.dropboxusercontent.com/u/42154947/blog%20pics/ok.png)

Here, in an over-simplified example, we have a counter.  This counter changes with increments of 1, i.e. `counter++` or `counter--`.  We have set an invariant of `cannot be < 0`, and we use this invariant to make a _guarantee_.  If each replica can only make 1 operation against the current known value (4), then we will never violate the invariant.  However, should a second operation be performed on any replica, we are no longer convergent: the result of this would have counter's value equal to -1:

![Not Okay](https://dl.dropboxusercontent.com/u/42154947/blog%20pics/not%20ok.png)

This is my primitive understanding, or my assumption, after watching 20 minutes of the RICON talk on this today.  What I'm most interested in is the bound that comes from the reservation count. This is the third time I'll bring this paper up on this blog, which I can accept -- maybe I'm a bit obsessed. The [Peter Balis, et al. paper](http://www.bailis.org/papers/ca-vldb2015.pdf) on Coordination Avoidance.  This paper, from a 10,000ft perspective, says only coordinate when you have to.  Define invariants, coordinate when one is violated.

It looks to me that a bounded CRDT is using this same concept, but using the invariant to avoid conflicts.

As a background process, the "Reservation Manager" above doles out reservations, allowing each replica to operate on an object _N_ times. When the Nth operation is reached, consensus is needed.  If you are clever enough, and your strongly consistent Reservation Manager service can stay far enough ahead, you never have to sync.

### Here comes some math

Mathematically, CRDTs are all about a join semilattice, and so I'm about to attempt to surmise what the lattice is in the context of a bounded CRDT.

Reservations is a semilattice _`(R,∨)`_. The current operation, if operations are _`O⊆R`_ is _`R ∨ O`_.  That is, the Least Upper Bound of _`R`_ is the current Operation Count.  This meets semilattice requirements of [commutativity, associativity, and idempotency](http://en.wikipedia.org/wiki/Semilattice#Algebraic_definition) in that with each operation _`⊤O`_ will always be equal to some element in _`R`_ so long as _`R`_ remains unbounded. Therefore  _`R v O`_ holds for any _`O⊆R`_.  Like I said, if you are clever enough you can keep your Reservation Manager far enough ahead.

This CRDT concept is a bit more complicated than the basic commutative or monotonic models, but is still farily easy to understand conceptually.  I'm very interested in what comes next from the SyncFree humans, namely the adaptive stuff, which is used to reduce replicas -- which decreases the LUB, which makes pre-determining reservations easier, which is 👍.

<div style="font-size: 0.8em; color: #555">
1. Read <a href="http://christophermeiklejohn.com/crdt/2014/07/22/readings-in-crdts.html">a bunch of stuff about CRDTs here</a> if you are so inclined.
</div>
