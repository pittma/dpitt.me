---
title: Montgomery Multiplication
published: 2024-02-02
tagged: true
tags: cryptography, math
related: dsp-9F0P
---

_Cryptography depends on computationally difficult problems. The
reason it works at all is because, take RSA for instance, finding the
prime factors of a very large number would take a classical computer
an implausible amount of time. But as I learned in [RSA for the
Impatient](/forest/dsp-9F0P.html), there are computationally difficult
problems involved just in key generation and encryption and decryption
itself, even when we're in possession of the key pair and don't need
to crack them. The topic of this post is targeting one those
computationally expensive problems—modular multiplication of very
large numbers._

_A technique known as Montgomery Multiplication takes an expensive
multiplication and division problem and projects it into a field where
division is as easy to a computer as repositioning a decimal is to
us. Why? Well, because we operate in base 10, but a computer operates
in base 2._

---


