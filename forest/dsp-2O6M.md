---
title: Why 52?
subtitle: "Tracking the lineage of the 52-bit radix in OpenSSL's RSA implementation"
published: 2024-04-16
math: true
tagged: true
tags: cryptography, math
related: dsp-9F0P dsp-5U7X
---

[Fast modular squaring with
AVX512IFMA](https://eprint.iacr.org/2018/335.pdf) seems to be the
entrypoint here, I found it simply by googling the instruction
[`VPMADD52LUQ`](https://www.felixcloutier.com/x86/vpmadd52luq). It
cites [_Efficient Software Implementations of Modular
Exponentiation_](https://eprint.iacr.org/2011/239.pdf) stating

> Our developments build on top of the Almost Montgomery Square (AMS)
> optimization of [8]

But also cites [_Software Implementation of Modular Exponentiation,
Using Advanced Vector Instructions Architectures_](/files/sime.pdf) as
the reference for the 52-bit representation:

> These instructions multiply eight 52-bit unsigned integers residing
> in wide 512-bit registers, produce the low (VPMADD52LUQ) and high
> (VPMADD52HUQ) halves of the 104-bit products, and add the results to
> 64-bit accumulators (i. e., SIMD elements), placing them in the
> destination register. They are designed for supporting big number
> multiplications, when the inputs are stored in a ”redundant
> representation” using radix 252 (as explained in [9]).

This paper though, cites [_Parallel Cryptographic Arithmetic Using a
Redundant Montgomery
Representation_](https://ieeexplore.ieee.org/document/1336767) as a
starting place.

> For example, Page and Smart [19] suggested using SIMD architectures
> to calculate several exponentiations in parallel, and using a
> “redundant Montgomery representation” (which we call here Non
> Reduced) to avoid conditional final subtractions in Montgomery
> Multiplications.

This seems to the the origin of the "redundant" terminology we find
all over the code, but the Gueron paper states "which we call here Non
Reduced", so perhaps we'll find that _Software Implementation of
Modular Exponentiation, Using Advanced Vector Instructions
Architectures_ is the right place to start for the current RSA
implementation.

---

It does look like that's the case. The windows—which they refer to as
_digits_—of large numbers does come from _Software Implementations of
Modular Exponentiations_, which I've transcluded below for
simplicity's sake.


$transclude("dsp-9X1T")$
