---
title: Software Implementation of Modular Exponentiation, Using Advanced Vector Instructions Architectures
published: 2024-04-16
math: true
tagged: true
tags: cryptography, math, paper notes
related:  dsp-2O6M
---

This was published in [Arithmetic of Finite
Fields](https://link.springer.com/book/10.1007/978-3-642-31662-3), but
I've grabbed just this chapter [here](/files/sime.pdf).

This paper goes into detail about the algorithms used, originally for
AVX2, the x86 instructions that those algorithms employ, and so on,
but what I was really looking for is the origin of the 52-bit
representation found in the current AVX-512 implementation found in
OpenSSL, [soon AWS-LC](https://github.com/aws/aws-lc/pull/1273), and
maybe some day in BoringSSL. It turns out, this paper contains that
origin, only here we're talking about the conversion of 32-bit numbers
to 29-bit numbers (arbitrarily, I think) and are given the following
equation as the way in which to convert numbers from one radix to
another.

$$$$
A = \sum^{k-1}_{i=0} X_i \times 2^{m \times i}
$$$$

Where $$A$$ is the value we wish to represent, $$X_i$$ is the value of
the $$i$$th _window_, or as the paper calls it, the $$i$$th digit, and
$$m$$ is the window size in bits.

So, say we want to represent the 16-bit number 24023 in windows of 5 bits.
```
0101110111010111
```
We think of as the following windows:
```
0 10111 01110 10111
```
Where $$m = 5$$ and $$k = 4$$, giving us

```
0   10111              01110             10111
0 + (23 * 2^(5 * 2)) + (14 *2^(5 * 1)) + (23 * 2^(5 * 0))
0 + 23552            + 448             + 23               = 24023
```
This also holds if we choose 3-bit windows:
```
0   101               110               111               010               111
0 + (5 * 2^(3 * 4)) + (6 * 2^(3 * 3)) + (7 * 2^(3 * 2)) + (2 * 2^(3 * 1)) + (7 * 2^(3 * 0))
0 + 20480           + 3072            + 448             + 16              + 7               = 24023
```

This representation is the one used in the AVX-512 implementation
where $$m = 52$$, carving up, for instance, the 1024-bit `bignum`s
into 52-bit windows.
