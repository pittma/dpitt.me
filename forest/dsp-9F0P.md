---
title: RSA for the impatient
published: 2024-01-31
math: true
tagged: true
tags: cryptography, math
related: dsp-5Q3M
---

_I've been asked to document the steps in an Almost Montgomery
Multiplication implementation at work which is terrifying, because
thus far, all I've really needed to know to port this patch from
OpenSSL to BoringSSL is a little about AVX-512 instruction set
extensions and some details on Linux's or Windows' calling
conventions. The hardest part of all of this has been teaching the
FIPS assembly parser how to understand mask register syntax. Until
now…_

_I've decided to walk, step-by-step, through RSA encryption, starting
with the most naive exposition possible._

---

# Simplest Possible Explanation

## I. Key Generation

**Choose two large primes $$p$$ and $$q$$**. For our sake, we're going
to use the absolutely massive numbers 7 and 13.

**Compute their product**; this is $$n$$: $$7 \times 13 = 91$$.

**Calculate Euler's totient function of $$n$$**, denoted as
$$\phi(n)$$, where $$\phi(n) = (p - 1)(q - 1)$$[^tot].

[^tot]: Euler's totient function counts the number of coprime positive
    integers less than $$n$$. The product of two primes is a special
    case and that's what we're seeing here.

**Chose a public exponent $$e$$ such that $$1 \lt e \lt \phi(n)$$, and
that $$e$$ is coprime with $$\phi(n)$$**. $$e$$ is a part of the
public key. Well, our $$\phi(n) = (7 - 1)(13 - 1) = 6 \times 12 =
72$$, so why don't we "choose" $$17$$ as our $$e$$.

**Compute the private exponent $$d$$ such that $$d \times e =\ 1\
\text{mod}\ \phi(n)$$** ($$d$$ is $$e$$'s multiplicative inverse
$$\text{mod}\ \phi(n)$$).
	 
This inversion is what allows $$e$$ and $$d$$ to "undo" each other,
which is what allows the original message to be retrieved from the
cipher text.  So our modulus is $$72$$, and our $$e$$ is $$17$$, from
here we need to compute $$17$$'s multiplicative inverse $$\text{mod}\
72$$, which we can do using using the Euclidean algorithm. Here's a
small Haskell function that computes an inverse given the modulus and

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

This we can break down with the recursion of our `inv` function:

* We call `inv 17 72`, this means we end up calling the recursor as `f
  0 72 1 17`.
* We catch the `otherwise` case, this sets `q = 72 / 17 = 4`.
* So we recurse with `f 1 17 (0 - (4 * 1)) (72 - (4 * 17))` or `f 1 17
  -4 4`.
* We hit the `otherwise` case again. This time `q = 17 / 4 = 4` so we
  recurse with `f -4 4 (1 - (4 * -4)) (17 - (4 * 4))` or `f -4 4 17
  1`.
* still in the `otherwise` case, so we compute `q = 4 / 1 = 4` and
  recurse with `f 17 1 (-4 - (4 * 17)) (4 - (4 * 1))` or `f 17 1 -72
  0`.
* This time we hit the base case because `newr` is `0`, so we return
  our answer, which is `abs t`, or `17`.
* We can check this because `17 * 17 (mod 72) = 1` (what are the odds
  that I picked an `e` that's its own inverse?).

## II. Encryption

"To encrypt a message $$M$$, represent it as a number smaller than
$$n$$". Crap. Not much information in the 6 bits it takes to contain
the number 91.

Let's see, could we "represent" a three-letter message: $$26 \times 3
= 78$$? We could add together each digit's numerical representation…
"dog" would be $$4 + 15 + 7 = 26$$. Well, no, because how do we
separate it again?  How do we get "dog" out of 26? It could just as
easily be "balk": $$2 + 1 + 12 + 11$$. I think the best we can do is
have a 3-digit code where each digit is 0-3, dividing our 6 bits into
3 groups of 2. So why don't we send the message "213", this would be
`10 01 11` in binary, or, in base 10, 39.

Next we compute the cipher text, $$C = M^e\ \text{mod}\ n$$. Our
cipher text is $$39^{17}\ \text{mod}\ 91 = 65$$.

## III. Decryption

Simply do the inverse! $$M = C^d\ \text{mod}\ n = 65^{17}\ \text{mod}\
91 = 39$$ Which, of course, we can de-numerical-ize as 213, left to
right, via its couplets, `10 01 11`.

Cool, we did it. We're cryptographers now.

# Optimizations

$transclude("dsp-6D8F")$

When we break our encryption and decryption operations up into CRT
form, where

$$$$
C = M^e\  \text{mod}\ n
$$$$

Becomes

$$$$
C \equiv M^e\ \text{mod}\ p \\
C \equiv M^e\ \text{mod}\ q
$$$$

Because, recall, $$n = p \cdot q$$.

We can now use Euler's Theorem, which tells us that

$$$$
x^{e\ \text{mod}\  \phi(m)} \equiv x^e\ \text{mod}\ m
$$$$

Which reduces significantly the number of multiplications needed
because the exponent on the left-hand side of this congruence is much
smaller than the one the right. Once we've done this smaller
exponentiation, we simply use the CRT to recombine the results.
