---
title: Montgomery Multiplication
published: 2024-03-13
tagged: true
math: true
tags: math, cryptography
related: dsp-9F0P
---

Montgomery multiplication is a faster way to do multiplication which
prefers base 2 to base 10, and shines when used iteratively. Numbers
are "represented" in montgomery form during the primary operations,
and then once those operations are complete, they are transported back
to their original form. It solves a modular multiplication of the
form:

$$$$
a * b\ \text{mod}\ m
$$$$

We will use $$a$$, $$b$$ and $$m$$ in our example below.

First we compute or choose a few constants. These can be precomputed
and then retained, they don't need to be regenerated each time.

1. Choose a power of two that's greater than $$m$$, this is
   $$r$$. Below I'm computing a number, but this could just as easily
   be chosen to be constant $$2^{64}$$ or something like that.

	```haskell
	r = 2 ^ ceiling (logBase 2 (fromIntegral m))
	```

1. Square $$r$$, modulo $$m$$. This we'll call $$r_2$$.

	```haskell
	r2 = r * r `mod` m
	```

1. Compute the multiplicative inverse of $$-m\ \text{mod}\ r$$, this is
   $$m'$$
   
   ```haskell
   m' = inv (negate m) r
   ```

Now we'll use these to compute the montgomery form of our input
variables.

1. Multiply $$a * r_2$$, this is $$a_m$$.
   ```haskell
   am = a * r2
   ```

1. Multiply $$a_m$$ by $$m'\ \text{mod}\ r$$, I'll call this $$a_m'$$
   ```haskell
   am' = am * m' `mod` r
   ```

Next we do a reduction of the montgomery form.

1. Do $$\cfrac{a_m + a_m' * m}{r}$$. This temp value we'll call $$t$$.
   ```haskell
   t = am + am' * m `div` r
   ```

1. The result montgomery form is $$t$$ if $$t < m$$, otherwise it's $$t - m$$.
   ```haskell
    if t < m
      then t
      else t - m
   ```
   
We do the same for $$b$$, then 
