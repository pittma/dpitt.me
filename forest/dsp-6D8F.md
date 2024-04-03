---
title: The Chinese Remainder Theorem
published: 2024-02-02
tagged: true
tags: cryptography, math
related: dsp-9F0P
math: true
---

The CRT states that, given a set of congruences (where all of the
$$m$$s are coprime):

$$$$
x \equiv a_1\ \text{mod}\ m_1 \\
x \equiv a_2\ \text{mod}\ m_2 \\
\vdots \\
x \equiv a_n\ \text{mod}\ m_n \\
$$$$

We can solve for $$x$$ with the equation:

$$$$
x \equiv (\sum_{i=1}^n a_i \cdot M_i \cdot M_{i}^{-1})\ \text{mod}\ N
$$$$

Where $$M_i$$ is $$\cfrac{m_1m_2 \dots m_n}{m_i}$$ and $$M_{i}^{-1}$$
is $$M_i$$'s multiplicative inverse modulo $$m_i$$.

**Example**

We will start with the congruences 

$$$$
x \equiv 1\ \text{mod}\ 3 \\
x \equiv 2\ \text{mod}\ 7 \\
$$$$

This gives us the equation:

$$$$
(1 \times 7 \times 1) + (2 \times 3 \times 5)
$$$$

Because, for $$M_1$$, $$\cfrac{3 \cdot 7}{3} = 7$$ and $$7^{-1}\
\text{mod}\ 3 = 1$$ and for $$M_2$$, $$\cfrac{3 \cdot 7}{7} = 3$$ and
$$3^{-1}\ \text{mod}\ 7 = 5$$

This results in $$7 + 30 = 37\ \text{mod}\ 21$$, or $$16$$, which we
can check: $$16\ \text{mod}\ 3 \equiv 1$$ and $$16\ \text{mod}\ 7
\equiv 2$$.
