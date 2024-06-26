---
title: Linear Algebra Fundamentals
published: 2024-06-03
math: true
tagged: true
tags: math
---

I have never studied linear algebra at all, so we're going back to the
beginning. Each header below are parts that I had to think hard about,
so it may skip over stuff.

# Matrix Multiplication

To multiply two matrices $$A$$ and $$B$$, $$B$$ must have the same
number of rows as $$A$$ has columns. For the equation $$A \times B =
C$$, each index in $$C$$ is determined by:

$$$$
C_{i, j} = \sum_k A_{i, k} B_{k, j}
$$$$

Where $$k$$ is that matched rows in $$B$$ and columns in $$A$$---which
you can see in the equation above---is what we're iterating over,
respectively.

Thinking about this by indices in $$C$$ was the missing link for
anytime I've tried to learn this in the past, it feels like one of
those things that's obvious to everyone who knows it already.

# Eigendecomposition

Not my first run-in with the Eigen-family of mathematical objects, but
it is my first time to buckle in and figure out how and why exactly
they work. The book sets this up by talking about representation and
gives the example of how an integer, whether represented in binary or
base-10, can still be represented via its prime
factors. Eigendecomposition is purportedly a way to represent matrices
via their functional properties.

The core idea is this: The eigendecomposition of a matrix $$A$$ is the
factorization $$A = Q \Lambda Q^{-1}$$, where $$\Lambda$$ is a
diagonal matrix containing the eigenvalues of $$A$$. 

An eigenvalue has a corresponding vector $$\mathbf{v}$$ with respect
to $$A$$ s.t. $$A \mathbf{v} = \lambda \mathbf{v}$$ where $$\lambda$$
is the eigenvalue corresponding to the vector $$\mathbf{v}$$.

To compute Eigendecomposition we first find the eigenvalues and vectors.

For the following examples, we'll use 

$$$$
A = \begin{pmatrix}
4 & 1 \\
2 & 3
\end{pmatrix}
$$$$ 

## Eigenvalues

Solve $$\operatorname{det}(A - \lambda I) = 0$$, the results are the
eigenvalues. $$I$$ is the identity matrix. The identity matrix is a
square matrix of a specific size which has 1's along its diagonal and
0's elsewhere.

### Determinates

$$\operatorname{det}$$ is a _determinate_. For 2x2 matrices of the form

$$$$
\begin{pmatrix}
a & b \\
c & d
\end{pmatrix}
$$$$

$$\operatorname{det}$$ is simply $$ad - bc$$, but apparently for
larger matrices, computing the determinate is much more complicated.

1. Compute the matrix difference, which is done elementwise. $$A -
   \lambda I$$ becomes

$$$$
\begin{pmatrix}
\lambda 1 & \lambda 0 \\
\lambda 0 & \lambda 1
\end{pmatrix} =
\begin{pmatrix}
\lambda & 0 \\
0 & \lambda
\end{pmatrix}
$$$$

so we end up with

$$$$
\begin{pmatrix}
4 - \lambda & 1 - 0 \\
2 - 0 & 3 - \lambda
\end{pmatrix} =
\begin{pmatrix}
4 - \lambda & 1 \\
2 & 3 - \lambda
\end{pmatrix}
$$$$

2. Then compute the determinate

$$$$
\operatorname{det}(\begin{pmatrix}
4 - \lambda & 1 \\
2 & 3 - \lambda
\end{pmatrix}) = 
(4 - \lambda)(3 - \lambda) - (2 \times 1) = 
(4 - \lambda)(3 - \lambda) - 2 
$$$$

Which we FOIL-ify to

$$$$
12 - 4\lambda - 3\lambda + \lambda^2 - 2
$$$$

Yielding us the polynomial (recall that in computing the eigenvalues,
$$\operatorname{det}(A - \lambda I) = 0$$).

$$$$
\lambda^2 - 7\lambda + 10 = 0
$$$$

The solutions to this polynomial are the eigenvalues. We factor into

$$$$
(\lambda - 5)(\lambda - 2) = 0
$$$$

Because $$-5 \times -2 = 10$$ and $$-5 + -2 = -7$$, which we can then
isolate individually to get $$\lambda - 5 = 0$$ so $$\lambda = 5$$ and
$$\lambda - 2 = 0$$ so $$\lambda = 2$$. **These are our eigenvalues**.

## Eigenvectors

For each eigenvalue $$\lambda_i$$, we solve $$(A - \lambda_i I)
\mathbf{v} = \mathbf{0}$$[^zero].

[^zero]: I'm using bold face here on $$\mathbf{0}$$ to denote the zero
    vector.

Continuing with our example, first we solve with $$\lambda = 5$$:

To start 

$$$$
5I = \begin{pmatrix} 5 & 0 \\ 0 & 5 \end{pmatrix}
$$$$

Then

$$$$
\begin{pmatrix}
4 & 1 \\ 
2 & 3
\end{pmatrix} - 
\begin{pmatrix}
5 & 0 \\
0 & 5
\end{pmatrix} = 
\begin{pmatrix}
-1 & 1 \\
2 & -2
\end{pmatrix}
$$$$

So we're solving

$$$$
\begin{pmatrix}
-1 & 1 \\
2 & -2
\end{pmatrix}
\begin{pmatrix}
v_1 \\
v_2
\end{pmatrix} =
\begin{pmatrix}
0 \\
0
\end{pmatrix}
$$$$

Which we can decompose (possibly a poor word choice) into

$$$$
-1v_1 + 1v_2 = 0 \\
2v_1 - 2v_2 = 0
$$$$

In both cases, we end up with a case s.t. $$v_1 = v_2$$, so "the
simplest option" is "chosen"[^aoc], yielding us the eigenvector

[^aoc]: I thought this may be an example of the axiom of choice,
    choosing arbitrarily from the infinite set of equal tuples. After
    asking around, consensus was that no, this is a "practical
    selection", not an arbitrary one.

$$$$
\begin{pmatrix}
1 \\
1
\end{pmatrix}
$$$$

with respect to $$\lambda = 5$$.

**Let's work through an example with our other eigenvalue, 2.**

To start 

$$$$
2I = \begin{pmatrix} 2 & 0 \\ 0 & 2 \end{pmatrix}
$$$$

Then

$$$$
\begin{pmatrix}
4 & 1 \\ 
2 & 3
\end{pmatrix} - 
\begin{pmatrix}
2 & 0 \\
0 & 2
\end{pmatrix} = 
\begin{pmatrix}
2 & 1 \\
2 & 1
\end{pmatrix}
$$$$

So we're solving

$$$$
\begin{pmatrix}
2 & 1 \\
2 & 1
\end{pmatrix}
\begin{pmatrix}
v_1 \\
v_2
\end{pmatrix} =
\begin{pmatrix}
0 \\
0
\end{pmatrix}
$$$$

Which we can decompose into

$$$$
2v_1 + 1v_2 = 0 \\
2v_1 + 1v_2 = 0
$$$$

These two equations are identical, and their simplest solution is

$$$$
\begin{pmatrix}
1 \\
-2
\end{pmatrix}
$$$$

## Finally, we arrive

Recall that **The eigendecomposition of a matrix $$A$$ is the
factorization $$A = Q \Lambda Q^{-1}$$**.

* $$Q$$ consists of the two eigenvectors as columns:

  $$$$
  \begin{pmatrix}
  1 & 1 \\
  1 & -2
  \end{pmatrix}
  $$$$
  
* And $$\Lambda$$ has the two eigenvalues along its diagonal, the
  other elements are 0:
  
  $$$$
  \begin{pmatrix}
  5 & 0 \\
  0 & 2
  \end{pmatrix}
  $$$$
