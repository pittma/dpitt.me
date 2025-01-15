---
title: A Sad Story About the Meaning of 'Constant Time'
subtitle: Just when I was starting think I was getting the hang of this.
published: 2024-10-07
tagged: true
tags: cryptography, math, programming
math: true
related: dsp-9F0P dsp-9X1T dsp-5U7X
---

_A few weeks ago, I finally got the massive RSA OpenSSL port merged
into AWS-LC, after a months-long effort of documenting, refactoring,
and justifying someone else's decisions_[^notme]_. The most difficult
part of the documentation process was
[this](https://github.com/aws/aws-lc/blob/bda01b40727126e8cc2ec1e375301700b0fa6ba7/crypto/fipsmodule/bn/rsaz_exp_x2.c#L346-L363),
where a pretty wild optimization from the original [AMM
paper](https://eprint.iacr.org/2011/239.pdf) needed to be
documented. It took me a few days to internalize it well enough to
know what exactly was going on, and how it corresponded to the
algorithm in the paper. On top of this, the **extraction** step of the
algorithm is implemented in
[perlasm](https://github.com/aws/aws-lc/blob/bda01b40727126e8cc2ec1e375301700b0fa6ba7/crypto/fipsmodule/bn/asm/rsaz-2k-avx512.pl#L505-L549)
which, despite only being ~40 lines, is very abstruse. But, once I had
my bearings about me, I noticed an opportunity._

[^notme]: You may recall that this is code that was originally written
    by Intel upon the release of Icelake in 2017, but it had never
    been ported to BoringSSL, which is the ancestor of AWS-LC.

---

# I. The Algorithm

## Motivation

What's happening here is the modular exponentiation of very large
numbers, on the order of $$2^{1024}$$ (in the least)[^modlen]. These
exponentiations are done with Montgomery Multiplication, which I have
documented elsewhere. The part I want to cover here _uses_ that
routine, but does further optimizations on top of that implementation
to make these massive exponentiation problems more tractable to do on
a computer. The paper calls this "w-ary exponentiation".

[^modlen]: RSA's modulus is the product of two large primes $$p$$ and
    $$q$$, we call that product $$n$$. In RSA 2k, $$p$$'s and $$q$$'s
    bit-width is 1024.

## In Detail

Compute $$a^b$$ by _windowing_ the bits of $$b$$ into windows $$w$$
of bit length $$l$$.

<table>
<tr><td><p>**Input:**</p></td><td><p>$$a$$, $$b$$</p></td></tr>
<tr><td><p>**Return:**</p></td><td><p>$$a^b$$</p></td></tr>
</table>

**Formally, we're computing**

$$$$
\textbf{exp}(a, w) = \prod_{i=0}^{|w|} (a^{w_i})^{2^{li}}
$$$$

Which is equivalent to $$a^b$$.

**Step-by-Step:**

1. Precompute $$a^{[0 \ldots 2^l)}$$, store the results in the
   table $$T$$, indexed by the exponent.
1. Initialize **Result** to $$1$$.
1. For each $$w_i$$:
   1. Look up the result of $$a^{w_i}$$ in $$T$$, this is $$T_i$$
      ($$a^{w_i} = T_i$$).
   1. Square $$T_i$$ $$li$$ times, this is a temporary value we can
      call $$t$$. This squaring is what accounts for the current
      window's position in the whole of the larger exponent.
   1. **Result** is set to the current **Result** times $$t$$. 


## Example

We'll use this method to solve $$5^{273}$$ with $$l = 3$$.

To begin, let's ask Haskell to do this for us:

```haskell
ghci> 5^273
65888737145190770152178587820437618798187258639803172187986018906925901818560997451564846065309816181644800521439740475440941611838012288617347746101822469899644829638418741524219512939453125
```

Yikes. Let's just call that `expected`:

```haskell
ghci> expected = 5^273
```

Now let's window the exponent into 3-bit windows.

```
100 010 001
  4   2   1
```

This means our reassembled exponentiation is

```haskell
ghci> :{
ghci| (1 *                    -- Initial value
ghci|   ((5^1) ^ (2^(3*0))) * -- First window, 5^1 would be in the table.
ghci|   ((5^2) ^ (2^(3*1))) * -- Second window, 5^2 would be in the table.
ghci|   ((5^4) ^ (2^(3*2)))   -- Third window, 5^4 would be in the table.
ghci| ) == expected
ghci| :}
True
```

# II. The Opportunity

In the preamble to this post I linked to some perlasm code which
constitutes the table lookup discussed in the previous section. This
routine takes a pointer to the table and a pair of indices. The table
is a nested array. The outer array holds the powers we're looking for,
and each inner array is the collection of 64-bit digits needed to
represent that power. This particular implementation is looking up the
power of two separate exponents because the larger routine is doing
parallel exponentiations. For readability, I expanded and gently
scrubbed the perlasm below, and added comments.

```perl
# set up our iterator.
vmovdqa64   .Lones(%rip), %ymm24

# broadcast the indices
vpbroadcastq    %rdx, %ymm22
vpbroadcastq    %rcx, %ymm23

# put a pointer to the end of the table in rax.
leaq   10240(%rsi), %rax 

# zero out ymm0
vpxor   %xmm0, %xmm0, %xmm0

# zeroing out ymm21 and ymm[1-5,16-19]
vmovdqa64   %ymm0, %ymm21
vmovdqa64   %ymm0, %ymm1
vmovdqa64   %ymm0, %ymm2
vmovdqa64   %ymm0, %ymm3
vmovdqa64   %ymm0, %ymm4
vmovdqa64   %ymm0, %ymm5
vmovdqa64   %ymm0, %ymm16
vmovdqa64   %ymm0, %ymm17
vmovdqa64   %ymm0, %ymm18
vmovdqa64   %ymm0, %ymm19

.Lloop:
    # Check if our current index matches either of the given indices. When they
    # match, k1 or k2 is a mask of all 1's.
    vpcmpq  $$0, %ymm21, %ymm22, %k1
    vpcmpq  $$0, %ymm21, %ymm23, %k2

    # vpblend brings over the bytes from the table when the indices match, but
    # leaves things in the destination register (because it is the same as the
    # second source register) when the indices do not match.
    vmovdqu64  (%rsi), %ymm20
    vpblendmq  %ymm20, %ymm0, %ymm0 {%k1}
    vmovdqu64  32(%rsi), %ymm20
    vpblendmq  %ymm20, %ymm1, %ymm1 {%k1}
    vmovdqu64  64(%rsi), %ymm20
    vpblendmq  %ymm20, %ymm2, %ymm2 {%k1}
    vmovdqu64  96(%rsi), %ymm20
    vpblendmq  %ymm20, %ymm3, %ymm3 {%k1}
    vmovdqu64  128(%rsi), %ymm20
    vpblendmq  %ymm20, %ymm4, %ymm4 {%k1}
    vmovdqu64  160(%rsi), %ymm20
    vpblendmq  %ymm20, %ymm5, %ymm5 {%k2}
    vmovdqu64  192(%rsi), %ymm20
    vpblendmq  %ymm20, %ymm16, %ymm16 {%k2}
    vmovdqu64  224(%rsi), %ymm20
    vpblendmq  %ymm20, %ymm17, %ymm17 {%k2}
    vmovdqu64  256(%rsi), %ymm20
    vpblendmq  %ymm20, %ymm18, %ymm18 {%k2}
    vmovdqu64  288(%rsi), %ymm20
    vpblendmq  %ymm20, %ymm19, %ymm19 {%k2}

    # loop control. Increment each value in 21, bump the table pointer up, and
    # make sure we haven't reached the end of the table.
    vpaddq  %ymm24, %ymm21, %ymm21
    addq    $$320, %rsi
    cmpq    %rsi, %rax
    jne .Lloop

# copy the extract results to the destination pointer.
vmovdqu64   %ymm0,  (%rdi)
vmovdqu64   %ymm1,  32(%rdi)
vmovdqu64   %ymm2,  64(%rdi)
vmovdqu64   %ymm3,  96(%rdi)
vmovdqu64   %ymm4,  128(%rdi)
vmovdqu64   %ymm5,  160(%rdi)
vmovdqu64   %ymm16, 192(%rdi)
vmovdqu64   %ymm17, 224(%rdi)
vmovdqu64   %ymm18, 256(%rdi)
vmovdqu64   %ymm19, 288(%rdi)
ret
```

If you're anything like me, once you've internalized the details of
the algorithm and understand the assembly above, you'll also notice
that what is happening here is pretty inefficient. If we were writing
this in an imperative language, we'd probably do something like:

```python
for i in range(n_windows):
    if idx == i:
        out = table[i]
        break
```

Because once we've found what we're looking for, we should stop
looking! The opportunity we have here, though, is even better. The
results in the table are ordered, so we can compute _exactly_ where
the power(s) we're looking for is(are). Something like:

```python
out = table[idx * num_digits]
```

Well, that implementation ends up looking like
[this](https://github.com/pittma/aws-lc/blob/becc97a05c568fad2f13ea6ed46264ea1ffbd072/crypto/fipsmodule/bn/asm/rsaz-2k-avx512.pl#L505-L555):

```perl

    # Copy the table pointer into %r11 so we can mess with it.
    movq $$red_tbl, %r11


    # Calculate the offset for idx1
    mov \$$320, %r10d
    imul $$red_tbl_idx1, %r10d


    # Bump the pointer
    addq %r10, %r11


    # Zero the temp register
    vpxor %xmm0, %xmm0, %xmm0


    # Copy to the result pointer
    vmovdqu64 (%r11), %ymm0
    vmovdqu64 %ymm0, ($$out)
    vmovdqu64 32(%r11), %ymm0
    vmovdqu64 %ymm0, 32($$out)
    vmovdqu64 64(%r11), %ymm0
    vmovdqu64 %ymm0, 64($$out)
    vmovdqu64 96(%r11), %ymm0
    vmovdqu64 %ymm0, 96($$out)
    vmovdqu64 128(%r11), %ymm0
    vmovdqu64 %ymm0, 128($$out)


    # Reset table pointer
    subq %r10, %r11


    # Calculate the offset for idx2
    mov \$$320, %r10d
    imul $$red_tbl_idx2, %r10d


    # Bump the pointer
    addq %r10, %r11


    # Re-zero the temp register
    vpxor %xmm0, %xmm0, %xmm0


    # Copy to the result pointer
    vmovdqu64 160(%r11), %ymm0
    vmovdqu64 %ymm0, 160($$out)
    vmovdqu64 192(%r11), %ymm0
    vmovdqu64 %ymm0, 192($$out)
    vmovdqu64 224(%r11), %ymm0
    vmovdqu64 %ymm0, 224($$out)
    vmovdqu64 256(%r11), %ymm0
    vmovdqu64 %ymm0, 256($$out)
    vmovdqu64 288(%r11), %ymm0
    vmovdqu64 %ymm0, 288($$out)
    
    ret
```

And yeah, this turns out to be a lot faster! I saw a ~4% improvement
in throughput for RSA 2k signing and I suspect this would be the same
or better for 3k and 4k. Unfortunately, the story does not end here.

# III. "Constant Time" (with scare quotes)

If you're a cryptographer and you've made it this far, I'm sorry. I
can imagine you screaming into your monitor as you read the previous
section.

To explain why a seasoned cryptographer would be getting paroxysmic
about the code I pasted above, I need to expand on the definition of
the term "constant time".

## Constant Time (without scare quotes)

**The part that I knew** (which was wrong):

Don't do things that are not constant in _time_. For example, division
on a computer is not constant unless the divisor is a power of 2.

**The part that I learned along the way:**

Don't use secret data to do anything that is not constant in time
because it is a side-channel vulnerability. An attacker can determine
the details of your secrets by watching how long it takes you to do
stuff. If we'd done something like the loop with `break` above, this
wouldn't be constant w/r/t time, but because it's done with a
multiplication to jump directly to the result we're looking for, I
thought I was safe.

**The part I didn't learn until I made a fool of myself by offering
this optimization:**

"Time" in "constant time" actually means time _and space_. This means
you cannot use secret data to determine your memory accesses because
an attacker can suss out details about your secrets by watching your
memory access patterns. Because the windows we're using as indices are
chunks of the exponent, and the exponent is secret, we can't use it to
control our memory accesses.

Ultimately, it's the class of functions in OpenSSL et al. that deal
with secret data that are called "constant-time". That's the
extent of its meaning. The problem with this optimization, though, is
called "data dependency"—its logic depends on secret data.

## How Exactly?

Doing this would require two precise attack vectors, but just knowing
it's possible is enough to avoid it altogether.

1. **Flush and reload** to know the victim process is currently doing
   an RSA signature by flushing the cache in the attacking process
   which has the shared library loaded. The attacker can do this
   because it knows which parts of the program are back in the cache
   because the library is shared.
1. Then a **prime and probe** attack which does the opposite. The
   attacking process fills the cache and then tracks which lines are
   evicted. Through this it records, in order, the distance the
   pointer moves during an RSA signature, which it knows is happening
   because of the flush and reload attack. This can then be used to
   decode the private exponent.
   
# IV. As It Turns Out

So, as it turns out, that original implementation that walks the table
end-to-end and uses `vpblend` to copy the right data out was on
purpose! It has to be done this way so memory is always accessed the
same way regardless of what the indices are, preventing the leaking of
any secret information. This is what I get for thinking I was onto something!
