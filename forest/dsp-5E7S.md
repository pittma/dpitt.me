---
title: Message Digest V
published: 2024-05-01
tagged: true
math: true
tags: math, cryptography
---

md5 replaces, you guessed it, md4—the series of which was created by
Ronald Rivest, who is the R in [RSA](/forest/dsp-9F0P.html).
  
# Algorithm

Let's say our message is $$M$$ whose length is $$l$$.

1. First $$M$$ is divided into 512-bit blocks. 
1. Pad the final block, first with a single `1`, then `0`'s until the
   final block's length is 448 bits ($$512 - 448$$), the last 64 bits
   are used to contain $$l\ \text{mod}\ 2^{64}$$.
1. Initialize 4 32-bit values (these constants come directly from the
   [RFC that specifies
   MD5](https://www.rfc-editor.org/rfc/rfc1321#section-3.3))[^rust].

   [^rust]: I'm going to use Rust here instead of Haskell because bit
       twiddling in Haskell is no fun at all.
   
   ```rust
   const A: [u8; 4] = [0x01, 0x23, 0x45, 0x67];
   const B: [u8; 4] = [0x89, 0xab, 0xcd, 0xef];
   const C: [u8; 4] = [0xfe, 0xdc, 0xba, 0x98];
   const D: [u8; 4] = [0x76, 0x54, 0x32, 0x10];
   ```

   Wikipedia actually has these buffers as 32-bit hex values, in
   little endian form, e.g. `A = 0x67452301`. We can of course get
   there from here with a little bit fiddling:
   
   ```rust
   let mut a: u32 = A[0].into();
   a |= (A[1] as u32) << 8; 
   a |= (A[2] as u32) << 16; 
   a |= (A[3] as u32) << 24; 
   ```
1. Next we define 4 functions 

   ```rust
   // F(X,Y,Z) = XY v not(X) Z
   fn f(x: u32, y: u32, z: u32) -> u32 {
       (x & y) | (!x & z)
   }
   
   // G(X,Y,Z) = XZ v Y not(Z)
   fn g(x: u32, y: u32, z: u32) -> u32 {
       (x & z) | (y & !z)
   }
   
   // H(X,Y,Z) = X xor Y xor Z
   fn h(x: u32, y: u32, z: u32) -> u32 {
       x ^ y ^ z
   }
   
   // I(X,Y,Z) = Y xor (X v not(Z))
   fn i(x: u32, y: u32, z: u32) -> u32 {
       y ^ (x | !z)
   }
   ```

   And another constant[^const], a set $$T$$, where $$T_i = 2^{32} *
   |\textbf{sin}(i)|$$ and $$i = [1\ldots64]$$.
   
   [^const]: Wikipedia actually contains the precomputed table for $$T$$, I'd
       imagine that most real-life implementations do the same.

1. There is then 4 rounds of several permutations between 512-bit
   chunks of the payload, 32-bits at a time, members of the $$T$$ set,
   and $$A$$, $$B$$, $$C$$, and $$D$$, where the results of those
   rounds are accumulated in those variables; it's all too long to
   type out here, to see the details of these rounds, click
   [here](https://www.ietf.org/rfc/rfc1321.html#section-3.4).
   
1. The final result is the concatenation of those 32-bit variables a
   into 128-bit hexadecimal value.
  
I have a non-optimized (both in terms of performance and in terms of
the repetitive nature of handwriting an MD5 implementation)
implementation in rust
[here](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=faf376ada0822e3b54c40a6c20858f38),
if you want to see the whole thing end-to-end.

# Weird things I've come across so far

## Not `(x & y) | (!x & z)` actually

$$F$$, the function used in the first round, does the following: $$(x
\land y) \lor (\neg x \land z)$$. At least…on paper. Implementations
typically do this instead: $$z \oplus (x \land (y \oplus z))$$. And it
turns out [these are the same
thing](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=b32a0ed73a1b848d416863748cf5b5ee).

The cool thing about the latter is that it's only 3 operations---2 XORs
and an AND---but also that it's right-associative. It's a fold. This
means that the second set of operations can accumulate into a single
register, while with the former needs a register for the LHS of the
outer OR, and one for the RHS, after which an OR can be applied to the
two.

# AVX-512-ifying this

There are multiple lines of thinking to be done here, I'll start with
the simplest.

## `vpternlog`

The instruction `vpternlog[d|q]` is a SIMD instruction
that can do ternary operations on data in its operands. Intel has also
been so kind as to provide implementations for the round functions
above ($$F \ldots I$$), which are delineated by passing `vpternlog` one
of these constants:

| Function | Expression | Constant |
| - | - | - |
| $$F$$ | $$(x \land y) \lor (\neg x \land z)$$ | `0xca` |
| $$G$$ | $$(x \land z) \lor (y \land \neg z)$$ | `0xe4` |
| $$H$$ | $$x \oplus y \oplus z$$ | `0x96` |
| $$I$$ | $$y \oplus (x \lor z)$$ | `0x39` |

So essentially what we need to do is load up `x`, `y`, and `z` into
vectors and let `vpternlog` go at them to do these operations for us.

## Current Implementation

Here's the round 1 function.

```perl
# round1_step() does:
#   dst = x + ((dst + F(x,y,z) + X[k] + T_i) <<< s)
#   %r10d = X[k_next]
#   %r11d = z' (copy of z for the next step)
# Each round1_step() takes about 5.3 clocks (9 instructions, 1.7 IPC)
sub round1_step
{
    my ($$pos, $$dst, $$x, $$y, $$z, $$k_next, $$T_i, $$s) = @_;
    $$code .= " mov	0*4(%rsi),	%r10d		/* (NEXT STEP) X[0] */\n" if ($$pos == -1);
    $$code .= " mov	%edx,		%r11d		/* (NEXT STEP) z' = %edx */\n" if ($$pos == -1);
    $$code .= <<EOF;
	xor	$$y,		%r11d		/* y ^ ... */
	lea	$$T_i($$dst,%r10d),$$dst		/* Const + dst + ... */
	and	$$x,		%r11d		/* x & ... */
	xor	$$z,		%r11d		/* z ^ ... */
	mov	$$k_next*4(%rsi),%r10d		/* (NEXT STEP) X[$$k_next] */
	add	%r11d,		$$dst		/* dst += ... */
	rol	\$$$$s,		$$dst		/* dst <<< s */
	mov	$$y,		%r11d		/* (NEXT STEP) z' = $$y */
	add	$$x,		$$dst		/* dst += x */
EOF
}
```

$$F$$ is found in the lines (recall that it's implemented as $$z
\oplus (x \land (y \oplus z))$$).

```
	xor	$$y,		%r11d		/* y ^ ... */
	and	$$x,		%r11d		/* x & ... */
	xor	$$z,		%r11d		/* z ^ ... */
```

## AVX-512 Implementation Ideas

### Basic `vpternlog` impl

An AVX-512 implementation would use the vector registers for `A`, `B`,
`C`, and `D`, as opposed to `%r11d`, mask out everything but the
bottom 32 bits, and then, I guess, use `vadd` instructions for the
additions?

### Parallel additions

These inner three additions `(dst + F(x,y,z) + X[k] + T_i)` could
theoretically be done in parallel, if we can get the vectors set up in
few enough instructions to make it worth doing this in parallel.
