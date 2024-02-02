---
title: Zeros, Stacks, and Paranoia
subtitle: A story about stacking keys and how to forget them.
published: 2024-01-12
tagged: true
tags: cryptography, programming, assembly, boringssl
---

_Boring and OpenSSL have a lot of “hardware”
implementations. Handwritten assembly routines for various algorithms
and modes, targeting specific architectures and features. If you were
to hazard a guess, would you say that used stack frames were typically
explicitly zeroed, or just “discarded” (by resetting the stack
pointer)?_

---

At the time of this writing, I work at Intel. My current job has me
porting hard things from one place to another, typically to
[AWS-LC](https://github.com/aws/aws-lc), as my team is dedicated to
co-engineering work with the folks at AWS. I'm not one of the people
who intrinsically just knows how to do hard things (yet), but I am one
of the people who can spend some time with an already done hard thing,
learn how it works, and then can contribute to it, translate it, or
whatever it is that needs to be done.

Most recently, we've been looking at the AVX-512 hardware
implementation for XTS[^xts]. The question was, "where is the stack
_cleared_?"

[^xts]: XOR-Encrypt-XOR Tweakable Block Cipher Stealing mode; an AES
    mode typically used for disk encryption.

We can see that, at the beginning of the encryption routine, the
incoming [stack pointer is saved into
`rbp`](https://github.com/aws/aws-lc/blob/d9caacae8c44227baa41e970cbed0dbfd4eef9c2/crypto/fipsmodule/aes/asm/aesni-xts-avx512.pl#L1476-L1480).

```perl
  $$code .= "push         %rbp\n";
  $$code .= "mov          %rsp,%rbp\n";
  $$code .= "sub          \$$VARIABLE_OFFSET,%rsp\n";
  $$code .= "and          \$$0xffffffffffffffc0,%rsp\n";
  $$code .= "mov          %rbx,$$GP_STORAGE($$TW)\n";
```

Then, in the routine labeled `ret`, [that pointer is restored to
`rsp`](https://github.com/aws/aws-lc/blob/d9caacae8c44227baa41e970cbed0dbfd4eef9c2/crypto/fipsmodule/aes/asm/aesni-xts-avx512.pl#L1855-L1859). `ret`
is a routine that is jumped to from several places in the main
encryption routine upon completion. Searching for the label in its
entirety (`L_ret_$${rndsuffix}`) in this file turns up 27
results[^vz].

[^vz]: `vzeroupper` is a one-pass no-operand instruction that zeros the
    upper (i.e. most significant) half of the vector registers. This is
    a good start.


```perl
  $$code .= <<___;
  mov    %rbp,%rsp
  pop    %rbp
  vzeroupper
  ret
```

This effectively “forgets” the frames used during the encryption
routine, but the bits and bytes written to those frames remain until
the stack grows back into them. And to make matters worse, in this
case, it's key material that is written into these frames. This of
course could leave a vector of attack open for a malicious but
intelligent attacker.

<div class="pull">
What could be more in need of paranoia than a `libcrypto`
implementation?
</div>

When I traded emails with the original author of this XTS code—recall
that I'm a porter not a knower—his response was that “a very paranoid
implementation” may zero the used frames before discarding them. While
this is a fair response out of context, what could be more in need of
paranoia than a `libcrypto` implementation?

So yeah, we need to zero the stack here too. Given this is all a part
of an AVX-512 implementation, we can make clever use of the SIMD
instructions and vector registers to clear those frames, adding the
following lines to this `ret` routine.

```perl
vpxor        %zmm0,%zmm0,%zmm0
# Zero-out the stack frames used for `key1`, 64 bytes at a time.
vmovdqa64    %zmm0,0x80(%rsp)
vmovdqa64    %zmm0,0xc0(%rsp)
vmovdqa64    %zmm0,0x100(%rsp)

# Stack usage is not divisible by 64, so we use a kmask register to
# only mov 48 of the bytes (6 quad-words).
mov       \$$0x3f,$$tmp1
kmovq     $$tmp1,%k2
vmovdqa64 %zmm0,0x140(%rsp){%k2}
```

`vmovdqa64` moves 512-bits (64 bytes) from the full-width SIMD
register `zmm0` to the location we specify with `<offset>(%rsp)` where
those 64 bytes are thought of logically as 64-bit integers, AKA
quad-words. Before those lines we self-exclusive-or `zmm0`. A
self-exclusive-or's result is always `0` because none of the bits are
exclusive when they're or'd with themselves. After that, we write
zeros, 64 _bytes_ at a time to the stack. The stack usage here isn't
evenly divisible by 64, so we use a mask register to tell this last
instruction to only use the first 48 bytes of the register when doing
its move. This is achieved by writing `0x3f`, or written another way
`0b_11_1111` (6 ones), into the mask register and then including that
mask register with our invocation.

There were a few other places that needed some zeroing attention, but
this is the most interesting part. And to answer the question I asked
in the front matter of this post, well, it's all of them. Or at least
it is now.
