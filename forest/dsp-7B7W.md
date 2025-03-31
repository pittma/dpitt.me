---
title: Vectorized XTS
published: 2025-03-14
tagged: true
tags: cryptography, assembly
garage: open
related: dsp-6K6O
---

So first the tweak is encrypted and written to the stack.

## Tweak Generation

```perl
# load the initial tweak / iv in to zmm0
vbroadcasti32x4 	 ($$TW),%zmm0
vbroadcasti32x4 shufb_15_7(%rip),%zmm8
mov 	 \$$0xaa,$$tmp1
kmovq 	 $$tmp1,%k2

# Because of the mask loaded into zmm8, this is taking the MSB from each 64-bit
# half of each tweak in the vector and placing it in the LSB position.
vpshufb 	 %zmm8,%zmm0,%zmm1

# The broadcasted tweaks are variably shifted left, by 0, 1, 2, or 3. This is
# the tweak multiplication step.
vpsllvq const_dq3210(%rip),%zmm0,%zmm4

# Those MSB to LSB bytes are then variably shifted _right_ by 8, 7, 6, 5;
# extracting (i.e., leaving only the) carry bits from each sequential tweak.
vpsrlvq const_dq5678(%rip),%zmm1,%zmm2

# The carry bits are then multiplied, carry-lessly, by the modulus of the GF
# (0x87) held in $$ZPOLY.
vpclmulqdq 	 \$$0x0,$$ZPOLY,%zmm2,%zmm3
# zmm2 has the carried bits, and zmm4 has the multiplied/left
# shifted tweaks. Here we are selectively xor-ing every other 64-bit lane; the
# lsb of each shifted tweak with carry bits from each shifted tweak. They should
# match—we save the msb, shift variably, then xor the msb's carried bits back in
# at the bottom of the tweak where the number of bits matches, this is a literal
# wrap around.
vpxorq 	 %zmm2,%zmm4,%zmm4{%k2}
# now we xor the corrected carries in, if the carries were 0, this has no effect.
# zmm9 now holds the first 4 tweaks.
vpxord 	 %zmm4,%zmm3,%zmm9

# Here we do the same routine a second time, but the shifts are adjusted to
# generate the next set from the same basis, zmm0, or the initial tweak.
vpsllvq const_dq7654(%rip),%zmm0,%zmm5
vpsrlvq const_dq1234(%rip),%zmm1,%zmm6
vpclmulqdq 	 \$$0x0,%zmm25,%zmm6,%zmm7
vpxorq 	 %zmm6,%zmm5,%zmm5{%k2}
vpxord 	 %zmm5,%zmm7,%zmm10

# Now that we have two sets, tweak generation is simpler, rather than beginning
# from the initial tweak, we start from two generations back and tweak
# generation is as simple as wrapping the msb around to the lsb, correcting the
# new lsb, and zipping it into the shifted tweak.
vpsrldq 	 \$$0xf,%zmm9,%zmm13
vpclmulqdq 	 \$$0x0,%zmm25,%zmm13,%zmm14
vpslldq 	 \$$0x1,%zmm9,%zmm11
vpxord 	 %zmm14,%zmm11,%zmm11

vpsrldq 	 \$$0xf,%zmm10,%zmm15
vpclmulqdq 	 \$$0x0,%zmm25,%zmm15,%zmm16
vpslldq 	 \$$0x1,%zmm10,%zmm12
vpxord 	 %zmm16,%zmm12,%zmm12
```

The reason we need a generation gap is because, once we have a gap,
each tweak is just 1 byte left shifted of the previous set, when we're
working from only a single generation back, we have to do the variable
shift. Either by 0/8 1/7 2/6 3/5 and then 1/7 2/6 3/5 4/4, both from
the basis of zmm0, or the same 0/8… route could be taken with zmm9 as
the basis for the second round.

There are performance reasons to use zmm0 a second time, the biggest
being that if we were to use zmm9 for the second basis, it chains the
computation of tweaks 4-7 to the completion of tweaks 0-3. If they
both compute independently off of zmm0, they can be parallelized via
**ILP (instruction-level parallelization)**.

:::{.insight}
Parallel tweak generation should be reusable, it's exactly the same in
both directions. Maybe see if there is a way reuse a 8-tweak
generation routine.
:::

One way to think about the `encrypt_by` implementations is that they
_flatten_ the AES to XTS relationship. See below where the xor step
from XTS is inline with the steps of an AES round:

```perl
$$code .= <<___;
# xor Tweak values
vpxorq    $$tw1, $$st1, $$st1
vpxorq    $$tw2, $$st2, $$st2

# ARK
vbroadcasti32x4 0x80($$TW), $$t0
vpxorq    $$t0, $$st1, $$st1
vpxorq    $$t0, $$st2, $$st2
___

if (0 == $$last_eight) {
  $$code .= <<___;
  vpsrldq		\$$0xf, $$tw1, %zmm13
  vpclmulqdq	\$$0x0, $$ZPOLY, %zmm13, %zmm14
  vpslldq		\$$0x1, $$tw1, %zmm15
  vpxord		%zmm14, %zmm15, %zmm15
___
}
# round 1
$$code .= <<___;
vbroadcasti32x4 0x90($$TW), $$t0
vaesenc  $$t0, $$st1, $$st1
vaesenc  $$t0, $$st2, $$st2
#...
```

The `if (0 == $$last_eight)` checks are generating the next
tweaks. Interspersing these this way allows for tweak generation to be
parallelized with the AES rounds because of OoO execution. Even if a
program is pinned to a single logical core, OoOE is still applicable
because there are 4 execution pipelines per physical core:

> The Front-end of the pipeline on recent Intel microarchitectures can
> allocate four µOps per cycle, while the Back-end can retire four
> µOps per cycle. From these capabilities the abstract concept of a
> pipeline slot can be derived. A pipeline slot represents the
> hardware resources needed to process one µOp. The Top-Down
> Characterization assumes that for each CPU core, on each clock
> cycle, there are four pipeline slots available.

:::{.quote-ref}
[Intel TMA](https://www.intel.com/content/www/us/en/docs/vtune-profiler/cookbook/2023-0/top-down-microarchitecture-analysis-method.html)
:::

This works via the prefetcher, which can determine data dependencies.

### Generating a single tweak

```perl
vmovdqa	shufb_15_7(%rip),%xmm11
mov 	 \$$0xaa,$$tmp1
kmovq 	 $$tmp1,%k2
vpshufb 	 %xmm11,%xmm0,%xmm12
vpsllq	\$$0x1,%xmm0,%xmm13
vpsrlq \$$0xf,%xmm12,%xmm14
vpclmulqdq 	 \$$0x0,%xmm25,%xmm14,%xmm15 # just the first lane of ZPOLY
vpxorq 	 %xmm13,%xmm14,%xmm13{%k2}
vpxord 	 %xmm13,%xmm15,%xmm15
```
