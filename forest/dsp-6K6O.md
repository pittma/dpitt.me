---
title: XTS
published: 2025-03-12
math: true
tagged: true
tags: cryptography
garage: open
---

**IEEE 1619**

---

**Things to Remember, that I keep forgetting:**

* A **Data Unit** can be many 128-bit blocks (the standard says up to
$$2^{20}$$ to be precise), but a single **Data Unit** has a single
**Tweak**.

<img src="/images/Screenshot 2025-02-12 at 11.19.42 AM.png">

* ~~The **Data Unit** number is an input into the algorithm (where
  does this happen in the assembly??).~~ It doesn't. See below.

:::{.insight}
The standard generates the tweak from the sector / **Data Unit**
Number, but OpenSSL does not do this, it treats the Tweak as a random
nonce/iv and encrypts it via `$$key2`. I think the insight here is that it
makes OpenSSL's implementation more general purpose by making the
initial tweak independent of key 2.

**The implication of this is that a call to an XTS cipher in OpenSSL
et al. is always on a single Data Unit.**
:::

## Cipher Stealing
1. Steal enough bytes from the last complete cipher text block for our
   remaining bytes to make up a complete AES plaintext block.
2. Encrypt this block.
3. The last complete cipher text block is _replaced_ with this new,
   now complete, block.
4. the _unstolen_ bytes from the previously last complete block are
   then tacked on to the end as the remainder.
