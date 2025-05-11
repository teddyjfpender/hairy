# Welcome to the *Finite-Field Funhouse* 🎡  

Mathematics and computer science often feel like distant cousins, yet when they meet on the carnival grounds of modern cryptography the result can only be described as exhilarating.  This mini-repository puts you right in the front seat, hurtling through increasingly exotic finite fields—first a prime field, then a complex-looking quadratic extension, finally a dizzying quartic tower—while a purely functional language keeps every safety bar locked in place.  Buckle up: we are about to discover why fields matter, why *these* particular fields matter, and why Haskell is the secret sauce that turns abstract algebra into reliable, screaming-fast code.

---

## 1 Fields:  Tiny Universes with Huge Power  

A field is a set in which you may add, subtract, multiply, and divide (except by zero) and still remain inside the set.  The integers fail this cosmic test because division can fling you out into the rationals, but the set
\[
\mathbb F_{p} \;=\; \{\,0,1,\dots,p-1\,\},
\qquad p\text{ prime},
\]
passes with flying colours once every operation is taken modulo \(p\).  The prime we adopt is
\[
p \;=\; 2^{31}-1,
\]
a Mersenne celebrity whose binary expansion is just thirty-one ones in a row.  That special shape lets a CPU replace an expensive modulus instruction by a pair of shifts, an AND, and a conditional subtraction.  In other words, arithmetic in \(\mathbb F_{p}\) is practically *free*.

Many cryptographic protocols require more structure than a prime field affords, in particular roots of unity of composite orders.  You obtain those by stacking extensions much like you stack Lego bricks.  First adjoin a root of \(-1\) to create a quadratic field
\[
\mathbb F_{p^{2}} \;=\; \mathbb F_{p}[i]/(i^{2}+1).
\]
Then adjoin a square root \(u\) of \(r = 2+i\) to create the quartic field
\[
\mathbb F_{p^{4}} \;=\; \mathbb F_{p^{2}}[u]/(u^{2}-r).
\]
Suddenly twelfth and even twenty-fourth roots of unity pop into existence, opening the door to lightning-fast FFTs that are the beating heart of modern proof systems such as PLONK, STARKs, and FRI.

---

## 2 Why Haskell Is the Perfect Carnival Ride Operator  

Purely functional programming may sound like an ivory-tower endeavour until you need machine-level performance *and* mathematical certainty.  The secret is that Haskell’s `newtype` wraps a raw machine word in a compile-time promise without adding runtime overhead.  When you see a value of type `M31` you know, as surely as the compiler does, that the integer it conceals is already reduced modulo \(p\).  That invariant is impossible to violate from client code because the raw constructor is kept hidden behind a smart constructor `mkM31`.

Strictness annotations (`!`) and `INLINE` pragmas then coax the optimiser into producing tight loops indistinguishable from hand-written C—except there are no manual bounds checks to forget, no accidental timing leaks to introduce, no undefined behaviour lurking in corner cases.  Equations you write in the source code *are* the equations that execute, because laziness yields to strictness exactly where you mark it and nowhere else.  The result is a rare combination of mathematician-friendly reasoning and engineer-approved speed.

---

## 3 Meet the Cast of Modules  

**`Field.M31`** plays the role of the prime field \(\mathbb F_{p}\).  Each operation is a single machine instruction or, at worst, a handful when a reduction is needed.  Division invokes Fermat’s little theorem with a bespoke 37-multiplication addition chain that GHC happily unrolls for you.

**`Field.CM31`** introduces our enigmatic co-star \(i\).  Every element is a pair \((a,b)\) printed as \(\;a+bi\).  Multiplication follows the familiar complex-number formula, but each basic operation is itself performed in \(\mathbb F_{p}\), so everything remains constant-time.

**`Field.QM31`** throws in a mystery ingredient \(u\) satisfying \(u^{2}=2+i\).  The quartic field therefore has elements \((a,b)\) printed as \(\;a+bu\), where \(a,b\) live in the quadratic field.  The multiplication law encodes the relation \(u^{2}=r\) directly; inverses ride on top of quadratic inverses, echoing the tower structure.

Because each higher layer reuses the instances of the lower layers, the code resembles nested Russian dolls, each one thin, type-safe, and blindingly fast.

---

## 4 The Big Picture:  From Toy Example to Cutting-Edge Protocols  

The humble operations implemented here—addition, multiplication, inversion—form the arithmetic backbone of the **Poseidon** hash in SNARKs, the multiplicative cosets in **PLONK** commitments, the evaluation domains in **STARK** proof systems, and even the pairing engine of **BLS12-381** signatures.  Swap in a different modulus and every algorithm still works, but swap out the field abstraction and entire proofs crumble.

By building the tower in Haskell you gain three strategic advantages.  First, *correct-by-construction* codecs make serialization and deserialization trustworthy.  Second, equational reasoning lets you translate algebraic optimizations—Karatsuba multiplication, Montgomery ladders—directly into code with confidence.  Third, property-based testing via QuickCheck can stress the field axioms themselves, surfacing edge cases before they reach production.

---

## 5 Invitation to Play  

Clone the repository, open a GHCi session, and write expressions such as

```hs
let x = mkQM31 42 7 9 2020
let y = recip x
x * y
```


then watch the interpreter reveal the multiplicative identity.  Each line you type exercises thousands of modular operations, yet the runtime scarcely twitches.  Feel free to implement a mini-FFT or a toy Pedersen hash inside \(\mathbb F_{p^{4}}\); the scaffolding is already in place.

> **Pro tip.**  Turn on `-ddump-simpl` in GHC and read the Core to marvel at how your high-level equations melt into raw bit-twiddling.

---

## 6 Finale  

Finite fields are the unsung heroes that let digital signatures, zero-knowledge proofs, and homomorphic encryptions strut their stuff on the world stage.  Haskell gives these heroes impeccably tailored costumes that never snag on the scenery.  Together they deliver mathematical precision at silicon speed, all while leaving you, the developer-magician, free to focus on the grand illusion rather than the stage machinery.

<span style="color:mediumvioletred;">Welcome to the funhouse.  May your moduli be prime, your extensions smooth, and your functional code forever total.</span>
