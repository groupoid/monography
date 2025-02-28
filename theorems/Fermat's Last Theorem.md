Number Theory: Fermat’s Last Theorem
====================================

Theorem: For integers 𝑛 > 2, there are no positive integers 𝑎,𝑏,𝑐 such that 𝑎ⁿ + 𝑏ⁿ = 𝑐ⁿ.

⊢ ∀ 𝑎 𝑏 𝑐 𝑛 : ℕ, 𝑎 > 0, 𝑏 > 0, 𝑐 > 0, 𝑛 > 2 → ¬(𝑎ⁿ + 𝑏ⁿ = 𝑐ⁿ)

Type Formers:

* Use ℕ,Π,Σ,Id from Anders.
* R: Type: Reals (for integers via embedding), (L,U): R: Dedekind cuts (though we’ll use 𝑍) from Julius.
* V: Type: Cumulative hierarchy for sets (e.g., elliptic curves), Z:V (integers as a Set), LEM:P∨¬P: Classical logic (used in Wiles’ proof) from Ernst.
* Group,Ring: Type: Algebraic structures, Simplicial: Type: Simplicial sets for modular forms and stacks from Dan.
* G→A:Type: Group actions (e.g., Galois groups), ∣ʃA: Type: Homotopy types from Urs.

Encoding:

* Z:V (Julius), defined as Z={(m,n):N×N}/∼, where (m,n)∼(p,q) if m+q=n+p.
* Z+ ={z:Z∣z>0}:V (Julius).
* ⊢ a:Z,n:N ⊢ pow(a,n):Z (exponentiation 𝑎ⁿ): pow : ℕ → ℕ → ℤ.
* Statement: Γ ⊢ ∀ a b c n : ℕ, a > 0, b > 0, c > 0, n > 2 → ¬(pow a n + pow b n = pow c n) : Prop.
* ⊢ Fermat(n):Prop=Σ(a:Z+).Σ(c:Z+).Id_Z(pow(a,n)+pow(b,n),pow(c,n)).
* Goal: ⊢ Π(𝑛:𝑁).(𝑛>2) → ¬Fermat(n), where ¬𝐴 ≡ 𝐴 → ∅.

Proof Sketch:

* Wiles’ proof uses elliptic curves, modular forms, and Galois representations.
* Define elliptic curves in V (ZFC type) as sets of points satisfying 𝑦2=𝑥3+𝑎𝑥+𝑏.
* Use group actions (𝔾 → A) from Super Type Theory for Galois groups.
* Encode modular forms via simplicial sets (e.g., modular curves as simplicial objects).
* Requires LEM for classical steps (e.g., reductio ad absurdum).

Lemmas:

* Elliptic Curves (a stack over 𝑄 via Dan Kan).
* Frey Curve Semistable, minimal discriminant Δ=2^8(𝑎𝑏𝑐)^{2n}.
* Modular Forms (a moduli stack of elliptic curves with level structure, Hecke algebra acting on modular forms).
* Galois Representations (ρ_E : G_Q → GL_2(Z_p)) (via group action in Equivariant Super Type Theory).
* Ribet-Serre Theorem.
* Modularity Theorem.

