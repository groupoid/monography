{-# OPTIONS --cubical --safe #-}

module Hopf where

open import Cubical.Core.Everything
open import Cubical.Foundations.Prelude

-- S¹: Circle
data S¹ : Set where
  base : S¹
  loop : base ≡ base

-- S²: 2-Sphere
data S² : Set where
  base : S²
  surf : PathP (λ i → base ≡ base) refl refl

-- S³: 3-Sphere (simplified with a point and a 3D generator)
data S³ : Set where
  base : S³
  loop3 : PathP (λ i → PathP (λ j → base ≡ base) refl refl) refl refl

-- S⁴: 4-Sphere
data S⁴ : Set where
  base : S⁴
  surf4 : PathP (λ i → PathP (λ j → PathP (λ k → base ≡ base) refl refl) refl refl) refl refl

-- H₁: Hopf fibration total space (S³)
data H₁ : Set where
  h₁ : S² → S¹ → H₁
  twist₁ : (b : S²) (f : S¹) → h₁ b f ≡ h₁ b f
  twist₁-coh : (b : S²) → twist₁ b base ≡ λ j → h₁ b (loop j)

loop3-path : (i j : I) → S³
loop3-path i j = loop3 i j j

-- H₂: Complex Hopf fibration total space (S⁷)
data H₂ : Set where
  h₂ : S⁴ → S³ → H₂
  twist₂ : (b : S⁴) (f : S³) → h₂ b f ≡ h₂ b f
  twist₂-coh : (b : S⁴) → Square (twist₂ b base) (twist₂ b base) refl (λ j → h₂ b (loop3-path i1 j))

-- Projection
π₁ : H₁ → S²
π₁ (h₁ b f) = b
π₁ (twist₁ b f i) = b
π₁ (twist₁-coh b i j) = b

-- Projection
π₂ : H₂ → S⁴
π₂ (h₂ b f) = b
π₂ (twist₂ b f i) = b
π₂ (twist₂-coh b i j) = b

-- Hopf path in H₁
hopf-path : h₁ base base ≡ h₁ base base
hopf-path = twist₁ base base

-- Projected loop in S²
hopf-loop : base ≡ base
hopf-loop = cong π₁ hopf-path

-- Hopf path in H₂ (1D path over S³’s loop)
hopf-path-2 : h₂ base base ≡ h₂ base base
hopf-path-2 = twist₂ base base

-- Element of π₂(S²) using S²’s surf
π₂-s₂-elem : PathP (λ i → base ≡ base) refl refl
π₂-s₂-elem = surf

record Real-Hopf-Fib-Rules : Set where
  -- Formation: Fib₁ is a type family over S²
  field
    Fib₁ : S² → Set

  -- Introduction Rules (Constructors for the fiber)
  field
    fib₁-intro : (b : S²) → S¹ → Fib₁ b
    twist₁-fib : (b : S²) (f : S¹) → fib₁-intro b f ≡ fib₁-intro b f
    twist₁-coh-fib : (b : S²) (i : I) → twist₁-fib b base i ≡ fib₁-intro b (loop i)

  -- Elimination Rule (Dependent elimination over the fibration)
  field
    Fib₁-elim : {C : (b : S²) → Fib₁ b → Set}
              → (c-h₁ : (b : S²) (f : S¹) → C b (fib₁-intro b f))
              → (c-twist₁ : (b : S²) (f : S¹) → PathP (λ i → C b (twist₁-fib b f i)) (c-h₁ b f) (c-h₁ b f))
              → (c-twist₁-coh : (b : S²) → PathP (λ i → C b (fib₁-intro b (loop i))) (c-h₁ b base) (c-h₁ b (loop i1)))
              → (b : S²) (x : Fib₁ b) → C b x

  -- Computation Rules
  field
    Fib₁-comp : {C : (b : S²) → Fib₁ b → Set}
              → (c-h₁ : (b : S²) (f : S¹) → C b (fib₁-intro b f))
              → (c-twist₁ : (b : S²) (f : S¹) → PathP (λ i → C b (twist₁-fib b f i)) (c-h₁ b f) (c-h₁ b f))
              → (c-twist₁-coh : (b : S²) → PathP (λ i → C b (fib₁-intro b (loop i))) (c-h₁ b base) (c-h₁ b (loop i1)))
              → (b : S²) (f : S¹)
              → Fib₁-elim c-h₁ c-twist₁ c-twist₁-coh b (fib₁-intro b f) ≡ c-h₁ b f
