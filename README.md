AXIO/1
======

Artificial Experienced Intelligence Ontology Version 10.0 (2025).

## TOC

* Intro
* Process
* Components
* Operators
* Refinement Check
* Design Goals
* Runtime Lamguages
* Higher Languages

## Intro

AXIO/1 Framework is a layered system for infinite reasoning, split into Runtime Languages (for execution)
and Higher Languages (for theorem proving), each with specialized roles. This is a cyclic, iterative system
for formal reasoning, where an operator (a human thinker, AI, or hybrid) drives a process that:

## Components

* Starting Points (Axioms, Definitions, Types, Propositions, Syntax).
* Environment (Model, Consistency, Completeness, Library).
* Thinking (Hypotheses (Presupposition, Assumption, Supposition), Computation (Lambda Calculus, Pi-Calculus), Deduction (Inference, General Induction), Conjecture/Assertion).
* Conclusions (Proof, Judgement, Theorem).
* Insight (Semantics, Categorical Frameworks, Abstraction).

## Process

* Begins with foundational elements (Starting Points).
* Operates within a structured setting (Environment).
* Engages in reasoning (Thinking).
* Produces results (Conclusions).
* Gains understanding (Insight).
* Loops back to refine and expand, infinitely.

Starting Points → Environment → Thinking → Conclusions → Insight → Cycle back — feels organic.
It’s like a scientific method for formal thought: set rules, test them, reason, conclude, learn, repeat.
Concurrency could mean multiple threads (human or machine) tackling different conjectures, syncing via the library.

## Operators

Human: Chooses propositions, interprets insights, guides conjectures.
Machine: Automates computation (e.g., Lambda for functions), checks consistency, suggests hypotheses from patterns in Library.
Hybrid: Human sets goals, machine crunches steps.

## Refinement Check

Infinite Thinking: Achieved via cycles 𝑛 → ∞.
Finite Steps: Each 𝑆𝑛 → 𝑆𝑛+1 is discrete.
Operator-Driven: 𝑂 controls direction and pace.

## Design Goals

Runtime Languages: Handle computation and concurrency (finite steps of 𝑇𝑛 in our model).
Higher Languages: Prove theorems and ensure soundness.
Infinite Thinking: Runtime executes cycles, higher languages refine.
Operator-Driven: Languages enable human/machine collaboration.

## Runtime Languages

### Joe

Role: Certified bytecode stack interpreter and compiler to Intel/ARM.
Features: Executes Lambda Calculus terms as bytecode, compiles to native code.
Fit: Computes concrete results. Certified for reliability.
Use Case: Operator runs algebraic steps or tests hypotheses on hardware.

### Bob

Role: Parallel, concurrent, non-blocking, zero-copy runtime with CAS cursors (compare-and-swap).
Features: Implements Pi-Calculus-style concurrency, optimized for matrix operations (BLAS-like).
Fit: Manages distributed validation across nodes, computes in parallel (e.g., parity table cases).
Use Case: Operator coordinates multi-threaded proof checks or simulations.

### Alice

Role: Linear types calculus with partial fractions for BLAS level 3 programming.
Features: Ensures resource safety (linear types), optimizes matrix computations (e.g., tensor products).
Fit: Handles complex (e.g., matrix-based proofs), enforces no redundant copies.
Use Case: Operator proves theorems involving linear algebra or tensor structures.

## Higher Languages

### Henk

Role: Pure Type System (PTS-91), Calculus of Inductive Constructions (CoC-88), infinite universes, AUTOMATH syntax.
Features: Flexible typing.
Use Case: Operator formalizes recursive or foundational proofs.
Rationale: Henk subsumes Alonzo’s STLC with richer types, making it a strong starting point.

### Per

Role: ΠΣ (MLTT-72) prover with CoC, identity types (MLTT-75), extended to CIC (IND-89).
Features: Dependent types, equality proofs.
Fit: Proves (e.g., "parity preservation"), ensures consistency.
Use Case: Operator handles equality or model-specific theorems.

### Anders

Role: Homotopy Type System (HTS-2013) with Strict Equality and Cubical Agda (CCHM-2016).
Features: Higher-dimensional types, paths, cubical primitives.
Fit: Extracts (e.g., "parity as a homotopy group"), builds cat.
Use Case: Operator abstracts to categorical or topological structures.

### Dan

Role: Simplicial CCHM-based system, replacing Rzk/GAP.
Features: Simplicial types, primitives (Simplex, Chain, Monoid, Category, Group).
Fit: Formalizes cat (e.g., "parity as a monoid"), verifies geometric proofs.
Use Case: Operator proves simplicial or algebraic topology insights.

### Élie

Role: CCHM-based system with Hopf Primitives.
Features: Hopf algebras (e.g., co-multiplication), topological type checking.
Fit: Deepens (e.g., "parity in Hopf structures"), proves advanced Abstractions.
Use Case: Operator links proofs to topological or physical systems.

## Monography

Compilation: https://axiosis.github.io/books/axio/axio.pdf

## LaTeX 

```
$ cp *.ttf ~/.local/share/fonts
$ sudo apt install texlive-full
$ sudo fc-cache -f
$ fc-match Geometria
$ make
```

