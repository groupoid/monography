open import Agda.Primitive

mutual
  data V : Set₁ where
    pi_ : (x : V) → (Elv x → V) → V
    uni_ : (f : (x : V) → (Elv x → V) → V)
           (g : (x : V) → (y : Elv x → V) → (Elv (f x y) → V) → V) → V

  Elv : V → Set₁  -- Changed from Set to Set₁
  Elv (pi_ a b) = (x : Elv a) → Elv (b x)
  Elv (uni_ f g) = Universe f g

  data Universe (F : (x : V) → (Elv x → V) → V) (G : (x : V) → (y : Elv x → V) → (Elv (F x y) → V) → V)
       : Set₁ where  -- Changed to Set₁
    fun_ : (x : Universe F G) → (V → Universe F G) → Universe F G
    f_   : (x : Universe F G) → (V → Universe F G) → Universe F G
    g_   : (x : Universe F G) (y : Elv (Elt F G x) → V)
           (z : Elv (F (Elt F G x) (λ (a : Elv (Elt F G x)) → y a))) → Universe F G

  data Elt (f : (x : V) → (Elv x → V) → V) (g : (x : V) → (y : Elv x → V) → (Elv (f x y) → V) → V)
       : Universe f g → Set₁ where  -- Changed to Set₁
    elt : {u : Universe f g} → V → Elt f g u
