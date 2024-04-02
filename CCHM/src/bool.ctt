{- Run-time Bool Type:
   - Bool type.
   - Theorems are in bool_theory module.
   Copyright (c) Groupoid Infinity, 2014-2018.

   HoTT 1.8 The type of booleans -}

module bool where
import proto

data bool = false | true

negation:    bool -> bool = split { false -> true ; true -> false }
or:  bool -> bool -> bool = split { false -> idfun bool ; true -> lambda bool bool true }
and: bool -> bool -> bool = split { false -> lambda bool bool false ; true -> idfun bool }

boolEq: bool -> bool -> bool = lambda bool (bool -> bool) negation
boolRec (A: U) (f t: A): bool -> A = split { false -> f ; true -> t }
boolInd (A:bool->U) (f:A false) (t: A true): (n:bool) -> A n = split { false  -> f ; true -> t }
