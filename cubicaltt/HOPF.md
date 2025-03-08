[![Build Status](https://travis-ci.org/groupoid/hopf.svg?branch=master)](https://travis-ci.org/groupoid/hopf)

Groupoid Infinity
=================

The Groupoid Infinity Cubical Base Library is compatible with `hcomptrans` branch of `cubicaltt` that fully supports recursive HITs. As example Hopf fibration is given formally due to Guillaume Brunerie:

```
rot: (x : S1) -> Path S1 x x = split
  base -> loop1
  loop @ i -> constSquare S1 base loop1 @ i

mu : S1 -> equiv S1 S1 = split
  base -> idEquiv S1
  loop @ i -> equivPath S1 S1 (idEquiv S1)
        (idEquiv S1) ( \(x : S1) -> rot x @ j) @ i

H : S2 -> U = split
  north -> S1
  south -> S1
  merid x @ i -> ua S1 S1 (mu x) @ i

total : U = (c : S2) * H c
```

Credits
-------

* Maxim Sokhatsky

