# `indexed-paths`

A library for working with 'free categories'.

A `Path` is a sequence of segments/layers of kind `k -> k -> *`, where
the 'output' type of each segment is matched with the 'input' type of
the following segment.

Another way to put it is: if types of kind `k` are nodes in a graph, and
then a value of type `l a b` is an edge between `a :: k` and `b :: k`. A
`Path` over `l :: k -> k -> *` names a particular path through the graph
defined by `l`.

e.g.

```

data Node = A | B | C | D

{-

G1:
    
A --X--> B --Y--> C
^                 |
|                 |
+--------Z--------+

-}
data G1 (a :: Node) :: Node -> * where
  X :: G1 'A 'B
  Y :: G1 'B 'C
  Z :: G1 'C 'A

{-

G2:

+----S---+
|        |  +--+
|        v /   |
A        B <---U
|        ^
|        |
+----T---+

-}
data G2 (a :: Node) :: Node -> * where
  S :: G2 'A 'B
  T :: G2 'A 'B
  U :: G2 'B 'B

```

When `k ~ *`, a `l :: * -> * -> *` can be used to describe graphs that are
implied by algebraic datatypes. 