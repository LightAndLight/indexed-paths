# `indexed-paths`

A library for working with 'free categories'.

## Contents

* What is this?
* Applications
    * State machines
    * Lensing

## What is this?

We're going to build some graphs. Suppose `k` is the kind of vertices in your
graph. If you you define a type `G :: k -> k -> Type`, then each value of type
`G a b` can be seen as an edge from `a :: k` to `b :: k`.

Example:

```haskell
{-

G1:

A ---W--> B
^         |
|         |
Z         X
|         |
|         v
D <--Y--- C

-}

data Node = A | B | C | D

data G1 :: Node -> Node -> Type where
  W :: G1 A B
  X :: G1 B C
  Y :: G1 C D
  Z :: G1 D A
```

The graphs we're defining are directed graphs that can have
more than one edge between any two vertices. These are known as
[quivers](https://en.wikipedia.org/wiki/Quiver_(mathematics)).

Every quiver defined in this manner gives rise to a category.
The objects of the category are vertices in the graph, and the
arrows of the category are sequences of graph edges, where the
target vertex of an edge matches the source node of the subsequent
edge in the sequence. Put simply, the arrows of the category are all
paths you can take by following edges of the graph. For each quiver `G`,
we'll call the category generated in this manner `Path(G)`.

Here's what `Path` looks like in Haskell:

```
data Path (g :: k -> k -> Type) :: k -> k -> Type where
  Nil :: Path g a a
  Cons :: g a b -> Path g b c -> Path g a c
```

It's literally a linked list of adjacent edges in the graph.

When it comes to representing sequences in programming, a linked list
is not always the best choice. There are a variety of sequence types with
operations that differ in space and time usage. `Path`s, being fundamentally
sequences, are also subject to the same considerations. This library defines
a few different representations, to allow efficient use of `Path`s in various
circumstances.

## Applications

### State machines

We can model state machines as quivers.

Example:

A door that can only be opened when it is unlocked,
and can only be locked when it is closed.

```haskell
{-

Locked ---Unlock--> Closed ---Open--> Opened
  ^                 |   ^                |
  |                 |   |                |
  +-------Lock------+   +-----Close------+

-}

data DoorS = Locked | Closed | Opened

data DoorG :: DoorS -> DoorS -> Type where
  Unlock :: DoorG Locked Closed
  Lock :: DoorG Closed Locked
  Open :: DoorG Closed Opened
  Close :: DoorG Opened Closed
```

`Path DoorG a b` represents a sequence of door actions that
start in state `a` and end in state `b`. We can then define the
door's runtime state, the action of an edge on that state,
and then use a sequence of door actions to affect that state.

```
data SDoorS :: DoorS -> Type where
  SLocked :: SDoorS Locked
  SClosed :: SDoorS Closed
  SOpened :: SDoorS Opened
  
data Door :: DoorS -> Type where
  Door :: { state :: SDoorS s, locks :: Int, unlocks :: Int, opens :: Int, closes :: Int } -> Door s
  
act :: DoorG a b -> Door a -> Door b
act edge door =
  case edge of
    Unlock ->
      door { state = SUnlocked, unlocks = unlocks door + 1 }
    Lock ->
      door { state = SLocked, locks = locks door + 1 }
    Open ->
      door { state = SOpened, opens = opens door + 1 }
    Close ->
      door { state = SClosed, closes = closes door + 1 }
      
actMany :: Path DoorG a b -> Door a -> Door b
actMany = composeMap act
```

### Lensing

Quivers of kind `Type -> Type -> Type` (quivers where the vertices are Haskell types),
can be used to describe the structure of datatypes.

Examples:

```haskell
data Pair a b = Pair a b

{-
Pair a b gives rise to the graph

    Pair a b
       / \
     Fst  Snd
     /     \
    v       v
    a       b

which can be encoded as the graph
-}

data PairG :: * -> * -> * where
  Fst :: PairG (Pair a b) a
  Snd :: PairG (Pair a b) b
```

```haskell
data Sum a b = Left a | Right b

{-

    Sum a b
       / \
      L   R
     /     \
    v       v
    a       b

-}

data SumG :: * -> * -> * where
  L :: SumG (Either a b) a
  R :: SumG (Either a b) b
```

```haskell
data List a = Nil | Cons a (List a)

{-

      List a <-+
      /  \     |
    Head  Tail-+
     |     
     v       
     a       

-}

data ListG :: * -> * -> * where
  Head :: ListG (List a) a
  Tail :: ListG (List a) (List a)
```

When a datatype-graph contains loops, such as `List`, `Path` can be used
to name items that are deeply nested in the datatype.

```haskell
{-

Cons 1 (Cons 2 (Cons 3 Nil))
         p1: ^
-}

p1 = Path.cons Tail (Path.singleton Head)


{-

Cons 1 (Cons 2 (Cons 3 Nil))
                 p2: ^
-}

p2 = Path.cons Tail (Path.cons Tail (Path.singleton Head))
```

TBC