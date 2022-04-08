# `call-alloy` [![Build Status](https://travis-ci.org/marcellussiegburg/call-alloy.svg?branch=master)](https://travis-ci.org/marcellussiegburg/call-alloy)

This is a simple library to call [Alloy](http://alloytools.org) given a specification.
This package includes a simple Java Library to make an API call to the Alloy Library.
Alloy is included (as JAR file) within this library as well.

## Requirements

- Java Runtime Environment:
  There is currently no warning if you have not set up any Java Runtime Environment.
  However, you will get runtime errors if it is not available when a call to Alloy happens.
  If you want to force a check, perform the test cases.

## Please note

The Java interface to get Alloy instances as well as the
[Alloy Jar](https://github.com/AlloyTools/org.alloytools.alloy/releases/download/v5.1.0/org.alloytools.alloy.dist.jar)
file are baked into this library.

On every call the application checks the [`XdgDirectory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#t:XdgDirectory) if the libraries exist in a current version.
If not they are placed there together with a version identifier.

## The library in action

This is a basic description on how to use the library.

### A specification example

Consider this Alloy specification of a simple Graph:

```Alloy
abstract sig Node {
  flow : Node -> lone Int,
  stored : one Int
} {
  stored >= 0
  all n : Node | some flow[n] implies flow[n] >= 0
  no flow[this]
}

fun currentFlow(x, y : one Node) : Int {
  let s = x.stored, f = x.flow[y] | s < f implies s else f
}

pred withFlow[x, y : one Node] {
  currentFlow[x, y] > 0
}

pred show {}

run withFlow for 3 Int, 2 Node
```

The graph is consisting of `Node`s, which might have some goods `stored` and may deliver them to other `Node`s (via `flow`).
`Node`s do not have `flow` to themselves.
The `currentFlow` is the minimum between the flow from the starting `Node` to the end `Node` and the currently `stored` goods at the starting `Node` (note: intermediate `Node`s are not allowed).
We call two `Nodes` `x` and `y` `withFlow` if `currentFlow` from `x` to `y` is greater than `0`.
We restrict our search to `3`-Bit signed `Int` values and `2` `Nodes`.

### An instance example

Calling Alloy using `getInstances` and the above program
could return the following (abbreviated) instance:

``` Haskell
[(Signature {
    scope = Nothing,
    sigName = "$withFlow_x"
    },
  Entry {
    annotation = Just Skolem,
    relation = fromList [
      ("",Single (fromList [Object {objSig = "Node", identifier = 1}]))
      ]
    }),
 (Signature {
    scope = Nothing,
    sigName = "$withFlow_y"
    },
  Entry {
    annotation = Just Skolem,
    relation = fromList [
      ("",Single (fromList [Object {objSig = "Node", identifier = 0}]))
      ]
    }),
 ...
 (Signature {
    scope = Just "this",
    sigName = "Node"
    },
  Entry {
    annotation = Nothing,
    relation = fromList [
      ("",Single (fromList [
        Object {objSig = "Node", identifier = 0},
        Object {objSig = "Node", identifier = 1}
        ])),
      ("flow",Triple (fromList [
        (Object {objSig = "Node", identifier = 0},Object {objSig = "Node", identifier = 1},NumberObject {number = 0}),
        (Object {objSig = "Node", identifier = 1},Object {objSig = "Node", identifier = 0},NumberObject {number = 3})
        ])),
      ("stored",Double (fromList [
        (Object {objSig = "Node", identifier = 0},NumberObject {number = 0}),
        (Object {objSig = "Node", identifier = 1},NumberObject {number = 1})
        ]))
      ]
    })
 ]
```

### A retrieval example

Using this library we may retrieve returned signature values using `lookupSig`,
then query parameter variables of the queried predicate using `unscoped`,
and query signature sets and relations using `getSingleAs`, `getDoubleAs`, and `getTripleAs`.

The following Code might for instance be used for the graph example:

``` Haskell
newtype Node = Node Int deriving (Eq, Show, Ord)

instanceToNames
  :: AlloyInstance
  -> Either String (Set Node, Set (Node, Int), Set (Node, Node, Int), Set (Node), Set (Node))
instanceToNames insta = do
  let node :: String -> Int -> Either String Node
      node = object "Node" Node
  n     <- lookupSig (scoped "this" "Node") insta
  nodes <- getSingleAs "" node n
  store <- getDoubleAs "stored" node int n
  flow  <- getTripleAs "flow" node node int n
  x     <- lookupSig (unscoped "$withFlow_x") insta >>= getSingleAs "" node
  y     <- lookupSig (unscoped "$withFlow_y") insta >>= getSingleAs "" node
  return (nodes, store, flow, x, y)
```

Calling `instanceToNames` on the above instance would result in the following expression:

``` Haskell
Right (
  fromList [Node 0,Node 1],
  fromList [(Node 0,0),(Node 1,1)],
  fromList [(Node 0,Node 1,0),(Node 1,Node 0,3)],
  fromList [Node 1],
  fromList [Node 0]
  )
```
