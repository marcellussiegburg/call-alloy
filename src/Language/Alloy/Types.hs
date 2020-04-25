{-|
Module      : Language.Alloy.Types
Description : Type definitions for Call Alloy library
Copyright   : (c) Marcellus Siegburg, 2019
License     : MIT
Maintainer  : marcellus.siegburg@uni-due.de

This module defines required types for the Alloy instance parser.
Unless reexported, these types are considered as internal.
-}
module Language.Alloy.Types (
  AlloyInstance, AlloySig, Annotation (..),
  Entries, Entry (..), Object (..), Relation (..), Signature (..),
  ) where

import Data.Map                         (Map)
import Data.Set                         (Set)

{-|
A complete Alloy instance.
-}
type AlloyInstance = Entries Map

{-|
A signature with all its objects and relations.
-}
type AlloySig      = Entry Map Set

{-|
A collection of signatures with associated entries.
-}
type Entries a = a Signature (Entry a Set)

{-|
An Alloy signature.
-}
data Signature = Signature {
    scope    :: Maybe String,
    sigName  :: String
  } deriving (Eq, Ord, Show)

{-|
A concrete instance of an Alloy signature.
-}
data Object =
    Object {
      objSig     :: String,
      identifier :: Int
    }
  | NumberObject {
      number :: Int
    }
  | NamedObject {
      objName :: String
    } deriving (Eq, Ord, Show)

{-|
An Alloy relation, i.e. a collection of a tuple of 'Object's.
The collection may be a singleton, a set, a list, ...
-}
data Relation a =
    EmptyRelation
  | Single (a Object)
  | Double (a (Object, Object))
  | Triple (a (Object, Object, Object))

{-|
Specifically marked values.
-}
data Annotation = Skolem deriving (Eq, Show)

{-|
An entry is a collection of a 'Relation'.
This collection may be a singleton, a set, a list, ...
-}
data Entry a b = Entry {
    annotation :: Maybe Annotation,
    relation   :: a String (Relation b)
  }
