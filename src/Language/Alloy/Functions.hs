{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Language.Alloy.Functions
Description : Type definitions for Call Alloy library
Copyright   : (c) Marcellus Siegburg, 2019
License     : MIT
Maintainer  : marcellus.siegburg@uni-due.de

This module exports basic types in order to work with parsed Alloy instances.
Furthermore, it provides functions to handle these parsed instances.
-}
module Language.Alloy.Functions (
  getSingle, getDouble, getTriple,
  getSingleAs, getDoubleAs, getTripleAs,
  int, object,
  lookupSig,
  objectName,
  relToMap,
  scoped, unscoped,
  ) where

import qualified Data.Set                         as S (fromList, size, toList)
import qualified Data.Map                         as M (fromList, lookup)

import Control.Monad.Except             (MonadError, throwError)
import Data.Function                    (on)
import Data.List                        (groupBy)
import Data.Map                         (Map)
import Data.Set                         (Set)
import Data.String                      (IsString, fromString)

import Language.Alloy.Types (
  AlloyInstance,
  AlloySig,
  Entry (..),
  Object (..),
  Relation (..),
  Signature (..),
  )

{-|
Create a 'Signature' given its scope and name.
-}
scoped :: String -> String -> Signature
scoped = Signature . Just

{-|
Create an unscoped 'Signature' given its name.
-}
unscoped :: String -> Signature
unscoped = Signature Nothing

{-
Might be introduced some time in Data.Set in the containers package
see https://github.com/haskell/containers/issues/779
-}
traverseSet
  :: (Ord a, Applicative f)
  => (a2 -> f a)
  -> Set a2
  -> f (Set a)
traverseSet f = fmap S.fromList . traverse f . S.toList

{-|
For retrieval of 'Int' values using a get... function.

e.g. returning all (within Alloy) available Int values could look like this

> do n <- lookupSig (unscoped "Int")
>    getSingleAs "" int n
-}
int
  :: (IsString s, MonadError s m, Semigroup s)
  => String
  -> Int
  -> m Int
int = object "" id

{-|
For retrieval of an unmixed type of values using a get... function
(should be the case for uniformly base named values;
this is usually never true for the universe (@lookupSig (unscoped "univ")@))
I.e. setting and checking the 'String' for the base name of the value to look for,
but failing in case anything different appears (unexpectedly).
-}
object
  :: (IsString s, MonadError s m, Semigroup s)
  => String
  -> (Int -> a)
  -> String
  -> Int
  -> m a
object s f s' g =
  if s /= s
  then throwError $ "expected an object of name " <> fromString s
    <> " but got an object of name "
    <> fromString s' <> "."
  else return $ f g

specifyObject
  :: (String -> Int -> m a)
  -> Object
  -> m a
specifyObject f o = case o of
  NumberObject i -> f "" i
  Object n i -> f n i
  NamedObject g -> error $ "there is no way of converting Object "
    ++ g
    ++ "\nPlease open an issue at https://github.com/marcellussiegburg/call-alloy stating what you tried to attempt"

{-|
Retrieve a set of values of a given 'AlloySig'.
Values will be created by applying the given mapping function from object Name
and 'Int' to value.
The mapping has to be injective (for all expected cases).
Successful if the signature's relation is a set (or empty).
-}
getSingleAs
  :: (IsString s, MonadError s m, Ord a)
  => String
  -> (String -> Int -> m a)
  -> AlloySig
  -> m (Set a)
getSingleAs s f inst = do
  set <- getSingle s inst
  traverseSet (specifyObject f) set

{-# DEPRECATED getSingle "use the typed version getSingleAs instead" #-}
{-|
Retrieve a set of objects of a given 'AlloySig'.
Successful if the signature's relation is a set (or empty).
-}
getSingle
  :: (IsString s, MonadError s m)
  => String
  -> AlloySig
  -> m (Set Object)
getSingle = lookupRel single

{-|
Retrieve a binary relation of values of given 'AlloySig'.
Values will be created by applying the given mapping functions from object Name
and 'Int' to the value.
The mapping has to be injective (for all expected cases).
Successful if the signature's relation is binary (or empty).
-}
getDoubleAs
  :: (IsString s, MonadError s m, Ord a, Ord b)
  => String
  -> (String -> Int -> m a)
  -> (String -> Int -> m b)
  -> AlloySig
  -> m (Set (a, b))
getDoubleAs s f g inst = do
  set <- getDouble s inst
  traverseSet applyMapping set
  where
    applyMapping (x, y) = (,)
      <$> specifyObject f x
      <*> specifyObject g y

{-# DEPRECATED getDouble "use the typed version getDoubleAs instead" #-}
{-|
Retrieve a binary relation of objects of a given 'AlloySig'.
Successful if the signature's relation is binary (or empty).
-}
getDouble
  :: (IsString s, MonadError s m)
  => String
  -> AlloySig
  -> m (Set (Object, Object))
getDouble = lookupRel double

{-|
Retrieve a ternary relation of values of a given 'AlloySig'.
Values will be created by applying the given mapping functions from object Name
and 'Int' to the value.
The mapping has to be injective (for all expected cases).
Successful if the signature's relation is ternary (or empty).
-}
getTripleAs
  :: (IsString s, MonadError s m, Ord a, Ord b, Ord c)
  => String
  -> (String -> Int -> m a)
  -> (String -> Int -> m b)
  -> (String -> Int -> m c)
  -> AlloySig
  -> m (Set (a, b, c))
getTripleAs s f g h inst = do
  set <- getTriple s inst
  traverseSet applyMapping set
  where
    applyMapping (x, y, z) = (,,)
      <$> specifyObject f x
      <*> specifyObject g y
      <*> specifyObject h z

{-# DEPRECATED getTriple "use the typed version getTripleAs instead" #-}
{-|
Retrieve a ternary relation of objects of a given 'AlloySig'.
Successful if the signature's relation is ternary (or empty).
-}
getTriple
  :: (IsString s, MonadError s m)
  => String
  -> AlloySig
  -> m (Set (Object, Object, Object))
getTriple = lookupRel triple

{-|
Transforms a relation into a Mapping.
-}
binaryToMap :: (Ord k, Ord v) => Set (k, v) -> Map k (Set v)
binaryToMap bin = M.fromList
  [(fst (head gs), S.fromList $ snd <$> gs)
  | gs <- groupBy ((==) `on` fst) $ S.toList bin]

{-# DEPRECATED relToMap "use binaryToMap instead" #-}
relToMap
  :: (IsString s, MonadError s m, Ord k, Ord v)
  => (a -> (k, v))
  -> Set a
  -> m (Map k (Set v))
relToMap f rel
  | S.size map' == length rel' = return $ binaryToMap map'
  | otherwise                  =
    throwError "relToMap: The performed transformation is not injective."
  where
    rel' = S.toList rel
    map' = S.fromList $ fmap f rel'

lookupRel
  :: (IsString s, MonadError s m)
  => (Relation a -> m b)
  -> String
  -> Entry Map a
  -> m b
lookupRel kind rel e = case M.lookup rel (relation e) of
  Nothing -> throwError $ fromString $ "relation " ++ fromString rel
    ++ " is missing in the Alloy instance"
  Just r  -> kind r

{-|
Lookup a signature within a given Alloy instance.
-}
lookupSig
  :: (IsString s, MonadError s m)
  => Signature
  -> AlloyInstance
  -> m AlloySig
lookupSig s insta = case M.lookup s insta of
  Nothing -> throwError $ fromString $ maybe "" (++ "/") (scope s) ++ sigName s
    ++ " is missing in the Alloy instance"
  Just e   -> return e

{-# DEPRECATED objectName "use the typed versions of get... e.g. getSingleAs instead of getSingle" #-}
{-|
Retrieve an object's name.
-}
objectName :: Object -> String
objectName o = case o of
  Object s n     -> s ++ '$' : show n
  NumberObject n -> show n
  NamedObject n  -> n

single
  :: (IsString s, MonadError s m, Monoid (a Object))
  => Relation a
  -> m (a Object)
single EmptyRelation = return mempty
single (Single r)    = return r
single _             = throwError "Relation is (unexpectedly) a mapping"

double
  :: (IsString s, MonadError s m, Monoid (a (Object, Object)))
  => Relation a
  -> m (a (Object, Object))
double EmptyRelation = return mempty
double (Double r)    = return r
double _             = throwError "Relation is not a binary mapping"

triple
  :: (IsString s, MonadError s m, Monoid (a (Object, Object, Object)))
  => Relation a
  -> m (a (Object, Object, Object))
triple EmptyRelation = return mempty
triple (Triple r)    = return r
triple _             = throwError "Relation is not a ternary mapping"
