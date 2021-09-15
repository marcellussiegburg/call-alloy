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
