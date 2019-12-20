{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Language.Alloy.Parser
Description : A generic Alloy instance parser for Call Alloy library
Copyright   : (c) Marcellus Siegburg, 2019
License     : MIT
Maintainer  : marcellus.siegburg@uni-due.de

This module allows for parsing and converting instances into Haskell data
structures.
Basically all modules are parsed into a Map of Map of Set allowing easy lookup
of every returned set and relation.
-}
module Language.Alloy.Parser (
  AlloyInstance, AlloySig, Signature,
  getSingle, getDouble, getTriple,
  lookupSig,
  objectName,
  parseInstance,
  relToMap,
  scoped, unscoped,
  ) where

import qualified Data.Set                         as S (fromList, size, toList)
import qualified Data.Map                         as M
  (alter, empty, fromList, insert, lookup, singleton)

import Control.Monad                    (void)
import Control.Monad.Except             (MonadError, throwError)
import Data.Function                    (on)
import Data.Functor                     (($>))
import Data.List                        (groupBy)
import Data.Map                         (Map)
import Data.Set                         (Set)
import Data.String                      (IsString, fromString)
import Text.Parsec
--import Text.Trifecta
import Text.Parsec.String               (Parser)

{-|
An Alloy signature.
-}
data Signature = Signature {
    scope    :: Maybe String,
    sigName  :: String
  } deriving (Eq, Ord, Show)

{-|
Create a 'Signature' given its scope and name.
-}
scoped :: String -> String -> Signature
scoped = Signature . Just

{-|
Create an unscoped Signature given its name.
-}
unscoped :: String -> Signature
unscoped = Signature Nothing

{-|
A complete Alloy instance.
-}
type AlloyInstance = Entries Map

{-|
A signature with all its objects and relations.
-}
type AlloySig      = Entry Map Set

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

data Relation a =
    EmptyRelation
  | Single (a Object)
  | Double (a (Object, Object))
  | Triple (a (Object, Object, Object))

data Annotation = Skolem deriving (Eq, Show)

type Entries a = a Signature (Entry a Set)

data Entry a b = Entry {
    annotation :: Maybe Annotation,
    relation   :: a String (Relation b)
  }

{-|
Retrieve a set of objects of a given 'AlloySig'.
Successful if the signatures relation is a set (or empty).
-}
getSingle
  :: (IsString s, MonadError s m)
  => String
  -> AlloySig
  -> m (Set Object)
getSingle = lookupRel single

{-|
Retrieve a binary relation of objects of a given 'AlloySig'.
Successful if the signatures relation is binary (or empty).
-}
getDouble
  :: (IsString s, MonadError s m)
  => String
  -> AlloySig
  -> m (Set (Object, Object))
getDouble = lookupRel double

{-|
Retrieve a ternary relation of objects of a given 'AlloySig'.
Successful if the signatures relation is ternary (or empty).
-}
getTriple
  :: (IsString s, MonadError s m)
  => String
  -> AlloySig
  -> m (Set (Object, Object, Object))
getTriple = lookupRel triple

binaryToMap :: (Ord k, Ord v) => Set (k, v) -> Map k (Set v)
binaryToMap bin = M.fromList
  [(fst (head gs), S.fromList $ snd <$> gs)
  | gs <- groupBy ((==) `on` fst) $ S.toList bin]

{-|
Transforms a relation into a Mapping.
Is only successful (i.e. returns 'return' if the given transformation function is
able to map the given values injectively.
-}
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
Lookup a signature within a given Alloy Instance.
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
Retrieve an objects name.
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

{-|
Parse an Alloy instance from a given String.
May fail with a 'ParseError'.
-}
parseInstance :: (MonadError ParseError m) => String -> m AlloyInstance
parseInstance inst = case parse alloyInstance "Alloy-Instance" inst of
  Left l  -> throwError l
  Right r -> return $ combineEntries r

combineEntries :: [Entries (,)] -> Entries Map
combineEntries = foldl createOrInsert M.empty
  where
    createOrInsert ys (s, e) = M.alter (Just . alterSig e) s ys
    alterSig e Nothing  = e { relation = uncurry M.singleton $ relation e}
    alterSig e (Just y) = y { relation = uncurry M.insert (relation e) (relation y) }

alloyInstance :: Parser [Entries (,)]
alloyInstance = (try (void $ string "---INSTANCE---" *> newline) <|> return ())
  *> many entry

entry :: Parser (Entries (,))
entry = do
  entryAnnotation <- try (string "skolem " $> Just Skolem) <|> pure Nothing
  entrySignature <- sig
  (entrySignature,)
    <$> (Entry
         <$> pure entryAnnotation
         <*> ((,)
              <$> ((string "<:" *> word) <|> pure "")
              <*> parseRelations <* (void newline <|> eof)))

sig :: Parser Signature
sig =
  try (Signature <$> (Just <$> word) <* char '/' <*> word)
  <|> Signature <$> pure Nothing <*> word

parseRelations :: Parser (Relation Set)
parseRelations = char '='
  *> (try (string "{}" $> EmptyRelation)
      <|> fmap Triple (try $ sep tripleRel)
      <|> fmap Double (try $ sep doubleRel)
      <|> fmap Single (sep singleRel))
  where
    sep rel = S.fromList
      <$> between (char '{') (char '}') (rel `sepBy` string ", ")
    tripleRel = (,,) <$> nextObject <*> nextObject <*> object
    doubleRel = (,) <$> nextObject <*> object
    singleRel = object
    nextObject = object <* string "->"

object :: Parser Object
object =
  try (Object <$> word <* char '$' <*> (read <$> many1 digit))
  <|> try (NumberObject <$> int)
  <|> NamedObject <$> word

int :: Parser Int
int = fmap read $ (++)
  <$> (try (string "-") <|> pure "")
  <*> many1 digit

word :: Parser String
word = (:)
  <$> (letter <|> char '$')
  <*> many (letter <|> digit <|> char '_')
