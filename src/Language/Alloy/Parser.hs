{-# LANGUAGE FlexibleContexts #-}
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
  parseInstance,
  ) where

import qualified Data.Set                         as S (fromList)
import qualified Data.Map                         as M
  (alter, empty, insert, singleton)

import Control.Monad                    (void)
import Control.Monad.Except             (MonadError, throwError)
import Data.Functor                     (($>))
import Data.Set                         (Set)
import Text.Parsec
--import Text.Trifecta
import Text.Parsec.String               (Parser)

import Language.Alloy.Types (
  AlloyInstance,
  Annotation (..),
  Entries,
  Entry (..),
  Object (..),
  Relation (..),
  Signature (..),
  )

{-|
Parse an Alloy instance from a given String.
May fail with a 'ParseError'.
-}
parseInstance :: (MonadError ParseError m) => String -> m AlloyInstance
parseInstance inst = case parse alloyInstance "Alloy-Instance" inst of
  Left l  -> throwError l
  Right r -> return $ combineEntries r

combineEntries :: [Entries (,)] -> AlloyInstance
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
