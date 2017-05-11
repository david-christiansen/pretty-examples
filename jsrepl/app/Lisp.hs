{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module Lisp where

import Control.Applicative
import Data.Char

import qualified Data.Text as T

import Lib

import Text.PrettyPrint.Final

data Expr = Symbol T.Text | Nil | Cons Expr Expr | Num Integer


data RExpr = RSymbol T.Text | RNum Integer | ProperList [RExpr] | ImproperList [RExpr]

toR (Symbol s) = RSymbol s
toR (Num i) = RNum i
toR Nil = ProperList []
toR (Cons e1 e2) =
  case toR e2 of
    RSymbol s -> ImproperList [toR e1, RSymbol s]
    ProperList [] -> ProperList [toR e1]
    ProperList es -> ProperList (toR e1 : es)
    ImproperList es -> ImproperList (toR e1 : es)

e1,e2,e3,e4,e5 :: Expr
e1 = Cons (Symbol (T.pack "quote")) (Cons (Symbol (T.pack "hello")) Nil)
e2 = Cons (Symbol (T.pack "λ")) (Cons (Cons (Symbol (T.pack ("x"))) Nil) (Cons (Symbol (T.pack "x")) Nil))
e3 = foldr Cons Nil (replicate 30 (Symbol (T.pack "i")))
e4 = foldr Cons Nil (replicate 30 (Symbol (T.pack "M")))
e5 = let go n = foldr Cons Nil (replicate n (Symbol (T.pack (show n))))
     in foldr Cons Nil [go n | n <- [0 .. 30]]

newtype Parse a = Parse { parse :: String -> Either String (String, a) }
  deriving Functor

instance Applicative Parse where
  pure x = Parse $ \ txt -> Right (txt, x)
  (Parse pf) <*> (Parse px) =
    Parse $ \txt ->
      case pf txt of
        Left err -> Left err
        Right (rest, f) ->
          case px rest of
            Left err -> Left err
            Right (rest', x) -> Right (rest', f x)

instance Alternative Parse where
  empty = Parse $ \ xs -> Left $ "Failed to parse " ++ show xs
  p1 <|> p2 =
    Parse $ \ txt ->
      case parse p1 txt of
        Left err -> parse p2 txt
        Right x -> Right x

token :: Parse a -> Parse a
token p = Parse $ \s ->
  case parse p s of
    Left err -> Left err
    Right (rest, res) -> Right (dropWhile isSpace rest, res)

isExtendedAlpha c = isAlpha c || c `elem` extra
  where extra = [ '+', '-', '.', '*', '/', '<', '=', '>', '!', '?', ':'
                , '$', '%', '_', '&', '~', '^'
                ]

readSym :: Parse Expr
readSym = Parse $ \ txt ->
  case takeWhile isExtendedAlpha txt of
    ok@(c:cs) | (not (c `elem` "+-#.") || cs == []) ->
      Right (drop (length ok) txt, Symbol (T.pack ok))
    _ -> Left $ "Not a symbol at " ++ take 20 txt

readQuoted = Parse $ \ txt ->
  case txt of
    ('\'':cs) ->
      fmap (\(r, e) -> (r, (Cons (Symbol (T.pack "quote")) (Cons e Nil))))
           (parse readExpr cs)
    other ->
      Left $ "Expected quote at: " ++ other

readChar :: Char -> Parse ()
readChar c = Parse $ \txt ->
  case txt of
    [] -> Left $ "Expected " ++ show c
    (x:xs) ->
      if c == x
        then Right (xs, ())
        else Left $ "Expected " ++ show c ++ " got " ++ show x

nat :: Parse Integer
nat = Parse $ \txt ->
  case takeWhile isDigit txt of
    [] -> Left $ "Expected integer at " ++ show (take 10 txt)
    num@(_:_) ->
      Right (drop (length num) txt, read num)

int :: Parse Expr
int = Num <$> (((* (-1)) <$> (readChar '-' *> nat)) <|> nat)

list :: Parse Expr
list = foldr Cons Nil <$>
         (token (readChar '(') *>
          many readExpr <*
          token (readChar ')'))

phrase :: Parse a -> Parse a
phrase p = Parse $ \ txt ->
  case parse p txt of
    Left e -> Left e
    Right ([], x) -> Right ([], x)
    Right (xs, x) -> Left $ "Expected to consume all input, but left over: " ++
                            show (take 15 xs)

readExpr = token int <|> token readSym <|> readQuoted <|> list


data LispAnn = LSym | LKwd | LExpr | LOpen | LClose
  deriving (Eq, Ord, Show)

lispKeywords = map T.pack ["lambda", "λ", "cond"]

open, close :: MonadPretty w LispAnn fmt m => m ()
open = annotate LOpen (text (T.pack "("))
close = annotate LClose (text (T.pack ")"))

parens :: MonadPretty w LispAnn fmt m => m a -> m a
parens l = grouped $ annotate LExpr $ do
  open
  x <- l
  close
  return x

prettyR :: MonadPretty w LispAnn fmt m => RExpr -> m ()
prettyR (RSymbol e) = if e `elem` lispKeywords then annotate LKwd (text e) else text e
prettyR (RNum u) = text (T.pack (show u))
prettyR (ProperList []) = annotate LExpr $ text (T.pack "'") >> parens (return ())
prettyR (ProperList [RSymbol q, e]) | q == T.pack "quote" =
  text (T.pack "'") >> prettyR e
prettyR (ProperList (RSymbol cond : branches)) | cond == T.pack "cond" =
  parens $ do
    i <- spaceWidth
    nest (2 * i) $ do
      prettyR (RSymbol cond)
      ifFlat (space i) newline
      grouped $ hvsep (map prettyR branches)
prettyR (ProperList (RSymbol lambda : ProperList args : body))
  | lambda `elem` map T.pack ["lambda", "λ"] =
  parens $ do
    i <- spaceWidth
    nest (2 * i) $ do
      prettyR (RSymbol lambda)
      space i
      parens $ hvsep (map prettyR args)
      ifFlat (space i) newline
      grouped $ hvsep (map prettyR body)
prettyR (ProperList [x]) = parens $ prettyR x
prettyR (ProperList (x:xs)) = parens $ do
  align $ do prettyR x
             i <- spaceWidth
             ifFlat (space i) newline
             hvsep (map prettyR xs)
prettyR (ImproperList xs) =
  parens $ hvsep (map prettyR xs)
  where withDot [] = []
        withDot [x] = [x]
        withDot [x, y] = [x, char '.', y]
        withDot (x:y:zs) = x : withDot (y:zs)

d1, d2, d3, d4, d5 :: DocM LispAnn ()
d1 = prettyR (toR e1)
d2 = prettyR (toR e2)
d3 = prettyR (toR e3)
d4 = prettyR (toR e4)
d5 = prettyR (toR e5)
