{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, tryTakeMVar, putMVar)

import Data.Foldable (for_)

import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (intersperse)

import qualified Data.Map.Strict as M
import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')


import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO(..))
import GHCJS.DOM (currentDocument, currentWindow)
import GHCJS.DOM.Document (createElementUnsafe, createTextNodeUnsafe, getBody)
import GHCJS.DOM.EventM (EventM, event, on, uiWhich)
import GHCJS.DOM.HTMLInputElement (getValueUnsafe, setValue, setType)
import GHCJS.DOM.Node (appendChild, removeChild)
import GHCJS.DOM.Types ( Document
                       , HTMLElement(..)
                       , HTMLInputElement(..)
                       , HTMLDivElement(..)
                       , HTMLBRElement(..)
                       , HTMLSpanElement(..)
                       , IsDocument
                       , IsNode
                       , Text
                       , unsafeCastTo
                       )
import GHCJS.DOM.Window (alert, resize)
--import GHCJS.DOM.WindowTimers (setTimeout)

import qualified GHCJS.DOM.Element as E (click, getAttribute, keyPress, setAttribute, load, getOffsetWidth, setInnerHTML)

---

import qualified Data.Text as T
---

import Lisp
import Lib
import Text.PrettyPrint.Final


----------------------

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
--------------------

--------------------
createElement doc tagName tagType =
  createElementUnsafe doc (Just tagName) >>= unsafeCastTo tagType

config :: REPLConfig LispAnn
config =
  REPLConfig
    { handleInput = hI
    , showAnn = show
    }
  where hI line =
          case parse (phrase readExpr) line of
            Left err -> Left err
            Right (_, e) ->
              Right (prettyR (toR e))

main :: IO ()
main = jsREPL config



