{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer hiding (Alt)
import Control.Monad.State
import Control.Monad.RWS hiding (Alt)

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, tryTakeMVar, putMVar)

import Data.Foldable (for_, traverse_)

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

import Language.Haskell.Exts

---
import Text.PrettyPrint.Final (newline)
---
import Lib
import PPHaskell

---
exts = EnableExtension <$>
         [ MultiParamTypeClasses
         , FunctionalDependencies
         ]

theMode = ParseMode "input.hs" Haskell2010 exts False True Nothing True

config :: REPLConfig HsAnn
config =
  REPLConfig
    { handleInput = hI
    , showAnn = show
    }
  where hI i =
          case parseExpWithMode theMode i of
            ParseFailed loc err -> Left err
            ParseOk ast ->
              Right $ pp ast >> newline

main :: IO ()
main = jsREPLMulti config



