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
import System.IO.Unsafe (unsafePerformIO) -- yolo

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

import Text.PrettyPrint.Final


----------------

-----------------------------------
-- Widths in HTML-land will be doubles representing pixels

preProcess :: T.Text -> T.Text
preProcess = T.replace (T.pack " ") (T.pack " ") -- it's a non breaking space char

renderAtom :: (IsDocument d) => d -> Atom Double -> IO HTMLElement
renderAtom htmlDoc ANewline = createElement htmlDoc "br" HTMLElement
renderAtom htmlDoc (AChunk (CText (preProcess -> t))) = do
  txt <- createTextNodeUnsafe' htmlDoc (T.unpack t)
  elt <- createElement htmlDoc "span" HTMLElement
  appendChild elt (Just txt)
  return elt
renderAtom htmlDoc (AChunk (CSpace w)) = do
  elt <- createElement htmlDoc "span" HTMLElement
  E.setAttribute elt "style" $
    "display: inline-block;" ++
    "min-width: " ++ show (round w) ++ "px;" ++
    "max-width: " ++ show (round w) ++ "px;"
  txt <- createTextNodeUnsafe' htmlDoc " "
  appendChild elt (Just txt)
  return elt


render :: (IsDocument d) => d -> HTMLElement -> (ann -> String) -> POut Double ann -> IO ()
render htmlDoc parent toClass out = go out >> return ()
  where
    go out =
      case out of
        PNull -> createTextNodeUnsafe' htmlDoc "" >>= appendChild parent . Just
        PAtom a -> renderAtom htmlDoc a >>= appendChild parent . Just
        PSeq a b -> go a >> go b
        PAnn a o -> do
          elt <- createElement htmlDoc "span" HTMLElement :: IO HTMLElement
          E.setAttribute elt "class" (toClass a)
          render htmlDoc (elt :: HTMLElement) toClass o
          appendChild parent (Just elt)

--------------------




----------------------

data LispAnn = LSym | LKwd | LExpr | LOpen | LClose
  deriving (Eq, Ord, Show)

newtype RenderM a = RenderM { runRender :: (Document, HTMLElement) -> IO a }
  deriving (Functor)

instance Applicative RenderM where
  pure = RenderM . const . pure
  RenderM f <*> RenderM x =
    RenderM (\doc -> do
                g <- f doc
                y <- x doc
                pure $ g y)

instance Monad RenderM where
  return = pure
  (RenderM x) >>= f = RenderM (\doc -> do
                                  y <- x doc
                                  runRender (f y) doc)

instance MonadIO RenderM where
  liftIO act = RenderM (const act)

askDoc :: RenderM Document
askDoc = RenderM  (pure . fst)

askParent :: RenderM HTMLElement
askParent = RenderM (pure . snd)

chunkWidths :: IORef (M.Map (Chunk Double, [String]) Double)
chunkWidths = unsafePerformIO (newIORef M.empty) -- TODO: move into RenderM state

measureChunk :: (Chunk Double, [String]) -> RenderM Double
measureChunk ch@(CText (preProcess -> txt), fmt) = do
  found <- M.lookup ch <$> liftIO (readIORef chunkWidths)
  case found of
    Just r -> return r
    Nothing -> do
      doc    <- askDoc
      parent <- askParent

      elt <- createElement doc "span" HTMLElement
      txtNode <- createTextNodeUnsafe' doc (T.unpack txt)
      appendChild elt (Just txtNode)
      E.setAttribute elt "className" (concat (intersperse " " fmt))
      E.setAttribute elt "display" "hidden"
      appendChild parent (Just elt)
      liftIO $ threadDelay 1
      w <- E.getOffsetWidth elt
      removeChild parent (Just elt)

      liftIO $ atomicModifyIORef' chunkWidths $ \ old -> (M.insert ch w old, ())
      return w
measureChunk (CSpace w, _)  = pure w

newtype DocM a = DocM { unDocM :: RWST (PEnv Double LispAnn [String])
                                       (POut Double LispAnn)
                                       (PState Double [String])
                                       (MaybeT RenderM)
                                        a }
  deriving
    ( Functor, Applicative, Monad, Alternative
    , MonadReader (PEnv Double LispAnn [String])
    , MonadWriter (POut Double LispAnn)
    , MonadState (PState Double [String])
    )

instance MonadPretty Double LispAnn [String] DocM

runDocM :: PEnv Double LispAnn [String] -> PState Double [String] -> DocM a
        -> RenderM (Maybe (PState Double [String], POut Double LispAnn, a))
runDocM e s d = fmap rearrange <$> (runMaybeT (runRWST (unDocM d) e s))
  where rearrange (a, s', o) = (s', o, a)


instance Measure Double [String] DocM where
  measure l = sum <$> mapM (liftRender . measureChunk) l

liftRender :: RenderM a -> DocM a
liftRender act = DocM (lift (lift act))

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

d1, d2, d3, d4, d5 :: DocM ()
d1 = prettyR (toR e1)
d2 = prettyR (toR e2)
d3 = prettyR (toR e3)
d4 = prettyR (toR e4)
d5 = prettyR (toR e5)
--------------------


--------------------
state0 :: PState Double [String]
state0 = PState
  { curLine = []
  }

env0 :: Monoid fmt => Double ->  PEnv Double a fmt
env0 w = PEnv
  { maxWidth = w
  , maxRibbon = (w * 4) / 5
  , layout = Break
  , failure = CantFail
  , nesting = 0
  , formatting = mempty
  , formatAnn = const mempty
  }


execDoc :: (MonadIO m) => Document -> HTMLElement -> DocM () -> m (POut Double LispAnn)
execDoc htmlDoc parent d = do
  parentWidth <- E.getOffsetWidth parent
  foo <- liftIO $ runRender (runDocM (env0 parentWidth) state0 d) (htmlDoc, parent)
  case foo of
    Nothing -> pure $ PAtom $ AChunk $ CText $ T.pack "<internal pretty printing error>"
    Just (_, o, ()) -> pure o

--------------------
createElement doc tagName tagType =
  createElementUnsafe doc (Just tagName) >>= unsafeCastTo tagType

record :: Document -> HTMLElement -> String -> EventM e t ()
record doc parent line = do
  case parse (phrase readExpr) line of
    Left err -> liftIO $ putStrLn err
    Right (_, e) ->
      do let d = prettyR (toR e)
         o <- liftIO $ execDoc doc parent d
         liftIO $ render doc parent (const "") o
         br <- createElement doc "br" HTMLBRElement
         appendChild parent (Just br)
         liftIO $ threadDelay 1
         return ()


main :: IO ()
main = do
  Just doc <- currentDocument
  Just window <- currentWindow
  Just body <- getBody doc

  transcript <- createElement doc "div" HTMLElement
  input      <- createElement doc "input" HTMLInputElement
  button     <- createElement doc "input" HTMLInputElement
  setType button "button"
  setValue button (Just "Go")

  -- examples <- createElement doc "select" HTMLElement
  -- for_ [e1, e2, e3, e4, e5] $ \e ->
  --   opt <- createElement doc "option" HTMLElement
    

  outputRecord <- newIORef [] :: IO (IORef [DocM ()])

  appendChild body (Just transcript)
  appendChild body (Just input)
  appendChild body (Just button)
  threadDelay 1

  renderLock <- newMVar ()

  let refreshOut = forkIO $ do
        ok <- tryTakeMVar renderLock
        case ok of
          Just () -> do
            E.setInnerHTML transcript (Just "")
            out <- reverse <$> readIORef outputRecord
            for_ out $ \ d -> do
              o <- execDoc doc transcript d
              render doc transcript show o
              threadDelay 1 -- render a browser frame
              br <- createElement doc "br" HTMLBRElement
              appendChild transcript (Just br)
            putMVar renderLock ()
          Nothing -> pure ()

  let addOut e = forkIO $
        atomicModifyIORef' outputRecord (\ old -> (prettyR (toR e) : old, ()))


  let save line =
       case parse (phrase readExpr) line of
         Left err -> liftIO $ do
           w <- currentWindow
           case w of
             Just win -> liftIO $ alert win err
             Nothing -> liftIO $ putStrLn err
         Right (_, e) ->
           liftIO $ do
             addOut e
             refreshOut
             setValue input (Just "")
             return ()

  let userInput = getValueUnsafe input :: EventM e t String

  on button E.click (userInput >>= save)
  on input E.keyPress $ do
    wh <- uiWhich
    if wh == 13
      then (userInput >>= save)
      else return ()

  (Just w) <- currentWindow
  on w resize $ liftIO (refreshOut *> pure ())


  liftIO $ return ()

createTextNodeUnsafe' :: (MonadIO m, IsDocument self) => self -> String -> m Text
createTextNodeUnsafe' = createTextNodeUnsafe


