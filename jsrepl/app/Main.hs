{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS

import Data.List (intersperse)

import Control.Monad (guard)
--import Control.Monad.Reader (ReaderT(..), ask, runReader)
import Control.Monad.IO.Class (MonadIO(..))
import GHCJS.DOM (currentDocument)
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
                       , unsafeCastTo)

import qualified GHCJS.DOM.Element as E (click, getAttribute, keyPress, setAttribute)

---

import qualified Data.Text as T
---

import Pretty

-- Widths in HTML-land will be ints representing pixels

renderAtom :: (IsDocument d) => d -> Atom Int -> IO HTMLElement
renderAtom htmlDoc ANewline = createElement htmlDoc "br" HTMLElement
renderAtom htmlDoc (AChunk (CText t)) = do
  txt <- createTextNodeUnsafe' htmlDoc (T.unpack t)
  elt <- createElement htmlDoc "span" HTMLElement
  appendChild elt (Just txt)
  return elt
renderAtom htmlDoc (AChunk (CSpace w)) = do
  elt <- createElement htmlDoc "span" HTMLElement
  E.setAttribute elt "style" ("width: " ++ show w ++ "px")
  return elt


render :: (IsDocument d) => d -> HTMLElement -> (ann -> String) -> POut Int ann -> IO ()
render htmlDoc parent toClass out = go out >> return ()
  where
    go out =
      case out of
        PNull -> createTextNodeUnsafe' htmlDoc "" >>= appendChild parent . Just
        PAtom a -> renderAtom htmlDoc a >>= appendChild parent . Just
        PSeq a b -> go a >> go b
        PAnn a o -> do
          elt <- createElement htmlDoc "span" HTMLElement :: IO HTMLElement
          E.setAttribute elt "className" (toClass a)
          render htmlDoc (elt :: HTMLElement) toClass o
          appendChild parent (Just elt)

--------------------

data Expr = Symbol T.Text | Nil | Cons Expr Expr

data RExpr = RSymbol T.Text | ProperList [RExpr] | ImproperList [RExpr]

toR (Symbol s) = RSymbol s
toR Nil = ProperList []
toR (Cons e1 e2) =
  case toR e2 of
    RSymbol s -> ImproperList [toR e1, RSymbol s]
    ProperList [] -> ProperList [toR e1]
    ProperList es -> ProperList (toR e1 : es)
    ImproperList es -> ImproperList (toR e1 : es)

e1,e2,e3,e4 :: Expr
e1 = Cons (Symbol (T.pack "quote")) (Cons (Symbol (T.pack "hello")) Nil)
e2 = Cons (Symbol (T.pack "λ")) (Cons (Cons (Symbol (T.pack ("x"))) Nil) (Cons (Symbol (T.pack "x")) Nil))
e3 = foldr Cons Nil (replicate 30 (Symbol (T.pack "i")))
e4 = foldr Cons Nil (replicate 30 (Symbol (T.pack "M")))

----------------------

data LispAnn = LSym | LKwd deriving (Eq, Ord, Show)

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

measureChunk :: (Chunk Int, [String]) -> RenderM Int
measureChunk (CText txt, fmt) = do
  doc    <- askDoc
  parent <- askParent
  elt <- createElement doc "span" HTMLElement
  txtNode <- createTextNodeUnsafe' doc (T.unpack txt)
  appendChild elt (Just txtNode)
  E.setAttribute elt "className" (concat (intersperse " " fmt))
  E.setAttribute elt "display" "hidden"
  appendChild parent (Just elt)
  --Just w <- E.getAttribute elt "offsetWidth"
  let w = length (T.unpack txt) -- FIXME
  removeChild parent (Just elt)
  return w --(read w)
measureChunk (CSpace w, _)  = pure w

newtype DocM a = DocM { unDocM :: RWST (PEnv Int LispAnn [String])
                                       (POut Int LispAnn)
                                       (PState Int [String])
                                       (MaybeT RenderM)
                                        a }
  deriving
    ( Functor, Applicative, Monad, Alternative
    , MonadReader (PEnv Int LispAnn [String])
    , MonadWriter (POut Int LispAnn)
    , MonadState (PState Int [String])
    )

instance MonadPretty Int LispAnn [String] DocM

runDocM :: PEnv Int LispAnn [String] -> PState Int [String] -> DocM a
        -> RenderM (Maybe (PState Int [String], POut Int LispAnn, a))
runDocM e s d = fmap rearrange <$> (runMaybeT (runRWST (unDocM d) e s))
  where rearrange (a, s', o) = (s', o, a)


instance Measure Int [String] DocM where
  measure l = sum <$> mapM (liftRender . measureChunk) l

liftRender :: RenderM a -> DocM a
liftRender act = DocM (lift (lift act))

prettyR :: RExpr -> DocM ()
prettyR (RSymbol e) = text e
prettyR (ProperList []) = text (T.pack "'()")
prettyR (ProperList [RSymbol q, e]) | q == T.pack "quote" =
  text (T.pack "'") >> prettyR e
prettyR (ProperList xs) =
  collection (text (T.pack "(")) (text (T.pack ")")) (return ())
    (map prettyR xs)
prettyR (ImproperList xs) =
  collection (text (T.pack "(")) (text (T.pack ")")) (return ())
    (withDot (map prettyR xs))
  where withDot [] = []
        withDot [x] = [x]
        withDot [x, y] = [x, char '.', y]
        withDot (x:y:zs) = x : withDot (y:zs)

d1, d2, d3, d4 :: DocM ()
d1 = prettyR (toR e1)
d2 = prettyR (toR e2)
d3 = prettyR (toR e3)
d4 = prettyR (toR e4)
--------------------

state0 :: PState Int [String]
state0 = PState
  { curLine = []
  }

env0 :: Monoid fmt => Int ->  PEnv Int a fmt
env0 w = PEnv
  { maxWidth = w
  , maxRibbon = (w * 4) `div` 5
  , layout = Break
  , failure = CantFail
  , nesting = 0
  , formatting = mempty
  , formatAnn = const mempty
  }


execDoc :: Document -> HTMLElement -> DocM () -> IO (POut Int LispAnn)
execDoc htmlDoc parent d = do
  (Just parentWidth) <- E.getAttribute parent "offsetWidth"
  foo <- runRender (runDocM (env0 (read parentWidth)) state0 d) (htmlDoc, parent)
  case foo of
    Nothing -> pure $ PAtom $ AChunk $ CText $ T.pack "<internal pretty printing error>"
    Just (_, o, ()) -> pure o

--------------------
createElement doc tagName tagType =
  createElementUnsafe doc (Just tagName) >>= unsafeCastTo tagType

record doc parent line = do
  textNode <- createTextNodeUnsafe' doc line
  br       <- createElement doc "br" HTMLBRElement
  appendChild parent (Just textNode)
  appendChild parent (Just br)
  return ()

main :: IO ()
main = do
  Just doc <- currentDocument
  Just body <- getBody doc

  transcript <- createElement doc "div" HTMLElement
  input      <- createElement doc "input" HTMLInputElement
  button     <- createElement doc "input" HTMLInputElement
  setType button "button"


  appendChild body (Just transcript)

  let terpri = createElement doc "br" HTMLBRElement >>= appendChild transcript . Just

  o1 <- execDoc doc transcript d1
  render doc transcript (const "") o1
  terpri
  o2 <- execDoc doc transcript d2
  render doc transcript (const "") o2
  terpri
  o3 <- execDoc doc transcript d3
  render doc transcript (const "") o3
  terpri
  o4 <- execDoc doc transcript d4
  render doc transcript (const "") o4
  terpri


  appendChild body (Just input)
  appendChild body (Just button)

  let save line =
        (record doc transcript line >>
         setValue input (Just "")) :: EventM e t ()

  let userInput = getValueUnsafe input :: EventM e t String

  on button E.click (userInput >>= save)
  on input E.keyPress $ do
    wh <- uiWhich
    if wh == 13
      then (userInput >>= save)
      else return ()

  liftIO $ return ()

createTextNodeUnsafe' :: (MonadIO m, IsDocument self) => self -> String -> m Text
createTextNodeUnsafe' = createTextNodeUnsafe


