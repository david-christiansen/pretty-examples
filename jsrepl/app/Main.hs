{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Data.Char (isSpace, isAlpha)
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

import Pretty


----------------

-----------------------------------
-- Widths in HTML-land will be ints representing pixels

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
  E.setAttribute elt "style" ("display: inline-block; min-width: " ++ show (round w) ++ "px;")
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

e1,e2,e3,e4,e5 :: Expr
e1 = Cons (Symbol (T.pack "quote")) (Cons (Symbol (T.pack "hello")) Nil)
e2 = Cons (Symbol (T.pack "λ")) (Cons (Cons (Symbol (T.pack ("x"))) Nil) (Cons (Symbol (T.pack "x")) Nil))
e3 = foldr Cons Nil (replicate 30 (Symbol (T.pack "i")))
e4 = foldr Cons Nil (replicate 30 (Symbol (T.pack "M")))
e5 = let go n = foldr Cons Nil (replicate n (Symbol (T.pack (show n))))
     in foldr Cons Nil [go n | n <- [0 .. 30]]

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

prettyR :: RExpr -> DocM ()
prettyR (RSymbol e) = if e `elem` lispKeywords then annotate LKwd (text e) else text e
prettyR (ProperList []) = text (T.pack "'()")
prettyR (ProperList [RSymbol q, e]) | q == T.pack "quote" =
  text (T.pack "'") >> prettyR e
prettyR (ProperList [x]) = do
  text (T.pack "(")
  prettyR x
  text (T.pack ")")
prettyR (ProperList (x:xs)) = do
  text (T.pack "(")
  align $ do prettyR x
             space 1
             hvsep (map prettyR xs)
--  hvsep $ align (prettyR x) : (map prettyR xs)
  text (T.pack ")")
prettyR (ImproperList xs) =
  collection (text (T.pack "(")) (text (T.pack ")")) (return ())
    (withDot (map prettyR xs))
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

readSym :: Parse Expr
readSym = Parse $ \ txt ->
  case takeWhile isAlpha txt of
    [] -> Left $ "Not a symbol at " ++ take 20 txt
    ok@(c:cs) -> Right (drop (length ok) txt, Symbol (T.pack ok))

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

readExpr = token readSym <|> readQuoted <|> list

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
              render doc transcript (const "") o
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


