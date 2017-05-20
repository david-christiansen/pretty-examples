{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Lib ( DocM(..)
           , REPLConfig(..)
           , jsREPL
           , jsREPLMulti
           , jsFile
           ) where

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS

import Control.Monad.IO.Class (MonadIO(..))

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newMVar, tryTakeMVar, putMVar)
import Data.Foldable (for_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List (intersperse)

import System.IO.Unsafe (unsafePerformIO) -- yolo

import GHCJS.DOM (currentDocument, currentWindow)
import GHCJS.DOM.Document (createElementUnsafe, createTextNodeUnsafe, getBody)
import GHCJS.DOM.EventM (EventM, event, on, target, uiWhich)
import GHCJS.DOM.File
import GHCJS.DOM.FileList
import GHCJS.DOM.FileReader
import GHCJS.DOM.HTMLInputElement (getFilesUnsafe, getValueUnsafe, setValue, setType)
import GHCJS.DOM.KeyboardEvent (getShiftKey)
import qualified GHCJS.DOM.HTMLTextAreaElement as TA (getValueUnsafe, setValue)
import GHCJS.DOM.Node (appendChild, removeChild)
import GHCJS.DOM.Types ( Document
                       , FileList(..)
                       , FromJSVal(..)
                       , HTMLElement(..)
                       , HTMLInputElement(..)
                       , HTMLDivElement(..)
                       , HTMLBRElement(..)
                       , HTMLTextAreaElement(..)
                       , HTMLSpanElement(..)
                       , IsDocument
                       , IsGObject
                       , IsNode
                       , JSVal
                       , Text
                       , ToJSString
                       , unsafeCastTo
                       )
import GHCJS.DOM.Window (alert, resize)
--import GHCJS.DOM.WindowTimers (setTimeout)

import qualified GHCJS.DOM.Element as E (change, click, getAttribute, keyPress, setAttribute, load, getOffsetWidth, setInnerHTML)

import Text.PrettyPrint.Final

----------------------------------

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
    "min-width: " ++ show w ++ "px;" ++
    "max-width: " ++ show w ++ "px;"
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
      E.setAttribute elt "class" (concat (intersperse " " fmt))
      E.setAttribute elt "display" "hidden"
      appendChild parent (Just elt)
      liftIO $ threadDelay 1
      w <- E.getOffsetWidth elt
      removeChild parent (Just elt)

      liftIO $ atomicModifyIORef' chunkWidths $ \ old -> (M.insert ch w old, ())
      return w
measureChunk (CSpace w, _)  = pure w


state0 :: PState Double [String]
state0 = PState
  { curLine = []
  }

env0 :: Monoid fmt => Double -> (a -> fmt) -> PEnv Double a fmt
env0 w fmtAnn = PEnv
  { maxWidth = w
  , maxRibbon = (w * 4) / 5
  , layout = Break
  , failure = CantFail
  , nesting = 0
  , formatting = mempty
  , formatAnn = fmtAnn
  }


execDoc :: (MonadIO m) => Document -> HTMLElement -> (ann -> [String]) -> DocM ann () -> m (POut Double ann)
execDoc htmlDoc parent fmtAnn d = do
  parentWidth <- E.getOffsetWidth parent
  foo <- liftIO $ runRender (runDocM (env0 parentWidth fmtAnn) state0 d) (htmlDoc, parent)
  case foo of
    Nothing -> pure $ PAtom $ AChunk $ CText $ T.pack "<internal pretty printing error>"
    Just (_, o, ()) -> pure o

newtype DocM ann a = DocM { unDocM :: RWST (PEnv Double ann [String])
                                           (POut Double ann)
                                           (PState Double [String])
                                           (MaybeT RenderM)
                                           a }
  deriving
    ( Functor, Applicative, Monad, Alternative
    , MonadReader (PEnv Double ann [String])
    , MonadWriter (POut Double ann)
    , MonadState (PState Double [String])
    )

instance MonadPretty Double ann [String] (DocM ann)

runDocM :: PEnv Double ann [String] -> PState Double [String] -> DocM ann a
        -> RenderM (Maybe (PState Double [String], POut Double ann, a))
runDocM e s d = fmap rearrange <$> (runMaybeT (runRWST (unDocM d) e s))
  where rearrange (a, s', o) = (s', o, a)


instance Measure Double [String] (DocM ann) where
  measure l = sum <$> mapM (liftRender . measureChunk) l

liftRender :: RenderM a -> DocM ann a
liftRender act = DocM (lift (lift act))


-----------------------------------
data REPLConfig ann =
  REPLConfig
    { handleInput :: String -> Either String (DocM ann ())
    , showAnn     :: ann -> String
    }


jsREPL :: REPLConfig ann -> IO ()
jsREPL config = do
  Just doc    <- currentDocument
  Just window <- currentWindow
  Just body   <- getBody doc

  transcript <- createElement doc "div" HTMLElement
  E.setAttribute transcript "id" "transcript"
  input      <- createElement doc "input" HTMLInputElement
  button     <- createElement doc "input" HTMLInputElement

  setType button "button"
  setValue button (Just "Go")

  outputRecord <- newIORef [] :: IO (IORef [DocM ann ()])

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
              o <- execDoc doc transcript (pure . showAnn config) d
              render doc transcript (showAnn config) o
              br <- createElement doc "br" HTMLBRElement
              appendChild transcript (Just br)
            putMVar renderLock ()
          Nothing -> pure ()

  let addOut doc = forkIO $
        atomicModifyIORef' outputRecord (\ old -> (doc : old, ()))


  let save line =
        case handleInput config line of
          Left err -> liftIO $ do
            w <- currentWindow
            case w of
              Just win -> liftIO $ alert win err
              Nothing -> liftIO $ putStrLn err
          Right doc ->
            liftIO $ do
              addOut doc
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

jsREPLMulti config = do
  Just doc    <- currentDocument
  Just window <- currentWindow
  Just body   <- getBody doc

  transcript <- createElement doc "div" HTMLElement
  E.setAttribute transcript "id" "transcript"
  input      <- createElement doc "textarea" HTMLTextAreaElement
  button     <- createElement doc "input" HTMLInputElement

  setType button "button"
  setValue button (Just "Go")

  outputRecord <- newIORef [] :: IO (IORef [DocM ann ()])

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
              o <- execDoc doc transcript (pure . showAnn config) d
              render doc transcript (showAnn config) o
              br <- createElement doc "br" HTMLBRElement
              appendChild transcript (Just br)
            putMVar renderLock ()
          Nothing -> pure ()

  let addOut doc = forkIO $
        atomicModifyIORef' outputRecord (\ old -> (doc : old, ()))


  let save line =
        case handleInput config line of
          Left err -> liftIO $ do
            w <- currentWindow
            case w of
              Just win -> liftIO $ alert win err
              Nothing -> liftIO $ putStrLn err
          Right doc ->
            liftIO $ do
              addOut doc
              refreshOut
              TA.setValue input (Just "")
              return ()

  let userInput = TA.getValueUnsafe input :: EventM e t String

  on button E.click (userInput >>= save)
  on input E.keyPress $ do
    wh <- uiWhich
    shift <- getShiftKey =<< ask
    if not shift && wh == 13
      then (userInput >>= save)
      else return ()

  (Just w) <- currentWindow
  on w resize $ liftIO (refreshOut *> pure ())


  liftIO $ return ()



jsFile :: REPLConfig ann -> IO ()
jsFile config = do
  Just doc    <- currentDocument
  Just window <- currentWindow
  Just body   <- getBody doc

  transcript <- createElement doc "div" HTMLElement
  E.setAttribute transcript "id" "transcript"
  file       <- createElement doc "input" HTMLInputElement

  setType file "file"

  outputRecord <- newIORef (pure ()) :: IO (IORef (DocM ann ()))

  appendChild body (Just transcript)
  appendChild body (Just file)
  threadDelay 1

  renderLock <- newMVar ()

  let refreshOut = forkIO $ do
        ok <- tryTakeMVar renderLock
        case ok of
          Just () -> do
            E.setInnerHTML transcript (Just "")
            out <- readIORef outputRecord
            o <- execDoc doc transcript (pure . showAnn config) out
            render doc transcript (showAnn config) o
            br <- createElement doc "br" HTMLBRElement
            appendChild transcript (Just br)
            putMVar renderLock ()
          Nothing -> pure ()

  let replaceOut doc = forkIO $
        atomicModifyIORef' outputRecord (\ old -> (doc, ()))


  let save line =
        case handleInput config line of
          Left err -> liftIO $ do
            w <- currentWindow
            case w of
              Just win -> liftIO $ alert win err
              Nothing -> liftIO $ putStrLn err
          Right doc ->
            liftIO $ do
              replaceOut doc
              refreshOut
              return ()

  let userInput = getValueUnsafe file :: EventM e t String

  on file E.change $ do
    liftIO $ putStrLn "file"
    fileList <- getFilesUnsafe file
    len <- liftIO $ getLength fileList
    if (len < 1)
      then liftIO $ putStrLn "No file"
      else do
        Just theFile <- item fileList 0
        (getName theFile) >>= (liftIO . putStrLn)
        reader <- newFileReader
        liftIO $ on reader loadEnd $ do
          Just tgt <- target
          res <- getResult tgt
          Just str <- liftIO $ fromJSVal res
          liftIO $ putStrLn str
          save str
          return ()
        readAsText reader (Just theFile) "UTF-8"

  
  (Just w) <- currentWindow
  on w resize $ liftIO (refreshOut *> pure ())


  liftIO $ return ()

createElement :: ( IsGObject b
                 , IsDocument self
                 , ToJSString tagName
                 , MonadIO m) =>
                 self -> tagName -> (JSVal -> b) -> m b
createElement doc tagName tagType =
  createElementUnsafe doc (Just tagName) >>= unsafeCastTo tagType

createTextNodeUnsafe' :: (MonadIO m, IsDocument self) => self -> String -> m Text
createTextNodeUnsafe' = createTextNodeUnsafe
