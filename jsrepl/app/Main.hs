module Main (main) where

import Control.Monad (guard)
--import Control.Monad.Reader (ReaderT(..), ask, runReader)
import Control.Monad.IO.Class (MonadIO(..))
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Document (createElementUnsafe, createTextNodeUnsafe, getBody)
import GHCJS.DOM.EventM (EventM, event, on, uiWhich)
import GHCJS.DOM.HTMLInputElement (getValueUnsafe, setValue, setType)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.Types ( Document
                       , HTMLInputElement(..)
                       , HTMLDivElement(..)
                       , HTMLBRElement(..)
                       , IsDocument
                       , IsNode
                       , Text
                       , unsafeCastTo)

import qualified GHCJS.DOM.Element as E (click, keyPress)

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

  transcript <- createElement doc "div" HTMLDivElement
  input      <- createElement doc "input" HTMLInputElement
  button     <- createElement doc "input" HTMLInputElement
  setType button "button"

  appendChild body (Just transcript)
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


