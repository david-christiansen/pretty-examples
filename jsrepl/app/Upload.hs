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
import Lib
import Text.PrettyPrint.Final

---

padCenter :: (MonadPretty w ann fmt m, Fractional w) => T.Text -> T.Text -> m ()
padCenter bigger smaller = do
  w  <- measureText bigger
  w' <- measureText smaller
  let padding = max 0 (w - w') / 2
  space padding
  text smaller
  space padding

padLeft :: (MonadPretty w ann fmt m, Fractional w) => T.Text -> T.Text -> m ()
padLeft bigger smaller = do
  w  <- measureText bigger
  w' <- measureText smaller
  let padding = max 0 (w - w')
  text smaller
  space padding

padRight :: (MonadPretty w ann fmt m, Fractional w) => T.Text -> T.Text -> m ()
padRight bigger smaller = do
  w  <- measureText bigger
  w' <- measureText smaller
  let padding = max 0 (w - w')
  space padding
  text smaller
  
---

data HsAnn = HKeyword
           | HModuleName
           | HPragma
           | HTyCon
           | HTyVar
           | HCon
           | HKind
           | HVar
           | HConstraint
           | TODO
  deriving Show

kwd = annotate HKeyword . kwd'
  where kwd' "->" = rightArrow
        kwd' "<-" = leftArrow
        -- kwd' "::" = ann
        kwd' "forall" = forAll
        kwd' "=>" = fatRightArrow
        kwd' "\\" = lambda
        kwd' k = text k

parens p = grouped $ do
  text "("
  align p
  text ")"

processName "->" = "→"
processName "<-" = "←"
processName ">>=" = "≫="
processName ">>" = "≫"
processName "=<<" = "=≪"
processName "++" = "⧺"
processName "." = "∘"
processName xs = swapChar <$> xs
  where swapChar '\'' = '′' -- PRIME
        swapChar '*' = '∗' -- ASTERISK OPERATOR
        swapChar x = x


rightArrow = text "→"
fatRightArrow = text "⇒"
leftArrow = text "←"
--ann = text "∷"
forAll = text "∀"
lambda = text "λ"

class PP a where
  pp :: a -> DocM HsAnn ()

instance PP ModuleName where
  pp (ModuleName n) =
    annotate HModuleName $ text (T.pack n)

todo :: Show a => a -> DocM HsAnn ()
todo x = annotate TODO (text (T.pack (show x)))

instance PP Name where
  pp (Ident str) = text (T.pack (processName str))
  pp (Symbol str) = text (T.pack (processName str))

instance PP ModulePragma where
  pp (LanguagePragma loc ns) = annotate HPragma $ do
    i <- spaceWidth
    text "{-# LANGUAGE"
    space i
    align $ hvsep (map pp ns)
    space i
    text "#-}"
    newline
  pp x = todo x

dot = text "."

instance PP SpecialCon where
  pp UnitCon = annotate HTyCon $ text "()"
  pp ListCon = annotate HTyCon $ text "[]"
  pp FunCon  = annotate HTyCon $ text "->"
  pp (TupleCon Boxed n) = annotate HTyCon $ do
    text "("
    for_ [i | i <- [0..n]] $ const (text ",")
    text ")"
  pp (TupleCon Unboxed n) = annotate HTyCon $ do
    text "(#"
    for_ [i | i <- [0..n]] $ const (text ",")
    text "#)"
  pp Cons = annotate HCon $ text ":"
  pp UnboxedSingleCon = annotate HCon $ text "(# #)"

instance PP QName where
  pp (UnQual n) = pp n
  pp (Qual mn n) = pp mn >> dot >> pp n
  pp (Special con) = pp con

instance PP ExportSpec where
  pp (EThingAll e) = pp e >> text "(..)"
  pp (EVar e) = annotate HVar $ pp e
  pp (EAbs NoNamespace n) = pp n
  pp (EAbs ns n) = (todo $ EAbs ns n)
  pp e = todo e

instance PP ImportDecl where
  pp (ImportDecl loc mod qualp srcp safep pkgn asMod specs) = do
    i <- spaceWidth
    kwd "import"
    space i
    if qualp then kwd "qualified" >> space i else pure ()
    pp mod
    case specs of
      Nothing -> pure ()
      Just (hidep, imports) -> do
        if hidep then kwd "hiding" >> space i else pure ()
        collection (text "(") (text ")") (text ",") $
          map pp imports
    for_ asMod $ \n -> do
      space i
      kwd "as"
      space i
      pp n

instance PP ImportSpec where
  pp (IVar n) = annotate HVar $ pp n
  pp (IAbs ns n) = pp n -- TODO
  pp (IThingAll n) = pp n >> text "(..)"
  pp (IThingWith n cs) = do
    pp n
    collection (text "(") (text ")") (text ",") $
      map pp cs

instance PP CName where
  pp (VarName n) = annotate HVar $ pp n
  pp (ConName n) = annotate HCon $ pp n

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

declName :: Decl -> Maybe Name
declName (TypeDecl _ n _ _) = pure n
declName (DataDecl _ _ _ n _ _ _) = pure n
declName (TypeSig _ [n] _) = pure n
declName (FunBind (Match _ n _ _ _ _ : ms)) = pure n
declName (PatBind _ p _ _) = name p
  where name (PVar n) = pure n
        name (PApp _ (p:ps)) = name p
        name (PParen p) = name p
        name _ = empty
declName _ = empty

splitDecls [] = []
splitDecls (x : xs) =
  case declName x of
    Nothing -> [x] : splitDecls xs
    Just n ->
      let (same, diff) = span ((== Just n) . declName) xs
      in (x : same) : splitDecls diff

instance PP Module where
  pp (Module loc name pragmas warnings exports imports decls) = do
    traverse_ pp pragmas
    if not (isEmpty pragmas) then newline else pure ()
    hsep [ annotate HKeyword $ text "module"
         , pp name
         , exportDoc
         , annotate HKeyword $ text "where"]
    newline
    for_ imports $ \i -> do pp i ; newline
    newline
    vsep $ map (>> newline) $ map (vsep . map pp) (splitDecls decls)
   where exportDoc =
           case exports of
             Nothing -> pure ()
             Just es -> grouped $ do
               align $ collection (text "(") (text ")") (text ",") (map pp es)

instance PP DataOrNew where
  pp DataType = kwd "data"
  pp NewType = kwd "newtype"

instance PP Decl where
  pp (TypeSig loc ns ty) = do
    i <- spaceWidth
    collection (return ()) (return ()) (text ",") (map (annotate HVar . pp) ns)
    space i
    kwd "::"
    space i
    align $ grouped $ pp ty
  pp (DataDecl loc dataOrNew ctx n tyvars ctors derives) = do
    em <- emWidth
    i <- spaceWidth
    grouped $ hvsep $
      [pp dataOrNew] ++
      (if isEmpty ctx then [] else map pp ctx ++ [kwd "=>"]) ++
      [annotate HTyCon $ pp n] ++
      map (annotate HTyVar . pp) tyvars
    space i
    align $ do
      kwd "="
      space i
      let sep = annotate HKeyword $ padCenter  "=" "|"
      sequence_ (intersperse (ifFlat (space i) newline >> sep >> space i)
                             (map (expr . pp) ctors))
    if isEmpty derives
      then pure ()
      else nest (2 * em) $ do
        newline
        kwd "deriving"
        space i
        collection (text "(") (text ")") (text ",") (map ppDerive derives)
  pp (PatBind loc pat rhs bnds) = do
    em <- emWidth
    hvsep [ hsep [pp pat , kwd "="]
          , expr $ nest (2 * em) $ pp rhs
          ]
    for_ bnds whereBlock
  pp (FunBind xs) = vsep (map pp xs)
  pp (TypeDecl loc name args rhs) =
    hsep [ kwd "type"
         , annotate HTyCon $ pp name
         , grouped $ hvsep (map pp args)
         , kwd "="
         , pp rhs
         ]
  pp (InstDecl loc overlap tyvars ctx n args body) = do
    em <- emWidth
    hsep $ [kwd "instance"] ++
           over ++
           [grouped $ hvsep (map pp tyvars)] ++
           (if isEmpty ctx
              then []
              else [hsep [hsep (map pp ctx), kwd "=>"]]) ++
           [ annotate HConstraint $ pp n
           , hsep (map pp args)
           , kwd "where"
           ]
    nest (2 * em) $ do
      newline
      vsep (map pp body)
      where over =
              map (annotate HPragma) $
              case overlap of
                Nothing -> []
                Just Overlap -> [text "{-# OVERLAPPING #-}"]
                Just NoOverlap -> [text "{-# NO_OVERLAP #-}"]
                Just Incoherent -> [text "{-# INCOHERENT #-}"]
  pp (ClassDecl loc ctx n args fundeps body) = do
    em <- emWidth
    i <- spaceWidth
    kwd "class"
    space i
    expr $
      hvsep $ (if isEmpty ctx
                 then []
                 else
                  [ align $
                      collection (kwd "(") (kwd ")" >> space i >> kwd "=>") (kwd ",") $
                        map pp ctx
                  ]) ++
              [ expr $ hvsep [ hsep [ annotate HConstraint $ pp n
                                    , align $ grouped $ hvsep $ map pp args
                                    ]
                             , expr $ hvsep $ map pp fundeps
                             , kwd "where"
                             ]
              ]
    nest (2 * em) $ do
      newline
      vsep (map pp body)
  pp d = todo d

instance PP FunDep where
  pp (FunDep ls rs) = do
    hsep [ kwd "|"
         , align $ grouped $ hvsep [ hsep $ (map (annotate HTyVar . pp) ls) ++ [kwd "->"]
                                   , hsep (map (annotate HTyVar . pp) rs)
                                   ]
         ]

instance PP ClassDecl where
  pp (ClsDecl d) = pp d
  pp x = todo x

instance PP InstDecl where
  pp (InsDecl d) = pp d
  pp (InsType loc t1 t2) =
    grouped $
    hvsep [ hsep [ kwd "type"
                 , pp t1
                 , kwd "="
                 ]
          , align $ pp t2
          ]
  pp x = todo x

whereBlock contents = do
  em <- emWidth
  nest em $ do
    newline
    nest em $ do
      kwd "where" >> newline
      pp contents


instance PP Match where
  pp (Match loc n pats ty rhs bnds) = do
    em <- emWidth
    nest (2 * em) $
      hvsep [ hsep [grouped $ hvsep $ [annotate HVar $ pp n] ++ map pp pats
                   , kwd "="
                   ]
            , expr $ pp rhs]
    for_ bnds whereBlock

instance PP Binds where
  pp (BDecls decls) = vsep (map pp decls)
  pp x = todo x

ppDerive (n, args) = do
  i <- spaceWidth
  annotate HConstraint $ pp n
  for_ args $ \a -> space i >> pp a

instance PP QualConDecl where
  pp (QualConDecl loc tyvars ctx ctor) = do
    i <- spaceWidth
    if isEmpty tyvars
      then return ()
      else grouped $ hvsep [ kwd "forall"
                           , hvsep (map pp tyvars)
                           , kwd "."
                           ]
    if isEmpty ctx
      then return ()
      else hsep [grouped (hvsep (map pp ctx)), kwd "=>"]
    pp ctor

instance PP ConDecl where
  pp (ConDecl n ts) = hsep [ annotate HCon $ pp n
                           , grouped $ hvsep (map pp ts)
                           ]
  pp (InfixConDecl t1 n t2) =
    grouped $ hvsep [ grouped (pp t1)
                    , annotate HCon (pp n)
                    , grouped (pp t2)
                    ]
  pp (RecDecl n fields) = do
    hsep [ annotate HCon (pp n)
         , expr $ alwaysBraces $
           [ hsep [ collection (return ()) (return ()) (text ",") (map pp ns)
                  , kwd "::"
                  , pp t
                  ]
           | (ns, t) <- fields
           ]
         ]

instance PP Kind where
  pp k = annotate HKind $ pp' k
    where pp' KindStar = annotate HKind (text "*")
          pp' (KindFn k1 k2) = do
            i <- spaceWidth
            pp' k1
            space i
            (text "->")
            space i
            pp' k2
          pp' (KindParen k) = grouped $ do
            text "("
            pp' k
            text ")"
          pp' (KindVar v) = pp v
          pp' (KindApp k1 k2) = grouped $ do
            i <- spaceWidth
            pp' k1
            space i
            pp' k2
          pp' (KindTuple ks) =
            collection (text "(") (text ")") (text ",") (map pp' ks)

instance PP TyVarBind where
  pp (KindedVar n k) = grouped $ do
    i <- spaceWidth
    text "("
    annotate HTyVar $ pp n
    space i
    kwd "::"
    space i
    pp k
    text ")"
  pp (UnkindedVar n) = annotate HTyVar $ pp n

instance PP Asst where
  pp (ClassA n tys) = do
    i <- spaceWidth
    annotate HConstraint $ pp n
    for_ tys $ \t -> space i >> pp t
  pp (ParenA a) = do
    text "("
    pp a
    text ")"
  pp (InfixA a1 n a2) =
    grouped $ hvsep [ pp a1
                    , pp n
                    , pp a2
                    ]
  pp (EqualP t1 t2) =
    grouped $ hvsep [ pp t1
                    , text "~"
                    , pp t2
                    ]
  pp x = todo x

instance PP Rhs where
  pp (UnGuardedRhs rhs) = pp rhs
  pp x = todo x

instance PP GuardedRhs where
  pp = todo

instance PP Pat where
  pp (PVar n) = annotate HVar $ pp n
  pp PWildCard = text "_"
  pp (PList ps) =
    collection (annotate HCon (text "[")) (annotate HCon (text "]")) (annotate HCon (text ",")) (map pp ps)
  pp (PParen p) = parens (pp p)
  pp (PInfixApp p1 op p2) =
    grouped $ hvsep [hsep [pp p1, pp op], pp p2]
  pp (PApp p1 p2s) =
    hsep [ pp p1
         , align $ grouped $ hvsep (map pp p2s)
         ]
  pp (PatTypeSig loc p t) = do
    i <- spaceWidth
    expr $ hvsep [hsep [pp p, kwd "::"], nest (2 * i) $ pp t]
  pp x = todo x

perhapsBraces docs =
  ifFlat (collection (text "{") (text "}") (text ";") docs)
         (align (vsep docs))

alwaysBraces docs = do
  i <- spaceWidth
  expr $  collection (text "{")
                     (text "}")
                     (text ",")
                     docs

instance PP Exp where
  pp (Var n) = annotate HVar $ pp n
  pp (Con n) = annotate HCon $ pp n
  pp (Lit l) = pp l
  pp (InfixApp e1 op e2) = do
    em <- emWidth
    grouped $ align $ nest em $ hvsep [hsep [pp e1, pp op], pp e2]
  pp (App e1 e2) = do
    i <- spaceWidth
    expr $ nest i $ hvsep [pp e1, pp e2]
  pp (NegApp e) = text "-" >> pp e
  pp (Lambda loc pats body) = do
    kwd "\\"
    align $ hvsep (map pp pats)
    kwd "->"
    grouped (pp body)
  pp (Let bnds body) = do
    i <- spaceWidth
    grouped $ hvsep [ do kwd "let"
                         space i
                         align (pp bnds)
                    , kwd "in"
                    , pp body
                    ]
  pp (If e1 e2 e3) = do
    grouped $ hvsep [ hsep [kwd "if", pp e1]
                    , hsep [kwd "then", pp e2]
                    , hsep [kwd "else", pp e3]
                    ]
  pp (MultiIf alts) = do
    hsep [kwd "if", align $ vsep $ map pp alts]
  pp (Case e alts) = do
    em <- emWidth
    i <- spaceWidth
    nest em $ grouped $ do
      hsep [ kwd "case"
           , pp e
           , kwd "of"
           ]
      ifFlat (space i) newline
      perhapsBraces (map (\a -> pp a) alts)
  pp (Do stmts) = do
    hsep [ kwd "do"
         , perhapsBraces (map pp stmts)
         ]
  pp (MDo stmts) = do
    kwd "mdo"
    perhapsBraces (map pp stmts)
  pp (Tuple Boxed es) =
    collection (annotate HCon $ text "(") (annotate HCon $ text ")") (annotate HCon $ text ",") (map pp es)
  pp (Tuple Unboxed es) =
    collection (annotate HCon $ text "(#") (annotate HCon $ text "#)") (annotate HCon $ text ",") (map pp es)
  pp (TupleSection Boxed es) =
    collection (annotate HCon $ text "(") (annotate HCon $ text ")") (annotate HCon $ text ",") (map (maybe (pure ()) pp) es)
  pp (TupleSection Unboxed es) =
    collection (annotate HCon $ text "(#") (annotate HCon $ text "#)") (annotate HCon $ text ",") (map (maybe (pure ()) pp) es)
  pp (List es) =
    collection (annotate HCon $ text "[") (annotate HCon $ text "]") (annotate HCon $ text ",") (map pp es)
  pp (ParArray es) =
    collection (annotate HCon $ text "[:") (annotate HCon $ text ":]") (annotate HCon $ text ",") (map pp es)
  pp (Paren e) = do
    parens $ pp e
  pp (LeftSection e op) =
    parens $ hvsep [pp e, pp op]
  pp (RightSection op e) =
    parens $ hvsep [pp op, pp e]
  pp (RecConstr n fields) =
    hsep [ annotate HCon $ pp n
         , alwaysBraces $ map pp fields
         ]
  pp (RecUpdate e fields) =
    hsep [ pp e
         , alwaysBraces $ map pp fields
         ]
  pp e = todo e

instance PP Alt where
  pp (Alt loc pat rhs bnds) = do
    hsep [ pp pat
         , kwd "->"
         , align $ grouped $ pp rhs]
    em <- emWidth
    for_ bnds whereBlock

instance PP FieldUpdate where
  pp (FieldUpdate n e) = hsep [pp n, kwd "=", align $ pp e]
  pp (FieldPun n) = pp n
  pp FieldWildcard = text ".."

instance PP Stmt where
  pp (Generator loc pat expr) = do
    em <- emWidth
    grouped $ nest (2 * em) $
      hvsep [ hsep [pp pat, kwd "<-"]
            , pp expr
            ]
  pp (Qualifier e) = pp e
  pp (LetStmt bnds) = do
    hsep [ kwd "let"
         , align (pp bnds)
         ]
  pp x = todo x

instance PP Literal where
  pp (Char c) = text (T.pack (show c)) -- TODO escape
  pp (String s) = text (T.pack (show s)) -- TODO escape
  pp (Int i) = text (T.pack (show i))
  pp (Frac r) = text (T.pack (show r))
  pp x = todo x

instance PP QOp where
  pp (QVarOp n) = pp n
  pp (QConOp n) = annotate HCon $ pp n

instance PP Type where
  pp (TyForall binds ctx ty) = do
    i <- spaceWidth
    case binds of
      Nothing -> pure ()
      Just bs -> do
        kwd "forall"
        space i
        hsep (map pp bs)
        kwd "."
    case ctx of
      [] -> expr $ pp ty
      [t] -> hvsep [ hsep [ expr $ pp t
                         , kwd "=>"
                         ]
                   , expr $ pp ty
                   ]
      _  -> do
        expr $ hvsep [ align $ grouped $
                       hsep [ collection (text "(") (text ")") (text ",") (map pp ctx)
                            , kwd "=>"
                            ]
                     , expr $ pp ty
                     ]
  pp (TyParen t) = parens (pp t)
  pp t@(TyFun _ _) = do
    i <- spaceWidth
    expr $ sequence_ $ intersperse (space i >> arr >> ifFlat (space i) newline) (map pp $ unFun t)
    where unFun (TyFun a b) = a : unFun b
          unFun other = [other]
          arr = annotate HTyCon rightArrow
  pp (TyList t) = grouped $ do
    annotate HTyCon $ text "["
    pp t
    annotate HTyCon $ text "]"
  pp (TyCon n) = annotate HTyCon $ pp n
  pp (TyVar n) = annotate HTyVar $ pp n
  pp (TyApp t1 t2) =
    hsep [pp t1, pp t2]
  pp (TyTuple Boxed ts) =
    collection (annotate HTyCon $ text "(")
               (annotate HTyCon $ text ")")
               (annotate HTyCon $ text ",")
               (map pp ts)
  pp (TyTuple Unboxed ts) =
    collection (annotate HTyCon $ text "(#")
               (annotate HTyCon $ text "#)")
               (annotate HTyCon $ text ",")
               (map pp ts)

  pp t = todo t

config :: REPLConfig HsAnn
config =
  REPLConfig
    { handleInput = hI
    , showAnn = show
    }
  where hI f =
          case parseFileContents f of
            ParseFailed loc err -> Left err
            ParseOk ast ->
              Right $ pp ast

main :: IO ()
main = jsFile config



