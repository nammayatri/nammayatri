{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Beckn.Utils.GenericPretty
  ( PrettyShow (..),
    Showable (..),
    StrWrap (..),
    genericPrettyShow,
    defaultOptionsL,
    consModifier,
    LayoutValue,
    defaultPretty,
    textPretty,
    layoutStr,
    prettyShowViaJSON,
  )
where

import Beckn.Prelude
import Data.Aeson
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HMS
import Data.Scientific
import qualified Data.Text as Text (Text, pack, unpack)
import qualified Data.Time as Time
import qualified Data.Vector as V
import Data.Void (Void, absurd)
import GHC.Generics

enclose, encloseSq :: String -> String
enclose s = '{' : s ++ "}"
encloseSq s = '[' : s ++ "]"

data OptionsL = OptionsL
  { labelModifier :: String -> String,
    consModifier :: String -> String
  }

defaultOptionsL :: OptionsL
defaultOptionsL =
  OptionsL
    { labelModifier = defaultModif,
      consModifier = defaultConsModif
    }

defaultModif :: String -> String
defaultModif x@('_' : ys) =
  case dropWhile (/= '_') ys of
    ('_' : zs) -> zs
    _ -> x
defaultModif x = x

defaultConsModif :: String -> String
defaultConsModif = identity

data LayoutUnit
  = LayoutUnit String LayoutValue
  deriving (Show, Eq)

data LayoutValue
  = LStr String
  | LLay String Layout
  | LEmpty
  | LJSON String
  deriving (Show, Eq)

layoutStr :: String -> LayoutValue
layoutStr = LStr

newtype Layout
  = Layout [LayoutUnit]
  deriving (Show, Eq)

lconcat :: Layout -> Layout -> Layout
lconcat (Layout l) (Layout r) = Layout $ l ++ r

defaultIndent, defaultWidth :: Int
defaultIndent = 4
defaultWidth = 80

numToIndent :: Int -> String
numToIndent ind = replicate (ind * defaultIndent) ' '

splitToFixedWidthWithIndent :: Int -> String -> [String]
splitToFixedWidthWithIndent ind s =
  let width = defaultWidth - ind * defaultIndent
   in splitToFixedWidth width s

splitToFixedWidth :: Int -> String -> [String]
splitToFixedWidth _ [] = []
splitToFixedWidth wid s =
  let (pref, suf) = splitAt wid s
   in pref : splitToFixedWidth wid suf

withIndent :: Int -> String -> String
withIndent ind str = numToIndent ind ++ str

defaultPretty :: (PrettyShow a) => a -> String
defaultPretty = prettyValue 0 . prettyShow

textPretty :: (PrettyShow a) => a -> Text.Text
textPretty = Text.pack . defaultPretty

prettyUnit :: Int -> LayoutUnit -> String
prettyUnit _ (LayoutUnit _ LEmpty) = ""
prettyUnit ind (LayoutUnit s val) =
  withIndent ind $ s ++ ": " ++ prettyValue ind val

prettyValue :: Int -> LayoutValue -> String
prettyValue _ (LStr s) = s ++ "\n"
prettyValue ind (LLay typ ls) =
  typ ++ "\n" ++ prettyLayout (ind + 1) ls
prettyValue ind (LJSON s) =
  ('\n' :) $
    unlines $
      map (withIndent $ ind + 1) $ splitToFixedWidthWithIndent ind s
prettyValue _ LEmpty = "empty\n"

prettyLayout :: Int -> Layout -> String
prettyLayout ind (Layout ls) = concatMap (prettyUnit ind) ls

class PrettyShow a where
  prettyShow :: a -> LayoutValue
  default prettyShow :: (Generic a, GPrettyShow (Rep a)) => a -> LayoutValue
  prettyShow = genericPrettyShow defaultOptionsL

genericPrettyShow ::
  (Generic a, GPrettyShow (Rep a)) =>
  OptionsL ->
  a ->
  LayoutValue
genericPrettyShow opts = gprettyShow opts . from

newtype Showable a
  = Showable a

instance (Show a) => PrettyShow (Showable a) where
  prettyShow (Showable x) = LStr $ show x

newtype StrWrap = StrWrap
  { unStrWrap :: String
  }

instance PrettyShow StrWrap where
  prettyShow s = LStr $ unStrWrap s

instance PrettyShow Int where
  prettyShow = LStr . show

instance PrettyShow Integer where
  prettyShow = LStr . show

instance PrettyShow Double where
  prettyShow = LStr . show

instance PrettyShow Text.Text where
  prettyShow = LStr . show

instance PrettyShow Bool where
  prettyShow = LStr . show

instance PrettyShow Time.Day where
  prettyShow = LStr . Time.formatTime Time.defaultTimeLocale "%F"

instance PrettyShow Void where
  prettyShow = absurd

instance PrettyShow BS.ByteString where
  prettyShow = LStr . show

instance PrettyShow BSL.ByteString where
  prettyShow = LStr . show

instance (PrettyShow a) => PrettyShow (Maybe a) where
  prettyShow (Just x) = prettyShow x
  prettyShow Nothing = LEmpty

instance (PrettyShow a, PrettyShow b) => PrettyShow (Either a b)

instance (PrettyShow a) => PrettyShow [a] where
  prettyShow [] = LEmpty
  prettyShow xs =
    LLay "{Array}" $ Layout $ foldr f [] $ zip [0 :: Int ..] xs
    where
      f (n, x) acc =
        LayoutUnit (encloseSq $ show n) (prettyShow x) : acc

instance PrettyShow (IO a) where
  prettyShow _ = LStr "<IO action>"

prettyShowViaJSON :: ToJSON a => a -> String
prettyShowViaJSON = defaultPretty . toJSON

instance PrettyShow Value where
  prettyShow (Object obj) = prettyShow obj
  prettyShow (Array arr') = prettyShow arr'
  prettyShow (String txt) = prettyShow txt
  prettyShow (Number num) = prettyShow num
  prettyShow (Bool bool) = prettyShow bool
  prettyShow Null = LEmpty

instance PrettyShow Object where
  prettyShow = LLay "" . Layout . map f . HMS.toList
    where
      f (fieldName, value) = LayoutUnit (Text.unpack fieldName) (prettyShow value)

instance (PrettyShow a) => PrettyShow (V.Vector a) where
  prettyShow = prettyShow . V.toList

instance PrettyShow Scientific where
  prettyShow = prettyShow . Showable

instance PrettyShow UTCTime where
  prettyShow = prettyShow . Showable

class GPrettyShow f where
  gprettyShow :: OptionsL -> f a -> LayoutValue

class GPrettyShowAux f where
  gprettyShowAux :: OptionsL -> f a -> Layout

class GPrettyShowIgnoreConstr f where
  gprettyShowIgnoreConstr :: OptionsL -> f a -> LayoutValue

instance (GPrettyShow f) => GPrettyShow (D1 d f) where
  gprettyShow opts (M1 x) = gprettyShow opts x

instance
  (GPrettyShowIgnoreConstr f, GPrettyShowIgnoreConstr g) =>
  GPrettyShow ((:+:) f g)
  where
  gprettyShow = gprettyShowIgnoreConstr

instance
  (GPrettyShowIgnoreConstr f, GPrettyShowIgnoreConstr g) =>
  GPrettyShowIgnoreConstr ((:+:) f g)
  where
  gprettyShowIgnoreConstr opts (L1 x) =
    gprettyShowIgnoreConstr opts x
  gprettyShowIgnoreConstr opts (R1 x) =
    gprettyShowIgnoreConstr opts x

instance
  (GPrettyShowIgnoreConstr f, Constructor c) =>
  GPrettyShowIgnoreConstr (C1 c f)
  where
  gprettyShowIgnoreConstr opts (M1 x) =
    gprettyShowIgnoreConstr opts x

instance (GPrettyShow f) => GPrettyShowIgnoreConstr (S1 c f) where
  gprettyShowIgnoreConstr opts (M1 x) = gprettyShow opts x

instance
  (Constructor c, GPrettyShowAux f) =>
  GPrettyShow (C1 c f)
  where
  gprettyShow opts m@(M1 x) =
    LLay (enclose $ consModifier opts $ conName m) $
      gprettyShowAux opts x

instance (PrettyShow c) => GPrettyShow (Rec0 c) where
  gprettyShow _ (K1 x) = prettyShow x

instance
  (GPrettyShowAux f, GPrettyShowAux g) =>
  GPrettyShowAux ((:*:) f g)
  where
  gprettyShowAux opts (x :*: y) =
    gprettyShowAux opts x `lconcat` gprettyShowAux opts y

instance (Selector s, GPrettyShow f) => GPrettyShowAux (S1 s f) where
  gprettyShowAux opts s@(M1 x) =
    Layout
      [ LayoutUnit
          (labelModifier opts $ selName s)
          (gprettyShow opts x)
      ]
