{-# LANGUAGE TemplateHaskell #-}

module Alchemist.DSL.Syntax.Transformer where

import Control.Lens hiding (noneOf)
import Data.HashMap.Strict (HashMap)
import Kernel.Prelude

data TransformerTT = TransformerTT
  { _name :: Text,
    _fromTypes :: [Text],
    _paramNames :: [Text],
    _toType :: Text,
    _outputTypeBindings :: [(Text, Text)],
    _pureMapping :: [(Text, Text)],
    _impureMapping :: [(Text, Text)]
  }
  deriving (Show)

$(makeLenses ''TransformerTT)

data Transformers = Transformers
  { _moduleName :: Text,
    _imports :: HashMap Text Text,
    _functions :: [TransformerTT],
    _monads :: [Text]
  }
  deriving (Show)

$(makeLenses ''Transformers)
