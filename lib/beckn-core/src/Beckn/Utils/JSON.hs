{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.JSON where

import Data.Aeson (Options (..), Value (..), defaultOptions)
import Data.HashMap.Strict (unions)
import qualified Data.Text as T
import EulerHS.Prelude hiding (Type)
import Language.Haskell.TH

replaceUnderscores :: Text -> Text
replaceUnderscores = T.replace "_" "-"

replaceUnderscoresString :: String -> String
replaceUnderscoresString = T.unpack . replaceUnderscores . T.pack

constructorsWithHyphens :: Options
constructorsWithHyphens =
  defaultOptions
    { constructorTagModifier = replaceUnderscoresString
    }

constructorsToLowerOptions :: Options
constructorsToLowerOptions =
  defaultOptions
    { constructorTagModifier = T.unpack . T.toLower . T.pack
    }

constructorsWithHyphensToLowerOptions :: Options
constructorsWithHyphensToLowerOptions =
  defaultOptions
    { constructorTagModifier = T.unpack . replaceUnderscores . T.toLower . T.pack
    }

uniteObjects :: [Value] -> Value
uniteObjects = Object . unions . map unwrapObject
  where
    unwrapObject (Object o) = o
    unwrapObject e = error ("expected Object, got " <> show e)

deriveJSON :: Name -> Name -> Q [InstanceDec]
deriveJSON name opt = deriveJSON' (pure $ ConT name) (pure $ VarE opt)

deriveJSON' :: TypeQ -> ExpQ -> Q [InstanceDec]
deriveJSON' name opt =
  [d|
    instance FromJSON $name where
      parseJSON = genericParseJSON $opt

    instance ToJSON $name where
      toJSON = genericToJSON $opt
    |]
