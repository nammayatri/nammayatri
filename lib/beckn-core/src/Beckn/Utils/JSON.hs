module Beckn.Utils.JSON where

import Data.Aeson (Options (..), Value (..), defaultOptions)
import Data.HashMap.Strict (unions)
import Data.Text (pack, replace, toLower, unpack)
import EulerHS.Prelude hiding (pack, unpack)

replaceUnderscores :: Text -> Text
replaceUnderscores = replace "_" "-"

replaceUnderscoresString :: String -> String
replaceUnderscoresString = unpack . replaceUnderscores . pack

constructorsWithHyphens :: Options
constructorsWithHyphens =
  defaultOptions
    { constructorTagModifier = replaceUnderscoresString
    }

constructorsToLowerOptions :: Options
constructorsToLowerOptions =
  defaultOptions
    { constructorTagModifier = unpack . toLower . pack
    }

constructorsWithHyphensToLowerOptions :: Options
constructorsWithHyphensToLowerOptions =
  defaultOptions
    { constructorTagModifier = unpack . replaceUnderscores . toLower . pack
    }

slashedRecordFields :: Options
slashedRecordFields =
  defaultOptions
    { fieldLabelModifier = \('_' : xs) -> unpack . replace "_" "/" . pack $ xs
    }

uniteObjects :: [Value] -> Value
uniteObjects = Object . unions . map unwrapObject
  where
    unwrapObject (Object o) = o
    unwrapObject e = error ("expected Object, got " <> show e)
