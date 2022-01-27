module Beckn.Utils.JSON where

import Beckn.Utils.Text (recursiveStrip)
import Data.Aeson (Options (..), SumEncoding (ObjectWithSingleField, UntaggedValue), Value (..), defaultOptions)
import Data.HashMap.Strict (size, unions)
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

constructorsWithHyphensUntagged :: Options
constructorsWithHyphensUntagged =
  defaultOptions
    { constructorTagModifier = replaceUnderscoresString,
      sumEncoding = UntaggedValue
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
    { fieldLabelModifier = unpack . replace "_" "/" . pack
    }

uniteObjects :: [Value] -> Value
uniteObjects values =
  let result = unions objects
   in if size result == sumOfSizes
        then Object result
        else error ("duplication fields in " <> show values)
  where
    objects = map unwrapObject values
    unwrapObject (Object o) = o
    unwrapObject e = error ("expected Object, got " <> show e)
    sumOfSizes = sum $ map size objects

objectWithSingleFieldParsing :: (String -> String) -> Options
objectWithSingleFieldParsing constructorMapping =
  defaultOptions
    { sumEncoding = ObjectWithSingleField,
      constructorTagModifier = constructorMapping
    }

stripPrefixUnderscoreIfAny :: Options
stripPrefixUnderscoreIfAny =
  defaultOptions
    { fieldLabelModifier = recursiveStrip
    }

untaggedValue :: Options
untaggedValue =
  defaultOptions
    { sumEncoding = UntaggedValue
    }