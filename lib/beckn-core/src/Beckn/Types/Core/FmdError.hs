module Beckn.Types.Core.FmdError
  ( FmdError (..),
    fmdErrorCode,
    fmdErrorDesc,
    allFmdErrors,
    fromFmdError,

    -- * Constants
    pickupLocationNotServicable,

    -- * Internals
    fmdErrorBareCodeText,
  )
where

import Beckn.Types.Core.Error
import Beckn.Utils.Common
import Data.List (lookup)
import EulerHS.Prelude
import qualified Text.Show

newtype FmdError = FmdError {fmdErrorBareCode :: Word}
  deriving (Eq, Ord, Generic, ToJSON)

instance Show FmdError where
  show err =
    "Error " <> toString (fmdErrorCode err) <> ": " <> toString (fmdErrorDesc err)

fmdErrorBareCodeText :: FmdError -> Text
fmdErrorBareCodeText = padNumber 3 . fmdErrorBareCode

fmdErrorCode :: FmdError -> Text
fmdErrorCode = ("FMD" <>) . fmdErrorBareCodeText

fmdErrorDesc :: FmdError -> Text
fmdErrorDesc (FmdError bareCode) =
  lookup bareCode allFmdErrorsInfo
    ?: error ("Unknown fmd error code " <> show bareCode)

fromFmdError :: FmdError -> Error
fromFmdError fmd =
  Error
    { _type = "DOMAIN-ERROR",
      _code = fmdErrorCode fmd,
      _path = Nothing,
      _message = Just $ fmdErrorDesc fmd
    }

allFmdErrors :: [FmdError]
allFmdErrors = map (FmdError . fst) allFmdErrorsInfo

pickupLocationNotServicable :: FmdError
pickupLocationNotServicable = FmdError 1

allFmdErrorsInfo :: [(Word, Text)]
allFmdErrorsInfo =
  [ (1, "Pickup location not serviceable"),
    (2, "Drop location not serviceable"),
    (3, "Pickup time not serviceable"),
    (4, "Drop time not serviceable"),
    (5, "Package too large"),
    (6, "Package too unstable"),
    (7, "Package too heavy"),
    (8, "Package too dangerous"),
    (9, "Agents unavailable"),
    (10, "Unable to contact pickup poc"),
    (11, "Unable to contact drop poc"),
    (12, "Amount too large for COD"),
    (13, "Package value too high"),
    (14, "Package category unserviceable"),
    (15, "Package damaged"),
    (16, "Package contents too many")
  ]
