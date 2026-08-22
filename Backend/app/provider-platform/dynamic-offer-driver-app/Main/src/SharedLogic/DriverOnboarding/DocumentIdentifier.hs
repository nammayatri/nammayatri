module SharedLogic.DriverOnboarding.DocumentIdentifier
  ( removeSpaceAndDash,
    preProcessDocumentIdentifier,
    normalizeDocumentIdentifier,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude

removeSpaceAndDash :: Text -> Text
removeSpaceAndDash = T.replace "-" "" . T.replace " " ""

preProcessDocumentIdentifier :: DTC.TransporterConfig -> Text -> Text
preProcessDocumentIdentifier transporterConfig
  | transporterConfig.preProcessDocumentIdentifiers = removeSpaceAndDash
  | otherwise = identity

normalizeDocumentIdentifier :: DTC.TransporterConfig -> Text -> Text
normalizeDocumentIdentifier transporterConfig
  | transporterConfig.preProcessDocumentIdentifiers = T.toUpper . removeSpaceAndDash
  | otherwise = identity
