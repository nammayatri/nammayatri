{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.DocumentOnboardingStage where

import Data.Aeson (Value (Null), object, withObject, (.:), (.=))
import qualified Data.List as List
import qualified Data.Text as T
import Kernel.Prelude
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data LegalStructure
  = IndividualLegalStructure
  | LegalEntityStructure
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentOnboardingStage
  = DriverOnboarding
  | VehicleDetailsStage
  | OperatorPermit
  | TaxAndLegal LegalStructure
  | BankDetails
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToSchema)

instance Show DocumentOnboardingStage where
  show DriverOnboarding = "DriverOnboarding"
  show VehicleDetailsStage = "VehicleDetailsStage"
  show OperatorPermit = "OperatorPermit"
  show (TaxAndLegal legalStructure) = "TaxAndLegal_" <> T.unpack (show legalStructure)
  show BankDetails = "BankDetails"

instance Read DocumentOnboardingStage where
  readsPrec d' =
    readParen
      (d' > appPrec)
      ( \r ->
          [(TaxAndLegal v1, r2) | r1 <- stripPrefix "TaxAndLegal_" r, (v1, r2) <- readsPrec (appPrec + 1) r1]
            ++ [(DriverOnboarding, r1) | r1 <- stripPrefix "DriverOnboarding" r]
            ++ [(VehicleDetailsStage, r1) | r1 <- stripPrefix "VehicleDetailsStage" r]
            ++ [(OperatorPermit, r1) | r1 <- stripPrefix "OperatorPermit" r]
            ++ [(BankDetails, r1) | r1 <- stripPrefix "BankDetails" r]
      )
    where
      appPrec = 10 :: Int
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

instance ToJSON DocumentOnboardingStage where
  toJSON DriverOnboarding = object ["tag" .= ("DriverOnboarding" :: Text), "contents" .= Null]
  toJSON VehicleDetailsStage = object ["tag" .= ("VehicleDetailsStage" :: Text), "contents" .= Null]
  toJSON OperatorPermit = object ["tag" .= ("OperatorPermit" :: Text), "contents" .= Null]
  toJSON (TaxAndLegal legalStructure) = object ["tag" .= ("TaxAndLegal" :: Text), "contents" .= legalStructure]
  toJSON BankDetails = object ["tag" .= ("BankDetails" :: Text), "contents" .= Null]

instance FromJSON DocumentOnboardingStage where
  parseJSON = withObject "DocumentOnboardingStage" $ \v -> do
    tag <- v .: "tag"
    case tag :: Text of
      "DriverOnboarding" -> pure DriverOnboarding
      "VehicleDetailsStage" -> pure VehicleDetailsStage
      "OperatorPermit" -> pure OperatorPermit
      "TaxAndLegal" -> TaxAndLegal <$> v .: "contents"
      "BankDetails" -> pure BankDetails
      _ -> fail $ "Unknown DocumentOnboardingStage tag: " <> T.unpack tag

$(mkBeamInstancesForEnum ''DocumentOnboardingStage)
