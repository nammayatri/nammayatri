module API.Types.ProviderPlatform.Fleet.Endpoints.OnboardingExtra where

import Data.Aeson (Value (Null), object, withObject, (.:), (.=))
import qualified Data.Text as T
import Kernel.Prelude

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
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToSchema)

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
