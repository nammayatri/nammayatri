module API.Types.ProviderPlatform.Fleet.Endpoints.OnboardingExtra where

import qualified Control.Lens as L
import Data.Aeson (Value (Null), eitherDecode, encode, object, withObject, (.:), (.=))
import Data.OpenApi (OpenApiType (OpenApiString), ToParamSchema (..), format, title, type_)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

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

instance ToParamSchema DocumentOnboardingStage where
  toParamSchema f =
    mempty
      & title L.?~ "DocumentOnboardingStage"
      & type_ L.?~ OpenApiString
      & format L.?~ show f

$(mkHttpInstancesForEnum ''DocumentOnboardingStage)
