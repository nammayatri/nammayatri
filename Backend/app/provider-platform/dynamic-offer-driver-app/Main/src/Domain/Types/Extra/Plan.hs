{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Extra.Plan where

import qualified Control.Lens as L
import Data.Aeson
import qualified Data.List as List
import Data.OpenApi
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.GenericPretty
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum, mkBeamInstancesForEnumAndList)

data ServiceProvider = CAUTIO
  deriving stock (Eq, Ord, Generic, Show, Read)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Show ServiceNames where
  show (YATRI_SUBSCRIPTION) = "YATRI_SUBSCRIPTION"
  show (YATRI_RENTAL) = "YATRI_RENTAL"
  show (DASHCAM_RENTAL serviceProvider) = "DASHCAM_RENTAL_" <> T.unpack (show serviceProvider)

data ServiceNames
  = YATRI_SUBSCRIPTION
  | YATRI_RENTAL
  | DASHCAM_RENTAL ServiceProvider
  deriving (Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''ServiceNames)

instance ToParamSchema ServiceNames where
  toParamSchema f =
    mempty
      & title L.?~ "ServiceNames"
      & type_ L.?~ OpenApiString
      & format L.?~ (show f)

instance FromHttpApiData ServiceNames where
  parseUrlPiece a = readEither a

instance ToHttpApiData ServiceNames where
  toQueryParam serviceName = show serviceName

instance Read ServiceNames where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (YATRI_SUBSCRIPTION, "")
            | List.isPrefixOf "YATRI_SUBSCRIPTION" r
          ]
            ++ [ (YATRI_RENTAL, "")
                 | List.isPrefixOf "YATRI_RENTAL" r
               ]
            ++ [ (DASHCAM_RENTAL (read r1 :: ServiceProvider), "")
                 | r1 <- stripPrefix "DASHCAM_RENTAL_" r
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

instance Show PlanBaseAmount where
  show (PERRIDE_BASE amount) = "PERRIDE_" <> T.unpack (show amount)
  show (DAILY_BASE amount) = "DAILY_" <> T.unpack (show amount)
  show (WEEKLY_BASE amount) = "WEEKLY_" <> T.unpack (show amount)
  show (MONTHLY_BASE amount) = "MONTHLY_" <> T.unpack (show amount)

data PlanBaseAmount
  = PERRIDE_BASE HighPrecMoney
  | DAILY_BASE HighPrecMoney
  | WEEKLY_BASE HighPrecMoney
  | MONTHLY_BASE HighPrecMoney
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable PlanBaseAmount

$(mkBeamInstancesForEnum ''PlanBaseAmount)

instance Read PlanBaseAmount where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (PERRIDE_BASE (read r1 :: HighPrecMoney), "")
            | r1 <- stripPrefix "PERRIDE_" r
          ]
            ++ [ (DAILY_BASE (read r1 :: HighPrecMoney), "")
                 | r1 <- stripPrefix "DAILY_" r
               ]
            ++ [ (WEEKLY_BASE (read r1 :: HighPrecMoney), "")
                 | r1 <- stripPrefix "WEEKLY_" r
               ]
            ++ [ (MONTHLY_BASE (read r1 :: HighPrecMoney), "")
                 | r1 <- stripPrefix "MONTHLY_" r
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

newtype RideCountBasedFeePolicyConfig = RideCountBasedFeePolicyConfig [RideCountBasedFeePolicy] deriving (Generic, ToJSON, FromJSON, Show)

data RideCountBasedFeePolicy = RideCountBasedFeePolicy
  { baseRideCount :: Int,
    platformFee :: Money,
    platformFeeCgst :: HighPrecMoney,
    platformFeeSgst :: HighPrecMoney,
    perRideFee :: Money,
    perRideCgst :: HighPrecMoney,
    perRideSgst :: HighPrecMoney
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)
