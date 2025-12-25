{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Dashboard.Common.DriverCoins
  ( module Dashboard.Common.DriverCoins,
    module ReExport,
  )
where

import Dashboard.Common as ReExport
import Data.Aeson
import qualified Data.List as List
import qualified Data.Vector as V
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
import qualified Text.Show (show)

data CoinMessage
  = CoinAdded
  | CoinSubtracted
  | FareRecomputation
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroRideType
  = ToMetro
  | FromMetro
  | FromOrToMetro
  | None
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverCoinsFunctionType
  = OneOrTwoStarRating
  | FiveStarRating
  | BookingCancellation
  | BookingCancellationPenalisaton
  | BookingCancellationCompensation
  | CustomerReferral
  | DriverReferral
  | PurpleRideCompleted
  | GoldTierRideCompleted
  | LeaderBoardTopFiveHundred
  | TrainingCompleted
  | BulkUploadFunction
  | BulkUploadFunctionV2 CoinMessage
  | MetroRideCompleted MetroRideType (Maybe Kernel.Prelude.Int)
  | RidesCompleted Kernel.Prelude.Int
  | QuizQuestionCompleted
  | BonusQuizCoins
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToSchema, ToJSON)

instance Show DriverCoinsFunctionType where
  show (MetroRideCompleted rideType Nothing) =
    "MetroRideCompleted " <> show rideType
  show (MetroRideCompleted rideType (Just coins)) =
    "MetroRideCompleted " <> show rideType <> " " <> show coins
  show OneOrTwoStarRating = "OneOrTwoStarRating"
  show FiveStarRating = "FiveStarRating"
  show BookingCancellation = "BookingCancellation"
  show BookingCancellationPenalisaton = "BookingCancellationPenalisaton"
  show BookingCancellationCompensation = "BookingCancellationCompensation"
  show CustomerReferral = "CustomerReferral"
  show DriverReferral = "DriverReferral"
  show PurpleRideCompleted = "PurpleRideCompleted"
  show GoldTierRideCompleted = "GoldTierRideCompleted"
  show LeaderBoardTopFiveHundred = "LeaderBoardTopFiveHundred"
  show TrainingCompleted = "TrainingCompleted"
  show BulkUploadFunction = "BulkUploadFunction"
  show QuizQuestionCompleted = "QuizQuestionCompleted"
  show BonusQuizCoins = "BonusQuizCoins"
  show (BulkUploadFunctionV2 msg) = "BulkUploadFunctionV2 " <> show msg
  show (RidesCompleted n) = "RidesCompleted " <> show n

-- These instance are for backward compatibility of the Old Events stored in the DB.
instance Read DriverCoinsFunctionType where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (RidesCompleted 1, r2)
            | r1 <- stripPrefix "RideCompleted" r,
              ((), r2) <- pure ((), r1)
          ]
            ++ [ (RidesCompleted 2, r2)
                 | r1 <- stripPrefix "TwoRidesCompleted" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (RidesCompleted 5, r2)
                 | r1 <- stripPrefix "FiveRidesCompleted" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (RidesCompleted 10, r2)
                 | r1 <- stripPrefix "TenRidesCompleted" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (RidesCompleted 8, r2)
                 | r1 <- stripPrefix "EightPlusRidesInOneDay" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (OneOrTwoStarRating, r2)
                 | r1 <- stripPrefix "OneOrTwoStarRating" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (FiveStarRating, r2)
                 | r1 <- stripPrefix "FiveStarRating" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (BookingCancellation, r2)
                 | r1 <- stripPrefix "BookingCancellation" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (BookingCancellationPenalisaton, r2)
                 | r1 <- stripPrefix "BookingCancellationPenalisaton" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (BookingCancellationCompensation, r2)
                 | r1 <- stripPrefix "BookingCancellationCompensation" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (CustomerReferral, r2)
                 | r1 <- stripPrefix "CustomerReferral" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (DriverReferral, r2)
                 | r1 <- stripPrefix "DriverReferral" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (PurpleRideCompleted, r2)
                 | r1 <- stripPrefix "PurpleRideCompleted" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (GoldTierRideCompleted, r2)
                 | r1 <- stripPrefix "GoldTierRideCompleted" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (LeaderBoardTopFiveHundred, r2)
                 | r1 <- stripPrefix "LeaderBoardTopFiveHundred" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (TrainingCompleted, r2)
                 | r1 <- stripPrefix "TrainingCompleted" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (BulkUploadFunction, r2)
                 | r1 <- stripPrefix "BulkUploadFunction" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (BulkUploadFunctionV2 msg, r2)
                 | r1 <- stripPrefix "BulkUploadFunctionV2 " r,
                   (msg, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MetroRideCompleted ToMetro (Just coins), r2)
                 | r1 <- stripPrefix "MetroRideCompleted ToMetro " r,
                   (coins, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MetroRideCompleted ToMetro Nothing, r1)
                 | r1 <- stripPrefix "MetroRideCompleted ToMetro" r,
                   r1 == ""
               ]
            ++ [ (MetroRideCompleted FromMetro (Just coins), r2)
                 | r1 <- stripPrefix "MetroRideCompleted FromMetro " r,
                   (coins, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MetroRideCompleted FromMetro Nothing, r1)
                 | r1 <- stripPrefix "MetroRideCompleted FromMetro" r,
                   r1 == ""
               ]
            ++ [ (MetroRideCompleted FromOrToMetro (Just coins), r2)
                 | r1 <- stripPrefix "MetroRideCompleted FromOrToMetro " r,
                   (coins, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (MetroRideCompleted FromOrToMetro Nothing, r1)
                 | r1 <- stripPrefix "MetroRideCompleted FromOrToMetro" r,
                   r1 == ""
               ]
            ++ [ (RidesCompleted v1, r2)
                 | r1 <- stripPrefix "RidesCompleted " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (QuizQuestionCompleted, r2)
                 | r1 <- stripPrefix "QuizQuestionCompleted" r,
                   ((), r2) <- pure ((), r1)
               ]
            ++ [ (BonusQuizCoins, r2)
                 | r1 <- stripPrefix "BonusQuizCoins" r,
                   ((), r2) <- pure ((), r1)
               ]
      )
    where
      app_prec = 9
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

instance FromJSON DriverCoinsFunctionType where
  parseJSON = withObject "DriverCoinsFunctionType" $ \obj -> do
    tag <- obj .: "tag"
    case tag :: String of
      "RideCompleted" -> pure $ RidesCompleted 1
      "TwoRidesCompleted" -> pure $ RidesCompleted 2
      "FiveRidesCompleted" -> pure $ RidesCompleted 5
      "EightPlusRidesInOneDay" -> pure $ RidesCompleted 8
      "TenRidesCompleted" -> pure $ RidesCompleted 10
      "OneOrTwoStarRating" -> pure OneOrTwoStarRating
      "FiveStarRating" -> pure FiveStarRating
      "BookingCancellation" -> pure BookingCancellation
      "BookingCancellationPenalisaton" -> pure BookingCancellationPenalisaton
      "BookingCancellationCompensation" -> pure BookingCancellationCompensation
      "CustomerReferral" -> pure CustomerReferral
      "DriverReferral" -> pure DriverReferral
      "PurpleRideCompleted" -> pure PurpleRideCompleted
      "GoldTierRideCompleted" -> pure GoldTierRideCompleted
      "LeaderBoardTopFiveHundred" -> pure LeaderBoardTopFiveHundred
      "TrainingCompleted" -> pure TrainingCompleted
      "BulkUploadFunction" -> pure BulkUploadFunction
      "QuizQuestionCompleted" -> pure QuizQuestionCompleted
      "BonusQuizCoins" -> pure BonusQuizCoins
      "RidesCompleted" -> RidesCompleted <$> obj .: "contents"
      "BulkUploadFunctionV2" -> BulkUploadFunctionV2 <$> obj .: "contents"
      "MetroRideCompleted" -> do
        contents <- obj .: "contents"
        case contents of
          -- Newer format: ["FromMetro", 5] or ["ToMetro"]
          Array arr' -> case V.toList arr' of
            [String rideType, Number rides] -> pure $ MetroRideCompleted (parseRideType rideType) (Just (round rides))
            [String rideType] -> pure $ MetroRideCompleted (parseRideType rideType) Nothing
            [String rideType, Null] -> pure $ MetroRideCompleted (parseRideType rideType) Nothing
            _ -> fail $ "Expected array of length 1 or 2 for 'MetroRideCompleted', got: " <> show contents
          -- Older format: "FromMetro"
          String rideType -> pure $ MetroRideCompleted (parseRideType rideType) Nothing
          _ -> fail "Unsupported format for 'MetroRideCompleted' contents"
      _ -> fail $ "Unknown DriverCoinsFunctionType tag encountered from DB : " ++ tag

parseRideType :: Text -> MetroRideType
parseRideType "ToMetro" = ToMetro
parseRideType "FromMetro" = FromMetro
parseRideType "FromOrToMetro" = FromOrToMetro
parseRideType _ = None

isMetroRideType :: MetroRideType -> Bool
isMetroRideType ToMetro = True
isMetroRideType FromMetro = True
isMetroRideType FromOrToMetro = True
isMetroRideType _ = False

$(mkBeamInstancesForEnumAndList ''DriverCoinsFunctionType)
