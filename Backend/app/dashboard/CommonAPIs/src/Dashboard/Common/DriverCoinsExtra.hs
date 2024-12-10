{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Dashboard.Common.DriverCoinsExtra
  ( module Dashboard.Common.DriverCoinsExtra,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import qualified Data.List as List
import Kernel.Prelude

data CoinMessage
  = CoinAdded
  | CoinSubtracted
  | FareRecomputation
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MetroRideType
  = ToMetro
  | FromMetro
  | None
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverCoinsFunctionType
  = OneOrTwoStarRating
  | FiveStarRating
  | BookingCancellation
  | CustomerReferral
  | DriverReferral
  | PurpleRideCompleted
  | LeaderBoardTopFiveHundred
  | TrainingCompleted
  | BulkUploadFunction
  | BulkUploadFunctionV2 CoinMessage
  | MetroRideCompleted MetroRideType
  | RidesCompleted Kernel.Prelude.Int
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (ToSchema, ToJSON)

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
            ++ [ (MetroRideCompleted v1, r2)
                 | r1 <- stripPrefix "MetroRideCompleted" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RidesCompleted v1, r2)
                 | r1 <- stripPrefix "RidesCompleted " r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
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
      "CustomerReferral" -> pure CustomerReferral
      "DriverReferral" -> pure DriverReferral
      "PurpleRideCompleted" -> pure PurpleRideCompleted
      "LeaderBoardTopFiveHundred" -> pure LeaderBoardTopFiveHundred
      "TrainingCompleted" -> pure TrainingCompleted
      "BulkUploadFunction" -> pure BulkUploadFunction
      "MetroRideCompleted" -> MetroRideCompleted <$> obj .: "contents"
      "RidesCompleted" -> RidesCompleted <$> obj .: "contents"
      "BulkUploadFunctionV2" -> BulkUploadFunctionV2 <$> obj .: "contents"
      _ -> fail $ "Unknown DriverCoinsFunctionType tag encountered from DB : " ++ tag
