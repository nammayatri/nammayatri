{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Search (buildSearchReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Domain.Types.BecknConfig
import qualified Domain.Types.Station as DStation
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common

buildSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  Text ->
  Spec.VehicleCategory ->
  BecknConfig ->
  Maybe DStation.Station ->
  Maybe DStation.Station ->
  Context.City ->
  m (Spec.SearchReq)
buildSearchReq transactionId vehicleType bapConfig mbFromStation mbToStation city = do
  now <- getCurrentTime
  messageId <- generateGUID
  let validTill = addUTCTime (intToNominalDiffTime (fromMaybe 30 bapConfig.searchTTLSec)) now
      ttl = diffUTCTime validTill now

  context <- Utils.buildContext Spec.SEARCH bapConfig transactionId messageId (Just $ Utils.durationToText ttl) Nothing city vehicleType

  pure $
    Spec.SearchReq
      { searchReqContext = context,
        searchReqMessage = tfSearchMessage vehicleType mbFromStation mbToStation
      }

tfSearchMessage :: Spec.VehicleCategory -> Maybe DStation.Station -> Maybe DStation.Station -> Spec.SearchReqMessage
tfSearchMessage vehicleType mbFromStation mbToStation =
  Spec.SearchReqMessage
    { searchReqMessageIntent = tfIntent vehicleType mbFromStation mbToStation
    }

tfIntent :: Spec.VehicleCategory -> Maybe DStation.Station -> Maybe DStation.Station -> Maybe Spec.Intent
tfIntent vehicleType mbFromStation mbToStation =
  Just $
    Spec.Intent
      { intentFulfillment = tfIntentFulfillment vehicleType mbFromStation mbToStation,
        intentPayment = Just $ Utils.mkPaymentForSearchReq Nothing Nothing Nothing Nothing Nothing Nothing (Just "0")
      }

tfIntentFulfillment :: Spec.VehicleCategory -> Maybe DStation.Station -> Maybe DStation.Station -> Maybe Spec.Fulfillment
tfIntentFulfillment vehicleType mbFromStation mbToStation =
  Just $
    Spec.Fulfillment
      { fulfillmentId = Nothing,
        fulfillmentStops = maybe Nothing (\(fromStation, toStation) -> tfStops (Just fromStation) (Just toStation)) ((,) <$> mbFromStation <*> mbToStation),
        fulfillmentTags = Nothing,
        fulfillmentType = Nothing,
        fulfillmentVehicle = tfVehicle vehicleType
      }

tfStops :: Maybe DStation.Station -> Maybe DStation.Station -> Maybe [Spec.Stop]
tfStops mbFromStation mbToStation =
  Just $
    [ Spec.Stop
        { stopAuthorization = Nothing,
          stopId = Nothing,
          stopInstructions = Nothing,
          stopLocation = tfLocation $ mbFromStation,
          stopType = Utils.encodeToText' Spec.START,
          stopParentStopId = Nothing
        },
      Spec.Stop
        { stopAuthorization = Nothing,
          stopId = Nothing,
          stopInstructions = Nothing,
          stopLocation = tfLocation $ mbToStation,
          stopType = Utils.encodeToText' Spec.END,
          stopParentStopId = Nothing
        }
    ]

tfLocation :: Maybe DStation.Station -> Maybe Spec.Location
tfLocation Nothing = Nothing
tfLocation (Just station) =
  Just $
    Spec.Location
      { locationDescriptor = Utils.tfDescriptor (Just $ station.code) (Just $ station.name),
        locationGps = Nothing,
        locationCity = Nothing,
        locationCountry = Nothing
      }

tfVehicle :: Spec.VehicleCategory -> Maybe Spec.Vehicle
tfVehicle vehicleType =
  Just $
    Spec.Vehicle
      { vehicleCategory = Utils.encodeToText' vehicleType,
        vehicleVariant = Nothing
      }
