{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Beckn.ACL.FRFS.Search (buildSearchReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.Station as DStation
import Kernel.Prelude
import Kernel.Utils.Common

buildSearchReq ::
  (MonadFlow m) =>
  DSearch.FRFSSearch ->
  BecknConfig ->
  DStation.Station ->
  DStation.Station ->
  m (Spec.SearchReq)
buildSearchReq search bapConfig fromStation toStation = do
  now <- getCurrentTime
  let transactionId = search.id.getId
      messageId = transactionId
      validTill = addUTCTime (intToNominalDiffTime 30) now
      ttl = diffUTCTime validTill now

  context <- Utils.buildContext Spec.SEARCH bapConfig transactionId messageId (Just $ Utils.durationToText ttl) Nothing

  pure $
    Spec.SearchReq
      { searchReqContext = context,
        searchReqMessage = tfSearchMessage search fromStation toStation
      }

tfSearchMessage :: DSearch.FRFSSearch -> DStation.Station -> DStation.Station -> Spec.SearchReqMessage
tfSearchMessage search fromStation toStation =
  Spec.SearchReqMessage
    { searchReqMessageIntent = tfIntent search fromStation toStation
    }

tfIntent :: DSearch.FRFSSearch -> DStation.Station -> DStation.Station -> Maybe Spec.Intent
tfIntent search fromStation toStation =
  Just $
    Spec.Intent
      { intentFulfillment = tfIntentFulfillment search fromStation toStation,
        intentPayment = Just $ Utils.mkPayment Spec.NOT_PAID Nothing Nothing Nothing Nothing
      }

tfIntentFulfillment :: DSearch.FRFSSearch -> DStation.Station -> DStation.Station -> Maybe Spec.Fulfillment
tfIntentFulfillment search fromStation toStation =
  Just $
    Spec.Fulfillment
      { fulfillmentId = Nothing,
        fulfillmentStops = tfStops fromStation toStation,
        fulfillmentTags = Nothing,
        fulfillmentType = Nothing,
        fulfillmentVehicle = tfVehicle search
      }

tfStops :: DStation.Station -> DStation.Station -> Maybe [Spec.Stop]
tfStops fromStation toStation =
  Just $
    [ Spec.Stop
        { stopAuthorization = Nothing,
          stopId = Nothing,
          stopInstructions = Nothing,
          stopLocation = tfLocation $ fromStation,
          stopType = Utils.encodeToText' Spec.START,
          stopParentStopId = Nothing
        },
      Spec.Stop
        { stopAuthorization = Nothing,
          stopId = Nothing,
          stopInstructions = Nothing,
          stopLocation = tfLocation $ toStation,
          stopType = Utils.encodeToText' Spec.END,
          stopParentStopId = Nothing
        }
    ]

tfLocation :: DStation.Station -> Maybe Spec.Location
tfLocation station =
  Just $
    Spec.Location
      { locationDescriptor = Utils.tfDescriptor (Just $ station.code) (Just $ station.name),
        locationGps = Nothing,
        locationCity = Nothing,
        locationCountry = Nothing
      }

tfVehicle :: DSearch.FRFSSearch -> Maybe Spec.Vehicle
tfVehicle search =
  Just $
    Spec.Vehicle
      { vehicleCategory = Utils.encodeToText' search.vehicleType
      }
