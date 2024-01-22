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
import qualified Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.Station as DStation
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildSearchReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DSearch.FRFSSearch ->
  Text ->
  DStation.Station ->
  DStation.Station ->
  m (Spec.SearchReq)
buildSearchReq search bapId fromStation toStation = do
  let transactionId = search.id.getId
      messageId = transactionId

  merchantId <- search.merchantId <&> (.getId) & fromMaybeM (InternalError "MerchantId not found")
  context <- Utils.buildContext Spec.SEARCH merchantId bapId transactionId messageId Nothing

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
        intentPayment = Just $ Utils.mkPayment Spec.NOT_PAID Nothing Nothing
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
          stopType = Just $ encodeToText Spec.START,
          stopParentStopId = Nothing
        },
      Spec.Stop
        { stopAuthorization = Nothing,
          stopId = Nothing,
          stopInstructions = Nothing,
          stopLocation = tfLocation $ toStation,
          stopType = Just $ encodeToText Spec.END,
          stopParentStopId = Nothing
        }
    ]

tfLocation :: DStation.Station -> Maybe Spec.Location
tfLocation station =
  Just $
    Spec.Location
      { locationDescriptor = Utils.tfDescriptor (Just $ station.code) (Just $ station.name),
        locationGps = Nothing
      }

tfVehicle :: DSearch.FRFSSearch -> Maybe Spec.Vehicle
tfVehicle search =
  Just $
    Spec.Vehicle
      { vehicleCategory = Just $ encodeToText search.vehicleType
      }
