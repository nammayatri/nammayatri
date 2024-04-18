{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Cancel (buildCancelReqV2, buildCancelSearchReqV2) where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as DRB
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import Tools.Error

buildCancelReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DCancel.CancelRes ->
  m Spec.CancelReq
buildCancelReqV2 res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle res.merchant.id "MOBILITY" (Utils.mapVariantToVehicle res.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
  ttl <- bapConfig.cancelTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.CANCEL Context.MOBILITY messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.city res.merchant.country (Just ttl)
  bppBookingId <- res.bppBookingId & fromMaybeM (InternalError "Unable to cancel booking because bppBookingId is missing.") -- Should never happen as at top level we have check to not trigger cancel without bppBookingId.
  pure
    Spec.CancelReq
      { cancelReqContext = context,
        cancelReqMessage = mkCancelMessageV2 res bppBookingId -- soft cancel and confirm cancel
      }

mkCancelMessageV2 :: DCancel.CancelRes -> Id DRB.BPPBooking -> Spec.CancelReqMessage
mkCancelMessageV2 res bppBookingId =
  Spec.CancelReqMessage
    { cancelReqMessageCancellationReasonId = Just (show Enums.CANCELLED_BY_CUSTOMER),
      cancelReqMessageOrderId = bppBookingId.getId,
      cancelReqMessageDescriptor =
        Just $
          Spec.Descriptor
            { descriptorName = Just "Cancel Ride",
              descriptorCode = Just res.cancelStatus, -- TODO::Beckn, confirm mapping according to spec.
              descriptorShortDesc = Nothing
            }
    }

buildCancelSearchReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DCancel.CancelSearch ->
  m Spec.CancelReq
buildCancelSearchReqV2 res = do
  let messageId = res.estimateId.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle res.merchant.id "MOBILITY" (Utils.mapVariantToVehicle res.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
  ttl <- bapConfig.cancelTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.CANCEL Context.MOBILITY messageId (Just res.searchReqId.getId) res.merchant.bapId bapUrl (Just res.providerId) (Just res.providerUrl) res.city res.merchant.country (Just ttl)
  pure
    Spec.CancelReq
      { cancelReqContext = context,
        cancelReqMessage = mkCancelSearchMessageV2 res
      }

mkCancelSearchMessageV2 :: DCancel.CancelSearch -> Spec.CancelReqMessage
mkCancelSearchMessageV2 res =
  Spec.CancelReqMessage
    { cancelReqMessageCancellationReasonId = Just (show Enums.CANCELLED_BY_CUSTOMER),
      cancelReqMessageOrderId = res.searchReqId.getId,
      cancelReqMessageDescriptor = Nothing
    }
