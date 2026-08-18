{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSupport (buildOnSupportMessageV2) where

import qualified Beckn.OnDemand.Utils.Common as BUtils
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601, mapServiceTierToCategory)
import qualified BecknV2.OnDemand.Utils.Context as CU
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC

buildOnSupportMessageV2 ::
  ( MonadFlow m,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DM.Merchant ->
  DRB.Booking ->
  Maybe Text ->
  Maybe Spec.Descriptor ->
  m Spec.OnSupportReq
buildOnSupportMessageV2 merchant booking mbMessageId mbDescriptor = do
  msgId <- generateGUID
  let bppId = getShortId merchant.subscriberId
      city = fromMaybe merchant.city booking.bapCity
      country = fromMaybe merchant.country booking.bapCountry
  bppUri <- BUtils.mkBppUri merchant.id.getId
  becknConfig <- QBC.findByMerchantIdDomainAndVehicle booking.providerId "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  ttl <- becknConfig.onSupportTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  bapUri <- parseBaseUrl booking.bapUri
  context <- CU.buildContextV2 Context.ON_SUPPORT Context.MOBILITY (fromMaybe msgId mbMessageId) (Just booking.transactionId) booking.bapId bapUri (Just bppId) (Just bppUri) city country (Just ttl)
  pure $
    Spec.OnSupportReq
      { onSupportReqContext = context,
        onSupportReqError = Nothing,
        onSupportReqMessage =
          Just
            Spec.OnSupportReqMessage
              { onSupportReqMessageSupport =
                  Spec.Support
                    { supportDescriptor = mbDescriptor,
                      supportRefType = Just "ORDER",
                      supportRefId = Just booking.id.getId,
                      supportCallbackPhone = Nothing,
                      supportPhone = Nothing, -- Need discuss what should we pass here
                      supportEmail = Nothing, -- same
                      supportUrl = Nothing
                    }
              }
      }
