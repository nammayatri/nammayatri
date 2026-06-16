{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.GoogleMobilityBilling
  ( reportNavBillableEvent,
  )
where

import qualified Domain.Types.Booking as DB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DOSC
import qualified Domain.Types.Ride as DRide
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps.Google.Config (GoogleCfg)
import qualified Kernel.External.Maps.Google.MobilityBilling as GoogleBilling
import qualified Kernel.External.Maps.Interface.Types as MapsIface
import qualified Kernel.External.Maps.Types as MapsTypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC

reportNavBillableEvent ::
  ( MonadFlow m,
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasRequestId r
  ) =>
  DB.Booking ->
  DRide.Ride ->
  m ()
reportNavBillableEvent booking ride = do
  mbGoogleCfg <- getMerchantGoogleMapsCfg booking.merchantOperatingCityId
  case mbGoogleCfg of
    Nothing ->
      logWarning $ "Google maps config not found for city " <> booking.merchantOperatingCityId.getId <> "; cannot report Google mobility billable event"
    Just googleCfg -> do
      apiKey <- decrypt googleCfg.googleKey
      merchantOpCity <-
        CQMOC.findById booking.merchantOperatingCityId
          >>= fromMaybeM (InternalError $ "MerchantOperatingCity not found: " <> booking.merchantOperatingCityId.getId)
      GoogleBilling.reportBillableEvent googleCfg.mobilityBillingUrl (toRegionCode merchantOpCity.country) apiKey ride.id.getId

getMerchantGoogleMapsCfg ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m (Maybe GoogleCfg)
getMerchantGoogleMapsCfg merchantOpCityId = do
  mbServiceConfig <- QOMSC.findByServiceAndCity (DOSC.MapsService MapsTypes.Google) merchantOpCityId
  pure $ case mbServiceConfig of
    Just sc -> case sc.serviceConfig of
      DOSC.MapsServiceConfig (MapsIface.GoogleConfig googleCfg) -> Just googleCfg
      _ -> Nothing
    Nothing -> Nothing

toRegionCode :: Context.Country -> Text
toRegionCode country = case country of
  Context.India -> "IN"
  Context.France -> "FR"
  Context.USA -> "US"
  Context.Netherlands -> "NL"
  Context.Finland -> "FI"
  _ -> "IN"
