module Lib.LocationUpdates (module Reexport, module Lib.LocationUpdates) where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant.MerchantServiceConfig as DOSC
import qualified Domain.Types.Person as Person
import Environment
import "location-updates" Lib.LocationUpdates as Reexport
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QOMC
import qualified Storage.Queries.Ride as QRide
import Tools.Error

buildRideInterpolationHandler :: Id Merchant -> Flow (RideInterpolationHandler Person.Person Flow)
buildRideInterpolationHandler orgId = do
  orgMapsConfig <- QOMC.findByMerchantId orgId >>= fromMaybeM (MerchantServiceUsageConfigNotFound orgId.getId)
  orgMapsServiceConfig <-
    QOMSC.findByMerchantIdAndService orgId (DOSC.MapsService orgMapsConfig.snapToRoad)
      >>= fromMaybeM (MerchantServiceConfigNotFound orgId.getId orgMapsConfig.snapToRoad)
  case orgMapsServiceConfig.serviceConfig of
    DOSC.MapsServiceConfig cfg ->
      return $
        mkRideInterpolationHandler cfg $
          \driverId dist -> Esq.runTransaction $ QRide.updateDistance driverId dist

-- _ -> throwError $ InternalError "Impossible happened"

makeLockKey :: Id Person.Person -> Text
makeLockKey (Id driverId) = "ARDU:driverLocationUpdate:" <> driverId
