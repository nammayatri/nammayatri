module Domain.Action.Internal.MeterRideFare where

import qualified API.Types.UI.PriceBreakup as API.Types.UI.PriceBreakup
import qualified Domain.Action.UI.PriceBreakup as DPB
import Domain.Types.Merchant (Merchant)
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Ride as QRide

data MeterRideFareInfoReq = MeterRideFareInfoReq
  { bppRideId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

getMeterRidePrice ::
  Id Merchant ->
  Maybe Text ->
  MeterRideFareInfoReq ->
  Flow API.Types.UI.PriceBreakup.MeterRidePriceRes
getMeterRidePrice merchantId apiKey MeterRideFareInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  let rideId = Id bppRideId
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound bppRideId)
  let merchOpCityId = Id merchantOperatingCityId
  DPB.getMeterRidePrice (Just ride.driverId, merchantId, merchOpCityId) rideId
