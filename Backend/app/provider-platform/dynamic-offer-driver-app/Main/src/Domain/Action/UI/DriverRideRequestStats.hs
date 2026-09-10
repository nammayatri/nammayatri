module Domain.Action.UI.DriverRideRequestStats where

import qualified API.Types.UI.DriverRideRequestStats
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.SearchRequestForDriverExtra as QSRFD

getRideRequestStats ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Maybe Int ->
    Flow API.Types.UI.DriverRideRequestStats.DriverRideRequestStatsRes
  )
getRideRequestStats (mbPersonId, _merchantId, _merchantOpCityId) mbDurationInMinutes = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  let durationInMinutes = fromMaybe (24 * 60) mbDurationInMinutes
  now <- getCurrentTime
  let since = addUTCTime (fromIntegral (- (durationInMinutes * 60))) now
  (totalRequests, acceptedRequests, rejectedRequests, pulledRequests, lastRequestAt, lastAcceptedRequestAt) <- QSRFD.getRideRequestStatsSince driverId since
  pure
    API.Types.UI.DriverRideRequestStats.DriverRideRequestStatsRes
      { totalRequests = totalRequests,
        acceptedRequests = acceptedRequests,
        rejectedRequests = rejectedRequests,
        pulledRequests = pulledRequests,
        lastRequestAt = lastRequestAt,
        lastAcceptedRequestAt = lastAcceptedRequestAt
      }
