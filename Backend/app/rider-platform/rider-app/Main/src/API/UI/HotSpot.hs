module API.UI.HotSpot where

import Data.List (sortOn)
import Data.Ord
import Domain.Types.HotSpot as HotSpot
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.CachedQueries.CacheConfig
import Storage.CachedQueries.Maps.LocationMapCache
import Tools.Auth
import qualified Tools.Maps as Maps
import Tools.Metrics

type API = "getHotSpot" :> TokenAuth :> ReqBody '[JSON] Maps.LatLong :> Get '[JSON] HotSpotResponse

handler :: FlowServer API
handler = getHotSpot

getHotSpot :: (Id Person.Person, Id Merchant.Merchant) -> Maps.LatLong -> FlowHandler HotSpotResponse
getHotSpot _ = withFlowHandlerAPI . getHotspot

-- hotspot

getHotspot ::
  ( HasCacheConfig r,
    HedisFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["maxRadiusToShowHotSpots" ::: HighPrecMeters],
    HasFlowEnv m r '["blockRadius" ::: HighPrecMeters],
    HasFlowEnv m r '["minFrequencyOfHotSpot" ::: Int]
  ) =>
  Maps.LatLong ->
  m HotSpotResponse
getHotspot latLong = do
  hotSpotsUnfiltered <- getLocationsInCache
  maxRadiusToTake <- asks (.maxRadiusToShowHotSpots)
  minFrequencyOfHotSpot <- asks (.minFrequencyOfHotSpot)
  blockRadius <- asks (.blockRadius)
  let filteredAccordingToMaxRadiusAndMinFrequency =
        filter
          ( \hotspot -> case hotspot.centroidLatLong of
              Just ll -> distanceBetweenInMeters ll latLong <= maxRadiusToTake && hotspot.frequency >= minFrequencyOfHotSpot
              Nothing -> False
          )
          hotSpotsUnfiltered
  let hotSpots = take 4 $ sortOn (Down . frequency) filteredAccordingToMaxRadiusAndMinFrequency
  return HotSpotResponse {..}
