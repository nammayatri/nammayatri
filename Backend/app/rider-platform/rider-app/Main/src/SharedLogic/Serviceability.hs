module SharedLogic.Serviceability where

import qualified Domain.Action.UI.Serviceability as Serviceability
import qualified Domain.Types.Booking as DB
import Domain.Types.Person
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBEnv)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantState as QMMS
import Tools.Error

validateServiceability :: (MonadFlow m, EncFlow m r, EsqDBFlow m r, HasField "esqDBReplicaEnv" r EsqDBEnv, CacheFlow m r) => LatLong -> [LatLong] -> Person -> m Context.City
validateServiceability origin stops person' = do
  Serviceability.NearestOperatingAndCurrentCity {nearestOperatingCity, currentCity} <- Serviceability.getNearestOperatingAndCurrentCity (.origin) (person'.id, person'.merchantId) False origin
  stopCitiesAndStates <- traverse (Serviceability.getNearestOperatingAndCurrentCity (.destination) (person'.id, person'.merchantId) False) stops
  mbMerchantState <- QMMS.findByMerchantIdAndState person'.merchantId currentCity.state
  let allowedStates = maybe [currentCity.state] (.allowedDestinationStates) mbMerchantState
  if all (\d -> d.currentCity.state `elem` allowedStates) stopCitiesAndStates
    then return nearestOperatingCity.city
    else throwError RideNotServiceable

validateServiceabilityForEditDestination :: (MonadFlow m, EncFlow m r, EsqDBFlow m r, HasField "esqDBReplicaEnv" r EsqDBEnv, CacheFlow m r) => DB.BookingDetails -> LatLong -> LatLong -> Person -> m Context.City
validateServiceabilityForEditDestination bookingDetails origin dest person' = do
  Serviceability.NearestOperatingAndCurrentCity {nearestOperatingCity, currentCity} <- Serviceability.getNearestOperatingAndCurrentCity (.origin) (person'.id, person'.merchantId) False origin
  destCityAndState <- Serviceability.getNearestOperatingAndCurrentCity (.destination) (person'.id, person'.merchantId) False dest
  -- InterCity/Rental: the new drop may be any serviceable area. The destination lookup above already
  -- throws for a non-serviceable point, and the driver still confirms acceptance, so we skip the
  -- same-city restriction that OneWay (and other categories) require.
  if isRentalOrInterCity bookingDetails
    then return nearestOperatingCity.city
    else do
      mbMerchantState <- QMMS.findByMerchantIdAndState person'.merchantId currentCity.state
      let allowedStates = maybe [currentCity.state] (.allowedDestinationStates) mbMerchantState
      if destCityAndState.currentCity.state `elem` allowedStates && destCityAndState.currentCity == currentCity
        then return nearestOperatingCity.city
        else throwError RideNotServiceable
  where
    isRentalOrInterCity bd = case bd of
      DB.RentalDetails _ -> True
      DB.InterCityDetails _ -> True
      _ -> False
