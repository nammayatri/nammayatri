module Domain.Action.Common.VehicleDetails
  ( getFilteredVehicleDetails,
    toMakeModelTuples,
  )
where

import Data.List (nub)
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleDetails as DVehicleDetails
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.Queries.VehicleDetails as QVehicleDetails

getFilteredVehicleDetails ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Kernel.Types.Beckn.Context.Country ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m [DVehicleDetails.VehicleDetails]
getFilteredVehicleDetails country merchantOperatingCityId =
  QVehicleDetails.findAllByCountryAndMerchantOperatingCityIdWithFallback country merchantOperatingCityId

toMakeModelTuples :: [DVehicleDetails.VehicleDetails] -> [(Text, [Text])]
toMakeModelTuples vehicleDetails =
  let makes = nub $ map DVehicleDetails.make vehicleDetails
   in map
        ( \make ->
            ( make,
              nub [DVehicleDetails.model vehicleDetail | vehicleDetail <- vehicleDetails, DVehicleDetails.make vehicleDetail == make]
            )
        )
        makes
