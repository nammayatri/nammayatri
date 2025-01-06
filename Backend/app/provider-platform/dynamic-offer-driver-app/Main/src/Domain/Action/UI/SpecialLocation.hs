module Domain.Action.UI.SpecialLocation where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Lib.Queries.SpecialLocation

getSpecialLocationList ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Bool) ->
    Environment.Flow [Lib.Queries.SpecialLocation.SpecialLocationFull]
  )
getSpecialLocationList (_, _, merchantOperatingCityId) isOrigin = do
  specialLocationsWithAllGates <- notNullGateGeoJson <$> findFullSpecialLocationsByMerchantOperatingCityId merchantOperatingCityId.getId
  let isOrigin' = fromMaybe True isOrigin
  let filteredSpecialLocationsWithGates = mapMaybe (\loc -> filterGates (Just loc) isOrigin') specialLocationsWithAllGates
  pure filteredSpecialLocationsWithGates
  where
    notNullGateGeoJson = filter (any (isJust . (.geoJson)) . (.gatesInfo))
