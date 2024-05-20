{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SpecialLocation where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Lib.Queries.SpecialLocation
import Servant
import Tools.Auth

getSpecialLocationList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Environment.Flow [SpecialLocationFull]
getSpecialLocationList (_, _, merchantOperatingCityId) = notNullGateGeoJson <$> findFullSpecialLocationsByMerchantOperatingCityId merchantOperatingCityId.getId
  where
    notNullGateGeoJson = filter (any (isJust . (.geoJson)) . (.gatesInfo))
