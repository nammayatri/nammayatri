{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.LiveMap (getLiveMapDrivers) where

import qualified API.Types.UI.LiveMap as Common
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error (PersonError (PersonNotFound))
import qualified Kernel.Types.Id
import Servant
import qualified Storage.Queries.Person as QP
import Tools.Auth

getLiveMapDrivers ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.Flow [API.Types.UI.LiveMap.MapDriverInfoRes]
getLiveMapDrivers (mbPersonId, _merchantId, _merchantOpCityId) = do
  driver <- traverse QP.findById mbPersonId >>= fromMaybeM (PersonNotFound $ show mbPersonId) . join

  error "Logic yet to be decided"

-- pure $ Common.MapDriverInfoRes
--   { driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
--     driverStatus :: DriverStatus,
--     rcNo :: Kernel.Prelude.Text,
--     vehicleStatus :: VehicleStatus,
--     position :: Kernel.External.Maps.Types.LatLong,
--     source :: Kernel.Prelude.Text,
--     destination :: Kernel.Prelude.Text
--   }
