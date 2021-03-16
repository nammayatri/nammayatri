{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Allocation where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.ProductInstance as SPI
import qualified Beckn.Types.Storage.Vehicle as SV
import Beckn.Utils.Common (encodeToText)
import EulerHS.Prelude
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.ProductInstance as QPI
import Types.API.ProductInstance (DriverVehicleInfo (..))

assignDriver ::
  Id SPI.ProductInstance ->
  [Id SPI.ProductInstance] ->
  SV.Vehicle ->
  SP.Person ->
  DB.SqlDB ()
assignDriver productInstanceId piIdList vehicle driver = do
  QPI.updateVehicle piIdList (Just $ vehicle ^. #_id)
  QPI.updateDriver piIdList (Just personId)
  QDI.updateOnRide driverId True
  QPI.updateStatusByIds piIdList SPI.TRIP_ASSIGNED
  updateInfo productInstanceId driver vehicle
  where
    personId = driver ^. #_id
    driverId = cast personId

updateInfo ::
  Id SPI.ProductInstance ->
  SP.Person ->
  SV.Vehicle ->
  DB.SqlDB ()
updateInfo piId driver vehicle =
  QPI.updateInfo piId info
  where
    info = encodeToText (mkInfoObj driver vehicle)
    mkInfoObj drivInfo vehiInfo =
      DriverVehicleInfo
        { driverInfo = encodeToText drivInfo,
          vehicleInfo = encodeToText vehiInfo
        }
