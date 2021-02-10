{-# LANGUAGE OverloadedLabels #-}

module Services.Allocation.Internals where

import App.Types
import Beckn.Types.App as BC
import qualified Beckn.Types.Storage.ProductInstance as PI
import EulerHS.Prelude
import qualified Product.ProductInstance as PI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as PIQ
import Types.API.ProductInstance
import Types.App

assignDriver :: RideId -> DriverId -> Flow ()
assignDriver rideId driverId = do
  ordPi <- PIQ.findById productInstanceId
  piList <- PIQ.findAllByParentId (ordPi ^. #_parentId)
  person <- QPerson.findPersonById personId
  let vehicleId = person ^. #_udf1
      req = buildProdInstUpdateReq vehicleId

  PI.updateVehicleDetails piList req
  PI.assignDriver piList req
  PI.updateStatus productInstanceId req
  PI.updateInfo productInstanceId
  where
    personId = PersonId $ driverId ^. #_getDriverId
    productInstanceId = ProductInstanceId $ rideId ^. #_getRideId
    buildProdInstUpdateReq vehicleId =
      ProdInstUpdateReq
        { _status = Just PI.TRIP_ASSIGNED,
          _personId = Just $ driverId ^. #_getDriverId,
          _vehicleId = vehicleId,
          _otpCode = Nothing
        }
