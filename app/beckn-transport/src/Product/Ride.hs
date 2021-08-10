module Product.Ride
  ( setDriverAcceptance,
  )
where

import App.Types
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import Beckn.Types.Id (Id (..), cast)
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.AllocationEvent as AllocationEvent
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.RideRequest as RideRequest
import Types.API.Ride
import Types.Storage.AllocationEvent
import qualified Types.Storage.AllocationEvent as AllocationEvent
import qualified Types.Storage.Person as SP
import Types.Storage.RideRequest
import Utils.Common

responseToEventType :: NotificationStatus -> AllocationEventType
responseToEventType ACCEPT = AllocationEvent.AcceptedByDriver
responseToEventType REJECT = AllocationEvent.RejectedByDriver

setDriverAcceptance :: Id SP.Person -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance personId req = withFlowHandlerAPI $ do
  currentTime <- getCurrentTime
  logTagInfo "setDriverAcceptance" logMessage
  productInstance <-
    ProductInstance.findById productInstanceId
      >>= fromMaybeM PIDoesNotExist
  transporterOrg <-
    Organization.findOrganizationById productInstance.organizationId
      >>= fromMaybeM OrgDoesNotExist
  guid <- generateGUID
  let driverResponse =
        DriverResponse {driverId = driverId, status = req.response}
  let rideRequest =
        RideRequest
          { id = Id guid,
            rideId = cast productInstanceId,
            shortOrgId = transporterOrg.shortId,
            createdAt = currentTime,
            _type = DRIVER_RESPONSE,
            info = Just $ encodeToText driverResponse
          }
  RideRequest.createFlow rideRequest
  AllocationEvent.logAllocationEvent
    (responseToEventType response)
    (cast productInstanceId)
    (Just driverId)
  pure Success
  where
    response = req.response
    productInstanceId = req.productInstanceId
    driverId = cast personId
    logMessage =
      "beckn:" <> productInstanceId.getId <> ":"
        <> getId driverId
        <> ":response"
        <> " "
        <> show response
