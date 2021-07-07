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
import qualified Types.Storage.RegistrationToken as SR
import Types.Storage.RideRequest
import Utils.Common

responseToEventType :: NotificationStatus -> AllocationEventType
responseToEventType ACCEPT = AllocationEvent.AcceptedByDriver
responseToEventType REJECT = AllocationEvent.RejectedByDriver

setDriverAcceptance :: SR.RegistrationToken -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance token req = withFlowHandlerAPI $ do
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
        DriverResponse {driverId = Id driverId, status = req.response, respondedAt = currentTime}
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
    (Just $ Id driverId)
  pure Success
  where
    response = req.response
    productInstanceId = req.productInstanceId
    driverId = token.entityId
    logMessage =
      "beckn:" <> productInstanceId.getId <> ":"
        <> driverId
        <> ":response"
        <> " "
        <> show response
