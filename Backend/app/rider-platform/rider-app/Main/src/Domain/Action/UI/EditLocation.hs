module Domain.Action.UI.EditLocation where

import API.Types.UI.EditLocation
import qualified Beckn.ACL.Update as ACL
import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import qualified Domain.Types.BookingUpdateRequest
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.Location as QL
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Servant.Client
import SharedLogic.BPPFlowRunner (withDirectBPP)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.DirectBPPCall as DirectBPPCall
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Ride as QR

getEditResult ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest ->
    Environment.Flow API.Types.UI.EditLocation.EditLocationResultAPIResp
  )
getEditResult (mbPersonId, merchantId) bookingUpdateReqId = do
  void $ fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  void $ CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bookingUpdateReq <- B.runInReplica $ QBUR.findById bookingUpdateReqId >>= fromMaybeM (InvalidRequest "Invalid booking update request id")
  return $ EditLocationResultAPIResp bookingUpdateReq

postEditResultConfirm ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postEditResultConfirm (mbPersonId, merchantId) bookingUpdateReqId = do
  void $ fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bookingUpdateReq <- B.runInReplica $ QBUR.findById bookingUpdateReqId >>= fromMaybeM (InvalidRequest "Invalid booking update request id")
  dropLocMapping <- B.runInReplica $ QLM.getLatestEndByEntityId bookingUpdateReqId.getId >>= fromMaybeM (InternalError $ "Latest drop location mapping not found for bookingUpdateRequestId: " <> bookingUpdateReqId.getId)
  destination' <- B.runInReplica $ QL.findById dropLocMapping.locationId >>= fromMaybeM (InternalError $ "Location not found for locationId: " <> dropLocMapping.locationId.getId)
  booking <- B.runInReplica $ QB.findById bookingUpdateReq.bookingId >>= fromMaybeM (InternalError $ "Invalid booking id" <> bookingUpdateReq.bookingId.getId)
  ride <- B.runInReplica $ QR.findByRBId booking.id >>= fromMaybeM (InvalidRequest $ "No Ride present for booking" <> booking.id.getId)
  let attemptsLeft = fromMaybe merchant.numOfAllowedEditLocationAttemptsThreshold ride.allowedEditLocationAttempts
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  let dUpdateReq =
        ACL.UpdateBuildReq
          { bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId,
            messageId = bookingUpdateReq.id.getId,
            city = merchant.defaultCity, -- TODO: Correct during interoperability
            details =
              ACL.UEditLocationBuildReqDetails $
                ACL.EditLocationBuildReqDetails
                  { bppRideId = ride.bppRideId,
                    origin = Nothing,
                    status = ACL.CONFIRM_UPDATE,
                    destination = Just destination',
                    stops = Nothing
                  },
            ..
          }
  becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
  QR.updateEditLocationAttempts ride.id (Just (attemptsLeft -1))
  QBUR.updateStatusById DBUR.CONFIRM bookingUpdateReqId
  withDirectBPP
    (\rt -> DirectBPPCall.directUpdate rt becknUpdateReq)
    (void . withShortRetry $ CallBPP.updateV2 booking.providerUrl becknUpdateReq)
  return Success

mkLocation :: QL.Location -> Common.Location
mkLocation QL.Location {..} =
  Common.Location
    { gps =
        Common.Gps
          { lat,
            lon
          },
      address =
        Common.Address
          { locality = address.area,
            area_code = address.areaCode,
            state = address.state,
            country = address.country,
            building = address.building,
            street = address.street,
            city = address.city,
            ward = address.ward,
            door = address.door
          }
    }
