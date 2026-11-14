module Domain.Action.UI.BlackListDriver where

import qualified API.Types.UI.BlackListDriver
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error

postDriverBlackList ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    API.Types.UI.BlackListDriver.BlackListDriverReq ->
    Environment.Flow APISuccess
  )
postDriverBlackList (mbPersonId, merchantId) rideId API.Types.UI.BlackListDriver.BlackListDriverReq {..} = do
  personId <- mbPersonId & fromMaybeM (InternalError "No person found")
  person <- QP.findById personId >>= fromMaybeM (InternalError "No person found") >>= decrypt
  merchant <- CQM.findById merchantId >>= fromMaybeM (InternalError "Merchant not found")
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  case (person.mobileNumber, person.mobileCountryCode) of
    (Just mobileNumber, Just countryCode) ->
      CallBPPInternal.blackListDriver
        merchant.driverOfferApiKey
        merchant.driverOfferBaseUrl
        merchant.driverOfferMerchantId
        mobileNumber
        countryCode
        ride.bppRideId.getId
        blackListed
        >> pure APISuccess.Success
    _ -> pure APISuccess.Success

getDriverBlacklist ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow [API.Types.UI.BlackListDriver.BlackListedDriverResp]
  )
getDriverBlacklist (mbPersonId, merchantId) = do
  personId <- mbPersonId & fromMaybeM (InternalError "No person found")
  person <- QP.findById personId >>= fromMaybeM (InternalError "No person found") >>= decrypt
  merchant <- CQM.findById merchantId >>= fromMaybeM (InternalError "Merchant not found")
  case (person.mobileNumber, person.mobileCountryCode) of
    (Just mobileNumber, Just countryCode) ->
      CallBPPInternal.getBlackListedDriverList
        merchant.driverOfferApiKey
        merchant.driverOfferBaseUrl
        merchant.driverOfferMerchantId
        mobileNumber
        countryCode
        <&> map
          ( \r ->
              API.Types.UI.BlackListDriver.BlackListedDriverResp
                { id = r.id,
                  driverName = r.driverName,
                  driverPhone = r.driverPhone,
                  driverRating = r.driverRating
                }
          )
    _ -> pure []

postDriverBlacklistUnblock ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow APISuccess
  )
postDriverBlacklistUnblock (mbPersonId, merchantId) driverId = do
  personId <- mbPersonId & fromMaybeM (InternalError "No person found")
  person <- QP.findById personId >>= fromMaybeM (InternalError "No person found") >>= decrypt
  merchant <- CQM.findById merchantId >>= fromMaybeM (InternalError "Merchant not found")
  case (person.mobileNumber, person.mobileCountryCode) of
    (Just mobileNumber, Just countryCode) ->
      CallBPPInternal.unblockDriver
        merchant.driverOfferApiKey
        merchant.driverOfferBaseUrl
        merchant.driverOfferMerchantId
        mobileNumber
        countryCode
        driverId
        >> pure APISuccess.Success
    _ -> pure APISuccess.Success
