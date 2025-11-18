module Domain.Action.Dashboard.RideBooking.Registration
  ( postRegistrationAuth,
    postRegistrationVerify,
    postRegistrationOtpResend,
    postRegistrationLogout,
  )
where

import qualified API.Types.Dashboard.RideBooking.Registration
import qualified "this" Domain.Action.UI.Registration
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.RegistrationToken
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (withPersonIdLogTag)

postRegistrationAuth ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.Dashboard.RideBooking.Registration.CustomerAuthReq ->
  Environment.Flow Domain.Action.UI.Registration.AuthRes
postRegistrationAuth merchantShortId _opCity req = do
  let authReq = buildAuthReq merchantShortId req
  Domain.Action.UI.Registration.auth authReq Nothing Nothing Nothing Nothing Nothing Nothing Nothing

postRegistrationVerify ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken ->
  Domain.Action.UI.Registration.AuthVerifyReq ->
  Environment.Flow Domain.Action.UI.Registration.AuthVerifyRes
postRegistrationVerify _merchantShortId _opCity tokenId req = Domain.Action.UI.Registration.verify tokenId req

postRegistrationOtpResend ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken ->
  Environment.Flow Domain.Action.UI.Registration.ResendAuthRes
postRegistrationOtpResend _merchantShortId _opCity tokenId = Domain.Action.UI.Registration.resend tokenId Nothing

postRegistrationLogout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postRegistrationLogout _merchantShortId _opCity personId = withPersonIdLogTag personId $ Domain.Action.UI.Registration.logout personId

buildAuthReq ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  API.Types.Dashboard.RideBooking.Registration.CustomerAuthReq ->
  Domain.Action.UI.Registration.AuthReq
buildAuthReq merchantShortId req =
  Domain.Action.UI.Registration.AuthReq
    { mobileNumber = Just req.mobileNumber,
      mobileCountryCode = Just req.mobileCountryCode,
      identifierType = Nothing,
      merchantId = merchantShortId,
      deviceToken = Nothing,
      notificationToken = Nothing,
      whatsappNotificationEnroll = Nothing,
      firstName = Nothing,
      middleName = Nothing,
      lastName = Nothing,
      email = Nothing,
      businessEmail = Nothing,
      language = Nothing,
      gender = Nothing,
      otpChannel = req.otpChannel,
      registrationLat = Nothing,
      registrationLon = Nothing,
      enableOtpLessRide = Nothing,
      allowBlockedUserLogin = Nothing
    }
