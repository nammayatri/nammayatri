module Domain.Action.Dashboard.RideBooking.DriverRegistration
  ( postDriverRegistrationAuth,
    postDriverRegistrationVerify,
    auth,
    verify,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified Domain.Action.UI.FleetDriverAssociation as FDV
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding as DomainRC
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import Tools.Error

postDriverRegistrationAuth, auth :: ShortId DM.Merchant -> Context.City -> Common.AuthReq -> Flow Common.AuthRes
postDriverRegistrationAuth = auth
auth merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  res <-
    DReg.auth
      True
      DReg.AuthReq
        { mobileNumber = Just req.mobileNumber,
          mobileCountryCode = Just req.mobileCountryCode,
          merchantId = merchant.id.getId,
          merchantOperatingCity = Just opCity,
          registrationLat = Nothing,
          registrationLon = Nothing,
          name = Nothing,
          email = Nothing,
          identifierType = Just SP.MOBILENUMBER
        }
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
  pure $ Common.AuthRes {authId = res.authId.getId, attempts = res.attempts}

postDriverRegistrationVerify :: ShortId DM.Merchant -> Context.City -> Text -> Bool -> Text -> Common.AuthVerifyReq -> Flow APISuccess
postDriverRegistrationVerify _merchantShortId _opCity = verify

verify :: Text -> Bool -> Text -> Common.AuthVerifyReq -> Flow APISuccess
verify authId mbFleet fleetOwnerId req = do
  let regId = Id authId :: Id SR.RegistrationToken
  res <-
    DReg.verify
      regId
      DReg.AuthVerifyReq
        { otp = req.otp,
          deviceToken = req.deviceToken,
          whatsappNotificationEnroll = Nothing
        }
  when mbFleet $ do
    checkAssoc <- runInReplica $ QFDV.findByDriverIdAndFleetOwnerId res.person.id fleetOwnerId True
    when (isJust checkAssoc) $ throwError (InvalidRequest "Driver already associated with fleet")
    assoc <- FDV.makeFleetDriverAssociation res.person.id fleetOwnerId (DomainRC.convertTextToUTC (Just "2099-12-12"))
    QFDV.create assoc
  pure Success
