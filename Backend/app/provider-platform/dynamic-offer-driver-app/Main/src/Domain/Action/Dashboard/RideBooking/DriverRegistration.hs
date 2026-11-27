module Domain.Action.Dashboard.RideBooking.DriverRegistration
  ( postDriverRegistrationAuth,
    postDriverRegistrationVerify,
    auth,
    verifyDriverOtp,
    linkDriverToFleet,
  )
where

import qualified "this" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.FleetDriverAssociation as FDV
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import Domain.Types.TransporterConfig
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverOnboarding as DomainRC
import qualified SharedLogic.DriverOnboarding.Common as SOnbCommon
import qualified SharedLogic.DriverOnboarding.OnboardingComms as SOnboardingComms
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RideExtra as QRideExtra
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
          name = req.name,
          email = Nothing,
          identifierType = Just SP.MOBILENUMBER,
          otpChannel = Nothing,
          password = Nothing,
          employeeId = Nothing
        }
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
  pure $ Common.AuthRes {authId = res.authId.getId, attempts = res.attempts}

postDriverRegistrationVerify :: ShortId DM.Merchant -> Context.City -> Text -> Bool -> Text -> Common.AuthVerifyReq -> Flow APISuccess
postDriverRegistrationVerify merchantShortId opCity authId mbFleet fleetOwnerId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  driverId <- verifyDriverOtp authId mbFleet transporterConfig req
  when mbFleet $ do
    driver <- linkDriverToFleet driverId fleetOwnerId Nothing transporterConfig
    SOnboardingComms.setOnboardingAs transporterConfig driver DDI.FLEET_DRIVER
  pure Success

-- | Checks the code a joining driver was sent and signs them in, and refuses a driver who cannot join a fleet
-- because of a live ride. A caller that ends the driver's current links has to call this first, so that a wrong
-- or used code, or such a driver, leaves those links as they are.
verifyDriverOtp :: Text -> Bool -> TransporterConfig -> Common.AuthVerifyReq -> Flow (Id SP.Person)
verifyDriverOtp authId mbFleet transporterConfig req = do
  let regId = Id authId :: Id SR.RegistrationToken
  res <-
    DReg.verify
      regId
      DReg.AuthVerifyReq
        { otp = req.otp,
          deviceToken = req.deviceToken,
          whatsappNotificationEnroll = Nothing
        }
      Nothing
  when (not mbFleet && req.isOnboardingFlow == Just True) $
    fork "Sending onboarding link SMS to Driver" $ do
      driver <- QP.findById res.person.id >>= fromMaybeM (PersonNotFound res.person.id.getId)
      merchantOpCity <- CQMOC.findById transporterConfig.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound transporterConfig.merchantOperatingCityId.getId)
      SOnbCommon.sendOnboardingLinkSms merchantOpCity transporterConfig driver Nothing
  when mbFleet $ do
    -- Check if driver has any active rides (not completed or cancelled)
    mbActiveRide <- B.runInReplica $ QRideExtra.getUpcomingOrActiveByDriverId res.person.id
    when (isJust mbActiveRide) $ throwError (InvalidRequest "Driver has active rides. Please complete or cancel all rides before adding to fleet")
  pure res.person.id

-- | Links a driver whose code 'verifyDriverOtp' has accepted to a fleet. It does not set the driver's onboarding
-- type: that takes the driver's onboarding lock, which the joining flow already holds here.
linkDriverToFleet :: Id SP.Person -> Text -> Maybe (Id SP.Person) -> TransporterConfig -> Flow SP.Person
linkDriverToFleet driverId fleetOwnerId mbOperatorId transporterConfig = do
  checkAssoc <- runInReplica $ QFDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId True
  when (isJust checkAssoc) $ throwError (InvalidRequest "Driver already associated with fleet")
  assoc <- FDV.makeFleetDriverAssociation driverId fleetOwnerId mbOperatorId DomainRC.defaultAssociationEnd (Just transporterConfig.merchantId) (Just transporterConfig.merchantOperatingCityId)
  QFDV.create assoc
  fleetLinkDriver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  when (transporterConfig.deleteDriverBankAccountWhenLinkToFleet == Just True) $ QDBA.deleteById driverId
  Analytics.handleDriverAnalyticsAndFlowStatus
    transporterConfig
    driverId
    Nothing
    ( \_ -> do
        Analytics.incrementFleetOwnerAnalyticsActiveDriverCount transporterConfig (Just fleetOwnerId) driverId
    )
    ( \driverInfo -> do
        DDriverMode.incrementFleetOperatorStatusKeyForDriver SP.FLEET_OWNER fleetOwnerId driverInfo.driverFlowStatus
    )
  pure fleetLinkDriver
