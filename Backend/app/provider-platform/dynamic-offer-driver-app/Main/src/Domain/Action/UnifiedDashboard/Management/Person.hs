{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UnifiedDashboard.Management.Person
  ( postPersonCreate,
    postUserLoginSendOtp,
  )
where

import qualified API.Types.UnifiedDashboard.Management.Person
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (encrypt, getDbHash)
import Kernel.Sms.Config (SmsConfig, useFakeSms)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonExtra as QPExtra
import Tools.Auth
import Tools.Error
import qualified Tools.SMS as Sms

postPersonCreate :: (Kernel.Types.Id.ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UnifiedDashboard.Management.Person.CreatePersonReq -> Environment.Flow API.Types.UnifiedDashboard.Management.Person.CreatePersonResp)
postPersonCreate merchantShortId opCity req = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> ", city: " <> show opCity)
  role <- parseRoleName req.roleName
  whenJust req.email $ \email -> do
    mbExistingPerson <- QPExtra.findByEmailAndMerchantIdAndRole (Just email) merchant.id role
    whenJust mbExistingPerson $ \_ ->
      throwError $ InvalidRequest "Email already registered"
  mobileHash <- getDbHash req.mobileNumber
  mbExistingPersonByMobile <- QPExtra.findByMobileNumberAndMerchantAndRole req.mobileCountryCode mobileHash merchant.id role
  whenJust mbExistingPersonByMobile $ \_ ->
    throwError $ InvalidRequest "Phone already registered"
  personId <- generateGUID
  now <- getCurrentTime
  encryptedMobileNumber <- encrypt req.mobileNumber
  let identifierType = maybe DP.MOBILENUMBER (const DP.EMAIL) req.email
  let person =
        DP.Person
          { id = personId,
            firstName = req.firstName,
            middleName = Nothing,
            lastName = Just req.lastName,
            role = role,
            gender = DP.UNKNOWN,
            hometown = Nothing,
            languagesSpoken = Nothing,
            identifierType = identifierType,
            email = req.email,
            passwordHash = Nothing,
            mobileNumber = Just encryptedMobileNumber,
            mobileCountryCode = Just req.mobileCountryCode,
            maskedMobileDigits = Just (T.takeEnd 4 req.mobileNumber),
            identifier = Nothing,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCity.id,
            isNew = False,
            onboardedFromDashboard = True,
            deviceToken = Nothing,
            language = Nothing,
            description = Nothing,
            createdAt = now,
            updatedAt = now,
            clientBundleVersion = Nothing,
            clientSdkVersion = Nothing,
            clientConfigVersion = Nothing,
            clientDevice = Nothing,
            backendConfigVersion = Nothing,
            backendAppVersion = Nothing,
            whatsappNotificationEnrollStatus = Nothing,
            alternateMobileNumber = Nothing,
            faceImageId = Nothing,
            qrImageId = Nothing,
            totalEarnedCoins = 0,
            usedCoins = 0,
            registrationLat = Nothing,
            registrationLon = Nothing,
            useFakeOtp = Nothing,
            clientId = Nothing,
            driverTag = Nothing,
            nyClubConsent = Just False,
            reactBundleVersion = Nothing,
            cloudType = Nothing -- FIXME just to make it compiled
          }
  QP.create person
  pure $ API.Types.UnifiedDashboard.Management.Person.CreatePersonResp {personId = Kernel.Types.Id.cast personId}

postUserLoginSendOtp ::
  ( Kernel.Types.Id.ShortId DM.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    API.Types.UnifiedDashboard.Management.Person.SendOtpReq ->
    Environment.Flow API.Types.UnifiedDashboard.Management.Person.SendOtpResp
  )
postUserLoginSendOtp merchantShortId opCity req = do
  merchant <- SMerchant.findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> ", city: " <> show opCity)

  -- Check for fake OTP from config only (no person lookup)
  smsCfg <- asks (.smsCfg)
  let useFakeOtpM = show <$> useFakeSms smsCfg
  otp <- maybe generateOTPCode return useFakeOtpM

  -- Only send OTP via SMS if fake OTP is not configured
  -- Send SMS directly without needing personId
  whenNothing_ useFakeOtpM $ do
    let phoneNumber = req.mobileCountryCode <> req.mobileNumber
        otpHash = smsCfg.credConfig.otpHash
    (mbSender, message, templateId) <-
      MessageBuilder.buildSendOTPMessage merchantOpCity.id $
        MessageBuilder.BuildSendOTPMessageReq
          { otp = otp,
            hash = otpHash
          }
    let sender = fromMaybe smsCfg.sender mbSender
    Sms.sendSMS merchant.id merchantOpCity.id (Sms.SendSMSReq message phoneNumber sender templateId)
      >>= Sms.checkSmsResult

  -- Return OTP to Dashboard so it can store in cache
  pure $ API.Types.UnifiedDashboard.Management.Person.SendOtpResp {otp = otp}

parseRoleName :: T.Text -> Environment.Flow DP.Role -- TODO: Try to remove this
parseRoleName roleName =
  case T.toUpper roleName of
    "DRIVER" -> pure DP.DRIVER
    "ADMIN" -> pure DP.ADMIN
    "FLEET_OWNER" -> pure DP.FLEET_OWNER
    "FLEET_BUSINESS" -> pure DP.FLEET_BUSINESS
    "OPERATOR" -> pure DP.OPERATOR
    _ -> throwError $ InvalidRequest $ "Invalid role name: " <> roleName
