module Domain.Action.Internal.SendEmailOTP
  ( sendEmailOTP,
    Email.OTP.Types.SendEmailOTPReq (..),
    Email.OTP.Types.SendEmailOTPRes (..),
  )
where

import qualified Control.Exception as E
import qualified Email.Flow as Email
import Email.OTP.Types
import Environment
import EulerHS.Prelude (whenNothing_)
import Kernel.Prelude
import Kernel.Sms.Config (useFakeSms)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import Tools.Error

sendEmailOTP ::
  Maybe Text ->
  Text ->
  Context.City ->
  SendEmailOTPReq ->
  Flow SendEmailOTPRes
sendEmailOTP apiKey merchantShortIdText city req = do
  merchant <- findMerchantByShortId (ShortId merchantShortIdText)
  dashboardToken <- asks (.dashboardToken)
  unless (Just dashboardToken == apiKey) $
    throwError (AuthBlocked "Invalid Rider App dashboard token")
  merchantOpCityId <- CQMOC.getMerchantOpCityId merchant (Just city)
  smsCfg <- asks (.smsCfg)
  let useFakeOtpM = show <$> useFakeSms smsCfg
  otp <- maybe generateOTPCode pure useFakeOtpM
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOpCityId.getId)
  emailOTPConfig <- riderConfig.emailOtpConfig & fromMaybeM (RiderConfigNotFound $ "Email OTP config not found for merchantOperatingCityId:- " <> merchantOpCityId.getId)
  emailServiceConfig <- asks (.emailServiceConfig)
  whenNothing_ useFakeOtpM $ do
    result <- liftIO $ E.try @E.SomeException $ Email.sendEmail emailServiceConfig emailOTPConfig [req.email] otp
    case result of
      Left err -> do
        logError $ "Failed to send email OTP: " <> show err
        throwError $ InternalError "Failed to send email OTP"
      Right _ -> pure ()
  pure $ SendEmailOTPRes {otp = Just otp}
