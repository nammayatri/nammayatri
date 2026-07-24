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
import Kernel.Prelude
import Kernel.Sms.Config (useFakeSms)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))

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
    throwError (AuthBlocked "Invalid BPP dashboard token")
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just city)
  smsCfg <- asks (.smsCfg)
  let useFakeOtpM = show <$> useFakeSms smsCfg
  otp <- maybe generateOTPCode pure useFakeOtpM
  -- When useFakeSms is enabled (dev/CI), short-circuit: don't require the
  -- transporter_config / emailOtpConfig rows to exist and don't hit SMTP.
  case useFakeOtpM of
    Just _ -> pure $ SendEmailOTPRes {otp = Just otp}
    Nothing -> do
      transporterConfig <-
        getOneConfig
          (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
          (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing))
          >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      emailOTPConfig <-
        transporterConfig.emailOtpConfig
          & fromMaybeM (TransporterConfigNotFound $ "Email OTP config not found for merchantOperatingCityId:- " <> merchantOpCityId.getId)
      emailServiceConfig <- asks (.emailServiceConfig)
      result <- liftIO $ E.try @E.SomeException $ Email.sendEmail emailServiceConfig emailOTPConfig [req.email] otp
      case result of
        Left err -> do
          logError $ "Failed to send email OTP: " <> show err
          throwError $ InternalError "Failed to send email OTP"
        Right _ -> pure $ SendEmailOTPRes {otp = Just otp}
