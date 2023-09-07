{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.Registration
  ( fleetOwnerLogin,
    fleetOwnerVerify,
    FleetOwnerLoginReq (..),
    FleetOwnerVerifyRes (..),
  )
where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Kernel.Sms.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.CachedQueries.Merchant as QMerchant
import Tools.Error
import Tools.SMS as Sms hiding (Success)

---------------------------------------------------------------------
data FleetOwnerLoginReq = FleetOwnerLoginReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    merchantId :: Text,
    otp :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

newtype FleetOwnerVerifyRes = FleetOwnerVerifyRes
  { authToken :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

fleetOwnerLogin ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r
  ) =>
  FleetOwnerLoginReq ->
  m APISuccess
fleetOwnerLogin req = do
  runRequestValidation validateInitiateLoginReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  let merchantId = ShortId req.merchantId
  merchant <-
    QMerchant.findByShortId merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getShortId)
  let useFakeOtpM = useFakeSms smsCfg
  otp <- maybe generateOTPCode (return . show) useFakeOtpM
  whenNothing_ useFakeOtpM $ do
    let otpHash = smsCfg.credConfig.otpHash
    let otpCode = otp
        phoneNumber = countryCode <> mobileNumber
        sender = smsCfg.sender
    withLogTag ("mobileNumber" <> req.mobileNumber) $
      do
        message <-
          MessageBuilder.buildSendOTPMessage merchant.id $
            MessageBuilder.BuildSendOTPMessageReq
              { otp = otpCode,
                hash = otpHash
              }
        Sms.sendSMS merchant.id (Sms.SendSMSReq message phoneNumber sender)
        >>= Sms.checkSmsResult
  let key = makeMobileNumberOtpKey mobileNumber
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Redis.setExp key otp expTime
  pure Success

fleetOwnerVerify ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r
  ) =>
  FleetOwnerLoginReq ->
  m APISuccess
fleetOwnerVerify req = do
  case req.otp of
    Just otp -> do
      mobileNumberOtpKey <- Redis.safeGet $ makeMobileNumberOtpKey req.mobileNumber
      case mobileNumberOtpKey of
        Just otpHash -> do
          unless (otpHash == otp) $ throwError InvalidAuthData
          pure Success
        Nothing -> throwError InvalidAuthData
    _ -> throwError InvalidAuthData

makeMobileNumberOtpKey :: Text -> Text
makeMobileNumberOtpKey mobileNumber = "MobileNumberOtp:mobileNumber-" <> mobileNumber

validateInitiateLoginReq :: Validate FleetOwnerLoginReq
validateInitiateLoginReq FleetOwnerLoginReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]
