{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnConfirm
  ( onConfirm,
    validateRequest,
    OnConfirmReq (..),
  )
where

import qualified Domain.Types.Booking as DRB
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as QMSUC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.SMS as Sms

data OnConfirmReq = OnConfirmReq
  { bppBookingId :: Id DRB.BPPBooking,
    specialZoneOtp :: Maybe Text
  }

data ValidatedOnConfirmReq = ValidatedOnConfirmReq
  { bppBookingId :: Id DRB.BPPBooking,
    specialZoneOtp :: Maybe Text,
    booking :: DRB.Booking
  }

onConfirm :: (EncFlow m r, HasFlowEnv m r '["smsCfg" ::: SmsConfig], EsqDBFlow m r, CacheFlow m r, EsqDBReplicaFlow m r) => ValidatedOnConfirmReq -> m ()
onConfirm ValidatedOnConfirmReq {..} = do
  whenJust specialZoneOtp $ \otp -> do
    void $ QRB.updateOtpCodeBookingId booking.id otp
    fork "sending Booking confirmed dasboard sms" $ do
      let merchantOperatingCityId = booking.merchantOperatingCityId
      merchantConfig <- QMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
      if merchantConfig.enableDashboardSms
        then do
          customer <- B.runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
          mobileNumber <- mapM decrypt customer.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
          smsCfg <- asks (.smsCfg)
          let countryCode = fromMaybe "+91" customer.mobileCountryCode
          let phoneNumber = countryCode <> mobileNumber
              sender = smsCfg.sender
          message <-
            MessageBuilder.buildSendBookingOTPMessage merchantOperatingCityId $
              MessageBuilder.BuildSendBookingOTPMessageReq
                { otp = show otp,
                  amount = show booking.estimatedTotalFare
                }
          Sms.sendSMS booking.merchantId merchantOperatingCityId (Sms.SendSMSReq message phoneNumber sender) >>= Sms.checkSmsResult
        else do
          logInfo "Merchant not configured to send dashboard sms"
  void $ QRB.updateStatus booking.id DRB.CONFIRMED

validateRequest :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => OnConfirmReq -> m ValidatedOnConfirmReq
validateRequest OnConfirmReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId" <> bppBookingId.getId)
  return $ ValidatedOnConfirmReq {..}
