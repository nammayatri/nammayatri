{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.DriverReferee where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.DriverReferral as Domain
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.RiderDetails as DRD
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (encrypt, getDbHash)
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.RiderDetails as QRD

data RefereeLinkInfoReq = RefereeLinkInfoReq
  { referralCode :: Id Domain.DriverReferral,
    customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

linkReferee ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Id Merchant ->
  Maybe Text ->
  RefereeLinkInfoReq ->
  m APISuccess
linkReferee merchantId apiKey RefereeLinkInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  unless (TU.validateAllDigitWithMinLength 6 referralCode.getId) $
    throwError $ InvalidRequest "Referral Code must have 6 digits"
  numberHash <- getDbHash customerMobileNumber
  driverReferralLinkage <- QDR.findByRefferalCode referralCode >>= fromMaybeM (InvalidRequest "Invalid referral code.")
  mbRiderDetails <- QRD.findByMobileNumberHashAndMerchant numberHash merchant.id
  _ <- case mbRiderDetails of
    Just _ -> QRD.updateReferralInfo numberHash merchant.id referralCode driverReferralLinkage.driverId
    Nothing -> do
      riderDetails <- mkRiderDetailsObj driverReferralLinkage.driverId
      QRD.create riderDetails
  pure Success
  where
    mkRiderDetailsObj driverId = do
      id <- generateGUID
      now <- getCurrentTime
      otp <- generateOTPCode
      encPhoneNumber <- encrypt customerMobileNumber
      pure $
        DRD.RiderDetails
          { id = Id id,
            mobileCountryCode = customerMobileCountryCode,
            mobileNumber = encPhoneNumber,
            merchantId,
            createdAt = now,
            updatedAt = now,
            referralCode = Just referralCode,
            referredByDriver = Just driverId,
            referredAt = Just now,
            hasTakenValidRide = False,
            hasTakenValidRideAt = Nothing,
            otpCode = Just otp,
            nightSafetyChecks = True
          }
