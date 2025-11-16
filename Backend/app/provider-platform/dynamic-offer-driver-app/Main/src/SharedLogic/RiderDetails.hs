{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.RiderDetails where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RiderDetails as DRD
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RiderDetails as QRD

getRiderDetails :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r) => Currency -> Id DM.Merchant -> Maybe (Id DMOC.MerchantOperatingCity) -> Text -> Text -> Text -> Bool -> m (DRD.RiderDetails, Bool)
getRiderDetails currency merchantId mbMerchantOperatingCityId customerMobileCountryCode customerPhoneNumber bapId nightSafetyCheck = do
  now <- getCurrentTime
  QRD.findByMobileNumberAndMerchantAndBapId customerPhoneNumber merchantId bapId >>= \case
    Nothing -> do
      riderD <- QRD.findByMobileNumberAndMerchant customerPhoneNumber merchantId
      case riderD of
        Nothing -> fmap (,True) . encrypt =<< buildRiderDetails now
        Just riderDetails -> do
          let updatedRiderDetails = riderDetails{bapId = Just bapId}
          QRD.updateByPrimaryKey updatedRiderDetails
          return (updatedRiderDetails, False)
    Just a -> return (a, False)
  where
    buildRiderDetails now = do
      id <- generateGUID
      otp <- generateOTPCode
      return $
        DRD.RiderDetails
          { id = id,
            mobileCountryCode = customerMobileCountryCode,
            merchantId,
            mobileNumber = customerPhoneNumber,
            createdAt = now,
            updatedAt = now,
            referralCode = Nothing,
            referredByDriver = Nothing,
            referredAt = Nothing,
            hasTakenValidRide = False,
            hasTakenValidRideAt = Nothing,
            otpCode = Just otp,
            cancellationDues = 0.0,
            cancellationDuesPaid = 0.0,
            noOfTimesCanellationDuesPaid = 0,
            noOfTimesWaiveOffUsed = 0,
            waivedOffAmount = 0.0,
            currency,
            disputeChancesUsed = 0,
            nightSafetyChecks = nightSafetyCheck,
            firstRideId = Nothing,
            payoutFlagReason = Nothing,
            isDeviceIdExists = Nothing,
            isFlagConfirmed = Nothing,
            cancelledRides = 0,
            totalBookings = 0,
            completedRides = 0,
            validCancellations = 0,
            cancellationDueRides = 0,
            merchantOperatingCityId = mbMerchantOperatingCityId,
            bapId = Just bapId
          }
