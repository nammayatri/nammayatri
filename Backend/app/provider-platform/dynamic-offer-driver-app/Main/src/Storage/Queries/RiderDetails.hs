{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RiderDetails where

import Domain.Types.DriverReferral
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.RiderDetails as DRDD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RiderDetails as BeamRD

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DRDD.RiderDetails -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RiderDetails -> m (Maybe RiderDetails)
findById (Id riderDetailsId) = findOneWithKV [Se.Is BeamRD.id $ Se.Eq riderDetailsId]

findByMobileNumberAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberAndMerchant mobileNumber_ (Id merchantId) = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  findOneWithKV [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.merchantId $ Se.Eq merchantId]]

updateHasTakenValidRide :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RiderDetails -> m ()
updateHasTakenValidRide (Id riderId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamRD.hasTakenValidRide True,
      Se.Set BeamRD.hasTakenValidRideAt (Just now),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateOtpCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RiderDetails -> Text -> m ()
updateOtpCode (Id riderId) otpCode = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamRD.otpCode $ Just otpCode, Se.Set BeamRD.updatedAt now]
    [Se.Is BeamRD.id (Se.Eq riderId)]

findAllReferredByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [RiderDetails]
findAllReferredByDriverId (Id driverId) = findAllWithDb [Se.Is BeamRD.referredByDriver $ Se.Eq (Just driverId)]

findByMobileNumberHashAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberHashAndMerchant mobileNumberDbHash (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.merchantId $ Se.Eq merchantId]]

updateReferralInfo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> Id Merchant -> Id DriverReferral -> Id Person -> m ()
updateReferralInfo customerNumberHash merchantId referralId driverId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamRD.referralCode (Just $ getId referralId),
      Se.Set BeamRD.referredByDriver (Just $ getId driverId),
      Se.Set BeamRD.referredAt (Just now)
    ]
    [Se.And [Se.Is BeamRD.mobileNumberHash (Se.Eq customerNumberHash), Se.Is BeamRD.merchantId (Se.Eq $ getId merchantId)]]

instance FromTType' BeamRD.RiderDetails RiderDetails where
  fromTType' BeamRD.RiderDetailsT {..} = do
    pure $
      Just
        RiderDetails
          { id = Id id,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            createdAt = createdAt,
            updatedAt = updatedAt,
            referralCode = Id <$> referralCode,
            referredByDriver = Id <$> referredByDriver,
            referredAt = referredAt,
            hasTakenValidRide = hasTakenValidRide,
            hasTakenValidRideAt = hasTakenValidRideAt,
            merchantId = Id merchantId,
            otpCode = otpCode
          }

instance ToTType' BeamRD.RiderDetails RiderDetails where
  toTType' RiderDetails {..} = do
    BeamRD.RiderDetailsT
      { BeamRD.id = getId id,
        BeamRD.mobileCountryCode = mobileCountryCode,
        BeamRD.mobileNumberEncrypted = unEncrypted mobileNumber.encrypted,
        BeamRD.mobileNumberHash = mobileNumber.hash,
        BeamRD.createdAt = createdAt,
        BeamRD.updatedAt = updatedAt,
        BeamRD.referralCode = getId <$> referralCode,
        BeamRD.referredByDriver = getId <$> referredByDriver,
        BeamRD.referredAt = referredAt,
        BeamRD.hasTakenValidRide = hasTakenValidRide,
        BeamRD.hasTakenValidRideAt = hasTakenValidRideAt,
        BeamRD.merchantId = getId merchantId,
        BeamRD.otpCode = otpCode
      }
