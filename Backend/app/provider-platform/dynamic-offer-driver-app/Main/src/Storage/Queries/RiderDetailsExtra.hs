module Storage.Queries.RiderDetailsExtra where

import Data.Maybe
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
import Storage.Queries.OrphanInstances.RiderDetails ()

-- Extra code goes here --

findByMobileNumberAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberAndMerchant mobileNumber_ (Id merchantId) = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  findOneWithKV [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.merchantId $ Se.Eq merchantId]]

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

findAllRiderDetailsWithOptions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Int -> Int -> Maybe UTCTime -> Maybe UTCTime -> Bool -> Maybe (Id Person) -> Maybe DbHash -> m [RiderDetails]
findAllRiderDetailsWithOptions merchantId limit offset mbFrom mbTo areActivatedRidesOnly mbReferredByDriver mbMobileNumberHash = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is BeamRD.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is BeamRD.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is BeamRD.merchantId $ Se.Eq merchantId.getId]
            <> [Se.Is BeamRD.referredByDriver $ Se.Eq (mbReferredByDriver <&> (.getId)) | isJust mbReferredByDriver]
            <> [Se.Is BeamRD.referredByDriver $ Se.Not $ Se.Eq Nothing] -- filter the ones which are referred
            <> [Se.Is BeamRD.mobileNumberHash $ Se.Eq (fromJust mbMobileNumberHash) | isJust mbMobileNumberHash]
            <> [Se.Is BeamRD.payoutFlagReason $ Se.Eq Nothing | areActivatedRidesOnly == True] -- show only which are not frauds i.e. Nothing as flag
        )
    ]
    (Se.Desc BeamRD.createdAt)
    (Just limit)
    (Just offset)
