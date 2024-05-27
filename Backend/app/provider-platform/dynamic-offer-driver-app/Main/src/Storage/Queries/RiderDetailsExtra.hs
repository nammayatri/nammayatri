{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RiderDetailsExtra where

import Domain.Types.DriverReferral
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.RiderDetails as DRDD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RiderDetails as BeamRD
import Storage.Queries.OrphanInstances.RiderDetails

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

addToFavDriverList :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Id Person -> m ()
addToFavDriverList riderId driverId = do
  mbRiderDetail <- findOneWithKV [Se.Is BeamRD.id $ Se.Eq riderId]
  case mbRiderDetail of
    Just riderDetail -> do
      let favDriverList = riderDetail.favDriverList
      unless (driverId.getId `elem` favDriverList) $ do
        updateOneWithKV
          [Se.Set BeamRD.favDriverList (favDriverList <> [driverId.getId])]
          [Se.Is BeamRD.id (Se.Eq riderId)]
    Nothing -> do
      logError $ "Rider details doesn't exist when adding driver to fav list for riderID : " <> riderId
      pure ()

removeFavouriteDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RiderDetails -> Text -> m ()
removeFavouriteDriver (Id riderId) driverId = do
  mbRiderDetail <- findOneWithKV [Se.Is BeamRD.id $ Se.Eq riderId]
  case mbRiderDetail of
    Just riderDetail -> do
      let favDriverList = riderDetail.favDriverList
          newFavDriverList = filter (/= driverId) favDriverList
      updateOneWithKV
        [Se.Set BeamRD.favDriverList newFavDriverList]
        [Se.Is BeamRD.id (Se.Eq riderId)]
    Nothing -> do
      logError $ "Rider details doesn't exist when removing driver to fav list for riderID : " <> riderId
      pure ()
