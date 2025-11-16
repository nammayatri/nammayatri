module Storage.Queries.FleetOwnerInformationExtra where

import qualified Domain.Types.FleetOwnerInformation
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOwnerInformation as Beam
import Storage.Queries.OrphanInstances.FleetOwnerInformation ()
import qualified Storage.Queries.Transformers.FleetOwnerInformation
import Tools.Encryption (encryptWithDefault)

findByPersonIdAndEnabledAndVerified ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Bool ->
  Maybe Bool ->
  Kernel.Types.Id.Id DP.Person ->
  m (Maybe Domain.Types.FleetOwnerInformation.FleetOwnerInformation)
findByPersonIdAndEnabledAndVerified mbEnabled mbVerified fleetOwnerPersonId = do
  findOneWithKV
    [ Se.And $
        [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]
          <> [Se.Is Beam.enabled $ Se.Eq (fromJust mbEnabled) | isJust mbEnabled]
          <> [Se.Is Beam.verified $ Se.Eq (fromJust mbVerified) | isJust mbVerified]
    ]

updateAadhaarImage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (EncryptedHashed Text) ->
  Maybe Text ->
  Maybe Text ->
  Kernel.Types.Id.Id DP.Person ->
  m ()
updateAadhaarImage aadhaarNumber aadhaarFrontImageId aadhaarBackImageId fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.aadhaarNumber Nothing,
      Se.Set Beam.aadhaarNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted aadhaarNumber),
      Se.Set Beam.aadhaarNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash aadhaarNumber),
      Se.Set Beam.aadhaarFrontImageId aadhaarFrontImageId,
      Se.Set Beam.aadhaarBackImageId aadhaarBackImageId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateBusinessLicenseImageAndNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Maybe (EncryptedHashed Text) ->
  Kernel.Types.Id.Id DP.Person ->
  m ()
updateBusinessLicenseImageAndNumber businessLicenseImageId businessLicenseNumber fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.businessLicenseImageId businessLicenseImageId,
      Se.Set Beam.businessLicenseNumber Nothing,
      Se.Set Beam.businessLicenseNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted businessLicenseNumber),
      Se.Set Beam.businessLicenseNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash businessLicenseNumber),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateFleetOwnerGstNumberAndEnabledStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (EncryptedHashed Text) ->
  Bool ->
  Kernel.Types.Id.Id DP.Person ->
  m ()
updateFleetOwnerGstNumberAndEnabledStatus gstNumber enabled fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.gstNumber Nothing,
      Se.Set Beam.gstNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted gstNumber),
      Se.Set Beam.gstNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash gstNumber),
      Se.Set Beam.enabled enabled,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateGstImage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (EncryptedHashed Text) ->
  Maybe Text ->
  Kernel.Types.Id.Id DP.Person ->
  m ()
updateGstImage gstNumber gstImageId fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.gstNumber Nothing,
      Se.Set Beam.gstNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted gstNumber),
      Se.Set Beam.gstNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash gstNumber),
      Se.Set Beam.gstImageId gstImageId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updatePanImage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe (EncryptedHashed Text) ->
  Maybe Text ->
  Kernel.Types.Id.Id DP.Person ->
  m ()
updatePanImage panNumber panImageId fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.panNumber Nothing,
      Se.Set Beam.panNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted panNumber),
      Se.Set Beam.panNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash panNumber),
      Se.Set Beam.panImageId panImageId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateByPrimaryKey ::
  (EncFlow m r, EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Domain.Types.FleetOwnerInformation.FleetOwnerInformation ->
  m ()
updateByPrimaryKey fleetOwnerInfo = do
  gstNumber' <- encryptWithDefault fleetOwnerInfo.gstNumber fleetOwnerInfo.gstNumberDec
  panNumber' <- encryptWithDefault fleetOwnerInfo.panNumber fleetOwnerInfo.panNumberDec
  aadhaarNumber' <- encryptWithDefault fleetOwnerInfo.aadhaarNumber fleetOwnerInfo.aadhaarNumberDec
  businessLicenseNumber' <- encryptWithDefault fleetOwnerInfo.businessLicenseNumber fleetOwnerInfo.businessLicenseNumberDec
  updateByPrimaryKey'
    fleetOwnerInfo{gstNumber = gstNumber',
                   panNumber = panNumber',
                   aadhaarNumber = aadhaarNumber',
                   businessLicenseNumber = businessLicenseNumber'
                  }
  where
    -- don't use directly without backfilling
    updateByPrimaryKey' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FleetOwnerInformation.FleetOwnerInformation -> m ()
    updateByPrimaryKey' (Domain.Types.FleetOwnerInformation.FleetOwnerInformation {..}) = do
      _now <- getCurrentTime
      updateWithKV
        [ Se.Set Beam.aadhaarBackImageId aadhaarBackImageId,
          Se.Set Beam.aadhaarFrontImageId aadhaarFrontImageId,
          Se.Set Beam.aadhaarNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted aadhaarNumber),
          Se.Set Beam.aadhaarNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash aadhaarNumber),
          Se.Set Beam.aadhaarNumber Nothing,
          Se.Set Beam.blocked blocked,
          Se.Set Beam.businessLicenseImageId businessLicenseImageId,
          Se.Set Beam.businessLicenseNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted businessLicenseNumber),
          Se.Set Beam.businessLicenseNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash businessLicenseNumber),
          Se.Set Beam.businessLicenseNumber Nothing,
          Se.Set Beam.enabled enabled,
          Se.Set Beam.fleetType fleetType,
          Se.Set Beam.gstImageId gstImageId,
          Se.Set Beam.gstNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted gstNumber),
          Se.Set Beam.gstNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash gstNumber),
          Se.Set Beam.gstNumber Nothing,
          Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
          Se.Set Beam.panImageId panImageId,
          Se.Set Beam.panNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted panNumber),
          Se.Set Beam.panNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash panNumber),
          Se.Set Beam.panNumber Nothing,
          Se.Set Beam.referredByOperatorId referredByOperatorId,
          Se.Set Beam.registeredAt registeredAt,
          Se.Set Beam.verified verified,
          Se.Set Beam.createdAt createdAt,
          Se.Set Beam.updatedAt _now
        ]
        [Se.And [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]]

getFleetOwnerByTicketPlaceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  m [Domain.Types.FleetOwnerInformation.FleetOwnerInformation]
getFleetOwnerByTicketPlaceId mbTicketPlaceId = do
  findAllWithKV
    [ Se.And $
        [Se.Is Beam.enabled $ Se.Eq True]
          <> [Se.Is Beam.verified $ Se.Eq True]
          <> [Se.Is Beam.ticketPlaceId $ Se.Eq mbTicketPlaceId]
    ]

-- Update prepaidSubscriptionBalance and lienAmount together for a fleet owner in one DB write.
updatePrepaidSubscriptionBalanceAndLienAmount ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Id DP.Person ->
  m ()
updatePrepaidSubscriptionBalanceAndLienAmount prepaidSubscriptionBalance lienAmount fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.prepaidSubscriptionBalance prepaidSubscriptionBalance,
      Se.Set Beam.lienAmount lienAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (getId fleetOwnerPersonId)]
