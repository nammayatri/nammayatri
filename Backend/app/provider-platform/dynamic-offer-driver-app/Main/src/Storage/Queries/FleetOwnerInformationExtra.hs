module Storage.Queries.FleetOwnerInformationExtra where

import Control.Lens ((^..), _Just, to)
import qualified Domain.Types.FleetOwnerInformation
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOwnerInformation as Beam
import Storage.Queries.OrphanInstances.FleetOwnerInformation ()
import qualified Storage.Queries.Transformers.FleetOwnerInformation
import Tools.Encryption (encryptWithDefault)

findFleetOwners ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Domain.Types.FleetOwnerInformation.FleetType ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.FleetOwnerInformation.FleetOwnerInformation]
findFleetOwners merchantOperatingCityId mbFleetType mbOnlyEnabled mbBlocked limit offset =
  findAllWithOptionsKV filteredList (Se.Desc Beam.createdAt) limit offset
  where
    enabledFleets =
      [Se.Is Beam.enabled $ Se.Eq enabled | Just enabled <- [mbOnlyEnabled]]
    combinedFilters =
      [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just $ getId merchantOperatingCityId)]
        <> [Se.Is Beam.fleetType $ Se.Eq fleetType | Just fleetType <- [mbFleetType]]
        <> enabledFleets
        <> [Se.Is Beam.blocked $ Se.Eq blocked | Just blocked <- [mbBlocked]]
    filteredList = [Se.And combinedFilters | not (null combinedFilters)]

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
          <> (mbEnabled ^.. _Just . to (\e -> Se.Is Beam.enabled $ Se.Eq e))
          <> (mbVerified ^.. _Just . to (\v -> Se.Is Beam.verified $ Se.Eq v))
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

updateFleetOwnerInfo ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Domain.Types.FleetOwnerInformation.FleetOwnerInformation ->
  m ()
updateFleetOwnerInfo fleetOwnerInfo = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.stripeIdNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted fleetOwnerInfo.stripeIdNumber),
      Se.Set Beam.stripeIdNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash fleetOwnerInfo.stripeIdNumber),
      Se.Set Beam.stripeAddress (fmap toJSON fleetOwnerInfo.stripeAddress),
      Se.Set Beam.fleetDob fleetOwnerInfo.fleetDob,
      Se.Set Beam.fleetName fleetOwnerInfo.fleetName,
      Se.Set Beam.fleetType fleetOwnerInfo.fleetType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerInfo.fleetOwnerPersonId)]

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
