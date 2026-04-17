module Storage.Queries.FleetOwnerInformationExtra where

import Data.Text (toLower)
import qualified Database.Beam as B
import qualified Domain.Types.FleetOwnerInformation
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.FleetOwnerInformation as Beam
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.FleetOwnerInformation ()
import Storage.Queries.OrphanInstances.Person ()
import qualified Storage.Queries.Transformers.FleetOwnerInformation
import Tools.Encryption (encryptWithDefault)

findFleetOwners ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Domain.Types.FleetOwnerInformation.FleetType ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.FleetOwnerInformation.FleetOwnerInformation]
findFleetOwners merchantOperatingCityId mbFleetType mbFromDate mbSearchString mbOnlyEnabled mbBlocked mbToDate mbLimit mbOffset = do
  searchHash <- mapM getDbHash mbSearchString
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral $ fromMaybe 10 mbLimit) $
            B.offset_ (fromIntegral $ fromMaybe 0 mbOffset) $
              B.orderBy_ (\(fleetOwnerInfo, _) -> B.desc_ fleetOwnerInfo.createdAt) $
                B.filter_'
                  ( \(fleetOwnerInfo, person) ->
                      fleetOwnerInfo.merchantOperatingCityId B.==?. B.val_ (Just $ getId merchantOperatingCityId)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\fleetType -> fleetOwnerInfo.fleetType B.==?. B.val_ fleetType) mbFleetType
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\enabled -> fleetOwnerInfo.enabled B.==?. B.val_ enabled) mbOnlyEnabled
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\blocked -> fleetOwnerInfo.blocked B.==?. B.val_ blocked) mbBlocked
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\fromDate -> B.sqlBool_ $ fleetOwnerInfo.createdAt B.>=. B.val_ fromDate) mbFromDate
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\toDate -> B.sqlBool_ $ fleetOwnerInfo.createdAt B.<=. B.val_ toDate) mbToDate
                        B.&&?. maybe
                          (B.sqlBool_ $ B.val_ True)
                          ( \searchString ->
                              B.sqlBool_ (B.lower_ (B.coalesce_ [person.email] (B.val_ "")) `B.like_` B.val_ ("%" <> toLower searchString <> "%"))
                                B.||?. maybe
                                  (B.sqlBool_ $ B.val_ False)
                                  (\hashedPhone -> person.mobileNumberHash B.==?. B.val_ (Just hashedPhone))
                                  searchHash
                          )
                          mbSearchString
                  )
                  do
                    fleetOwnerInfo <- B.all_ (BeamCommon.fleetOwnerInformation BeamCommon.atlasDB)
                    person <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\person -> Beam.fleetOwnerPersonId fleetOwnerInfo B.==. BeamP.id person)
                    pure (fleetOwnerInfo, person)
  case res of
    Right fleetOwnerInfoList -> catMaybes <$> mapM (fromTType' . fst) fleetOwnerInfoList
    Left _ -> pure []

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
      Se.Set Beam.businessLicenseNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted fleetOwnerInfo.businessLicenseNumber),
      Se.Set Beam.businessLicenseNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash fleetOwnerInfo.businessLicenseNumber),
      Se.Set Beam.businessLicenseNumber Nothing,
      Se.Set Beam.vatNumber fleetOwnerInfo.vatNumber,
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
          Se.Set Beam.vatNumber vatNumber,
          Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
          Se.Set Beam.panImageId panImageId,
          Se.Set Beam.panNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted panNumber),
          Se.Set Beam.panNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash panNumber),
          Se.Set Beam.panNumber Nothing,
          Se.Set Beam.tdsRate tdsRate,
          Se.Set Beam.referredByOperatorId referredByOperatorId,
          Se.Set Beam.registeredAt registeredAt,
          Se.Set Beam.verified verified,
          Se.Set Beam.createdAt createdAt,
          Se.Set Beam.updatedAt _now
        ]
        [Se.And [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]]

findEligibleFleetOwnersForScheduledPayout ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Int ->
  Maybe (Id DP.Person) ->
  m [Domain.Types.FleetOwnerInformation.FleetOwnerInformation]
findEligibleFleetOwnersForScheduledPayout merchantOpCityId batchSize mbLastPersonId =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId),
          Se.Is Beam.enabled $ Se.Eq True,
          Se.Is Beam.blocked $ Se.Eq False,
          Se.Is Beam.payoutVpa $ Se.Not (Se.Eq Nothing),
          Se.Is Beam.isBlockedForScheduledPayout $ Se.Not (Se.Eq (Just True))
        ]
          <> maybe [] (\lastId -> [Se.Is Beam.fleetOwnerPersonId $ Se.GreaterThan (getId lastId)]) mbLastPersonId
    ]
    (Se.Asc Beam.fleetOwnerPersonId)
    (Just batchSize)
    Nothing

updatePayoutVpaAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Maybe Domain.Types.FleetOwnerInformation.PayoutVpaStatus ->
  Kernel.Types.Id.Id DP.Person ->
  m ()
updatePayoutVpaAndStatus payoutVpa payoutVpaStatus fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.payoutVpa payoutVpa,
      Se.Set Beam.payoutVpaStatus payoutVpaStatus,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updatePayoutRegAmountRefunded ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Kernel.Types.Common.HighPrecMoney ->
  Kernel.Types.Id.Id DP.Person ->
  m ()
updatePayoutRegAmountRefunded payoutRegAmountRefunded fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.payoutRegAmountRefunded payoutRegAmountRefunded,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

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

updateVatNumberById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Kernel.Types.Id.Id DP.Person ->
  m ()
updateVatNumberById vatNumber fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.vatNumber vatNumber,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]

updateBusinessLicenseNumberById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  Maybe (EncryptedHashed Text) ->
  Kernel.Types.Id.Id DP.Person ->
  m ()
updateBusinessLicenseNumberById businessLicenseNumber fleetOwnerPersonId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.businessLicenseNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted businessLicenseNumber),
      Se.Set Beam.businessLicenseNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash businessLicenseNumber),
      Se.Set Beam.businessLicenseNumber Nothing,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]
