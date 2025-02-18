module Storage.Queries.VehicleRegistrationCertificateExtra where

import qualified Database.Beam as B
import Domain.Types.Image
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.VehicleRegistrationCertificate
import Domain.Types.VehicleVariant as Vehicle
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverRCAssociation as BeamDRA
import qualified Storage.Beam.VehicleRegistrationCertificate as BeamVRC
import Storage.Queries.OrphanInstances.VehicleRegistrationCertificate ()

-- Extra code goes here --
upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => VehicleRegistrationCertificate -> m ()
upsert a@VehicleRegistrationCertificate {..} = do
  res <- findOneWithKV [Se.Is BeamVRC.certificateNumberHash $ Se.Eq (a.certificateNumber & (.hash))]
  if isJust res
    then
      updateWithKV
        [ Se.Set BeamVRC.permitExpiry permitExpiry,
          Se.Set BeamVRC.pucExpiry pucExpiry,
          Se.Set BeamVRC.insuranceValidity insuranceValidity,
          Se.Set BeamVRC.vehicleClass vehicleClass,
          Se.Set BeamVRC.vehicleVariant vehicleVariant,
          Se.Set BeamVRC.vehicleManufacturer vehicleManufacturer,
          Se.Set BeamVRC.manufacturerModel manufacturerModel,
          Se.Set BeamVRC.vehicleCapacity vehicleCapacity,
          Se.Set BeamVRC.vehicleModel vehicleModel,
          Se.Set BeamVRC.vehicleColor vehicleColor,
          Se.Set BeamVRC.vehicleEnergyType vehicleEnergyType,
          Se.Set BeamVRC.verificationStatus verificationStatus,
          Se.Set BeamVRC.reviewedAt reviewedAt,
          Se.Set BeamVRC.failedRules failedRules,
          Se.Set BeamVRC.fleetOwnerId fleetOwnerId,
          Se.Set BeamVRC.fitnessExpiry fitnessExpiry,
          Se.Set BeamVRC.reviewRequired reviewRequired,
          Se.Set BeamVRC.airConditioned airConditioned,
          Se.Set BeamVRC.luggageCapacity luggageCapacity,
          Se.Set BeamVRC.userPassedVehicleCategory userPassedVehicleCategory,
          Se.Set BeamVRC.mYManufacturing mYManufacturing,
          Se.Set BeamVRC.updatedAt updatedAt
        ]
        [Se.Is BeamVRC.certificateNumberHash $ Se.Eq (a.certificateNumber & (.hash))]
    else createWithKV a

findLastVehicleRC :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRC certNumberHash = do
  findAllWithOptionsKV [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash] (Se.Desc BeamVRC.fitnessExpiry) Nothing Nothing <&> listToMaybe

updateVehicleVariant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id VehicleRegistrationCertificate -> Maybe Vehicle.VehicleVariant -> Maybe Bool -> Maybe Bool -> m ()
updateVehicleVariant (Id vehicleRegistrationCertificateId) variant reviewDone reviewRequired = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set BeamVRC.updatedAt now]
        <> [Se.Set BeamVRC.reviewedAt (Just now) | isJust reviewDone]
        <> [Se.Set BeamVRC.reviewRequired reviewRequired | isJust reviewRequired]
        <> [Se.Set BeamVRC.vehicleVariant variant | isJust variant]
        <> [Se.Set BeamVRC.verificationStatus Documents.VALID | isJust variant]
    )
    [Se.Is BeamVRC.id (Se.Eq vehicleRegistrationCertificateId)]

findByRCAndExpiry :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => EncryptedHashedField 'AsEncrypted Text -> UTCTime -> m (Maybe VehicleRegistrationCertificate)
findByRCAndExpiry certNumber expiry = do
  let certNumberHash = certNumber & (.hash)
  findOneWithKV [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash, Se.Is BeamVRC.fitnessExpiry $ Se.Eq expiry]]

findAllById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id VehicleRegistrationCertificate] -> m [VehicleRegistrationCertificate]
findAllById rcIds = findAllWithKV [Se.Is BeamVRC.id $ Se.In $ map (.getId) rcIds]

findAllByImageId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Image] -> m [VehicleRegistrationCertificate]
findAllByImageId imageIds = findAllWithKV [Se.Is BeamVRC.documentImageId $ Se.In $ map (.getId) imageIds]

findLastVehicleRCWrapper :: (MonadFlow m, EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCWrapper certNumber = do
  certNumberHash <- getDbHash certNumber
  runInReplica $ findLastVehicleRC certNumberHash

findLastVehicleRCFleet :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DbHash -> Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCFleet certNumberHash fleetOwnerId = do
  findAllWithOptionsKV [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash, Se.Is BeamVRC.fleetOwnerId $ Se.Eq $ Just fleetOwnerId]] (Se.Desc BeamVRC.updatedAt) Nothing Nothing <&> listToMaybe

findLastVehicleRCFleet' :: (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCFleet' certNumber fleetOwnerId = do
  certNumberHash <- getDbHash certNumber
  runInReplica $ findLastVehicleRCFleet certNumberHash fleetOwnerId

findByCertificateNumberHash ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (DbHash -> m (Maybe VehicleRegistrationCertificate))
findByCertificateNumberHash certificateHash = do
  findOneWithKV
    [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certificateHash]

findAllRCByStatusForFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Documents.VerificationStatus -> Maybe Bool -> Integer -> Integer -> Id Merchant.Merchant -> m [VehicleRegistrationCertificate]
findAllRCByStatusForFleet fleetOwnerId status mbRcActive limitVal offsetVal (Id merchantId') = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\(rc', _) -> B.desc_ rc'.createdAt) $
                B.filter_'
                  ( \(rc, driverRcAssociation) ->
                      rc.merchantId B.==?. B.val_ (Just merchantId')
                        B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                        B.&&?. rc.verificationStatus B.==?. B.val_ status
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isRcActive -> driverRcAssociation.isRcActive B.==?. B.val_ isRcActive) mbRcActive
                  )
                  do
                    rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                    driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                    pure (rc, driverRcAssociation)
  case res of
    Right res' -> do
      let rcList = fst <$> res'
      catMaybes <$> mapM fromTType' rcList
    Left _ -> pure []

findAllInactiveRCForFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Integer -> Integer -> Id Merchant.Merchant -> m [VehicleRegistrationCertificate]
findAllInactiveRCForFleet fleetOwnerId limitVal offsetVal merchantId = do
  allActiveRCs <- findAllActiveRCForFleet fleetOwnerId Documents.VALID merchantId
  -- now find all the rc which are not in this list
  let allActiveRCIds = map (.id.getId) allActiveRCs
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\rc -> B.desc_ rc.createdAt) $
                B.filter_'
                  ( \rc ->
                      rc.merchantId B.==?. B.val_ (Just merchantId.getId)
                        B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                        B.&&?. B.sqlBool_ (B.not_ (rc.id `B.in_` (B.val_ <$> allActiveRCIds)))
                  )
                  $ B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

findAllActiveRCForFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Documents.VerificationStatus -> Id Merchant.Merchant -> m [VehicleRegistrationCertificate]
findAllActiveRCForFleet fleetOwnerId status (Id merchantId') = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.orderBy_ (\(rc', _) -> B.desc_ rc'.createdAt) $
            B.filter_'
              ( \(rc, driverRcAssociation) ->
                  rc.merchantId B.==?. B.val_ (Just merchantId')
                    B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                    B.&&?. rc.verificationStatus B.==?. B.val_ status
                    B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                    B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
              )
              do
                rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                pure (rc, driverRcAssociation)
  case res of
    Right res' -> do
      let rcList = fst <$> res'
      catMaybes <$> mapM fromTType' rcList
    Left _ -> pure []

countAllActiveRCForFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Id Merchant.Merchant -> m Int
countAllActiveRCForFleet fleetOwnerId (Id merchantId') = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
            B.filter_'
              ( \(rc, driverRcAssociation) ->
                  rc.merchantId B.==?. B.val_ (Just merchantId')
                    B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                    B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                    B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
              )
              do
                rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                pure (rc, driverRcAssociation)
  pure $ either (const 0) (\r -> if null r then 0 else head r) res

updateVerificationStatusAndRejectReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Documents.VerificationStatus -> Text -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatusAndRejectReason verificationStatus rejectReason (Kernel.Types.Id.Id imageId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamVRC.verificationStatus verificationStatus, Se.Set BeamVRC.rejectReason (Just rejectReason), Se.Set BeamVRC.updatedAt _now] [Se.Is BeamVRC.documentImageId $ Se.Eq imageId]

findAllByFleetOwnerIdAndSearchString :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Integer -> Integer -> Id Merchant.Merchant -> Text -> Maybe DbHash -> m [VehicleRegistrationCertificate]
findAllByFleetOwnerIdAndSearchString limit offset (Id merchantId') fleetOwnerId mbSearchStringHash = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limit $
            B.offset_ offset $
              B.orderBy_ (\rc' -> B.desc_ rc'.updatedAt) $
                B.filter_'
                  ( \rc ->
                      rc.merchantId B.==?. B.val_ (Just merchantId')
                        B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash) mbSearchStringHash
                  )
                  $ B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []
