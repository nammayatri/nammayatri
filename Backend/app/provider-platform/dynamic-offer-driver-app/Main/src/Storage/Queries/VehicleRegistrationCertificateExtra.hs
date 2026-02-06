module Storage.Queries.VehicleRegistrationCertificateExtra where

import Data.Text (toLower)
import qualified Database.Beam as B
import Domain.Types.Image hiding (id)
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.TripTransaction as DTT
import Domain.Types.VehicleRegistrationCertificate hiding (id)
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
import qualified Storage.Beam.Person as BeamPerson
import qualified Storage.Beam.TripTransaction as BeamTripTransaction
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
          Se.Set BeamVRC.merchantId (merchantId & fmap (.getId)),
          Se.Set BeamVRC.merchantOperatingCityId (merchantOperatingCityId & fmap (.getId)),
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
          Se.Set BeamVRC.updatedAt updatedAt,
          Se.Set BeamVRC.unencryptedCertificateNumber a.unencryptedCertificateNumber
        ]
        [Se.Is BeamVRC.certificateNumberHash $ Se.Eq (a.certificateNumber & (.hash))]
    else createWithKV a

findLastVehicleRC :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRC certNumberHash = do
  findAllWithOptionsKV [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash] (Se.Desc BeamVRC.fitnessExpiry) Nothing Nothing <&> listToMaybe

findLastVehicleRCWithApproved :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> Maybe Bool -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCWithApproved certNumberHash mbApproved = do
  findAllWithOptionsKV [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash, Se.Is BeamVRC.approved $ Se.Eq mbApproved]] (Se.Desc BeamVRC.fitnessExpiry) Nothing Nothing <&> listToMaybe

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

findByRC :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => EncryptedHashedField 'AsEncrypted Text -> m (Maybe VehicleRegistrationCertificate)
findByRC certNumber = do
  let certNumberHash = certNumber & (.hash)
  findOneWithKV [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash]

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

findLastVehicleRCWrapperWithApproved :: (MonadFlow m, EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Text -> Maybe Bool -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCWrapperWithApproved certNumber mbApproved = do
  certNumberHash <- getDbHash certNumber
  runInReplica $ findLastVehicleRCWithApproved certNumberHash mbApproved

findLastVehicleRCFleet :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DbHash -> Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCFleet certNumberHash fleetOwnerId = do
  findAllWithOptionsKV [Se.And [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash, Se.Is BeamVRC.fleetOwnerId $ Se.Eq $ Just fleetOwnerId]] (Se.Desc BeamVRC.updatedAt) Nothing Nothing <&> listToMaybe

findLastVehicleRCFleet' :: (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m (Maybe VehicleRegistrationCertificate)
findLastVehicleRCFleet' certNumber fleetOwnerId = do
  certNumberHash <- getDbHash certNumber
  runInReplica $ findLastVehicleRCFleet certNumberHash fleetOwnerId

partialFindLastVehicleRCFleet :: (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> Integer -> Integer -> m [VehicleRegistrationCertificate]
partialFindLastVehicleRCFleet certNumber fleetOwnerId limit offset = do
  dbConf <- getReplicaBeamConfig
  certNumberHash <- getDbHash certNumber
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limit $
            B.offset_ offset $
              B.orderBy_ (\rc' -> B.desc_ rc'.updatedAt) $
                B.filter_'
                  ( \rc ->
                      rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                        B.&&?. ( B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower certNumber <> "%")))
                                   B.||?. (rc.certificateNumberHash B.==?. B.val_ certNumberHash)
                               )
                  )
                  $ B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

findByCertificateNumberHash ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (DbHash -> m (Maybe VehicleRegistrationCertificate))
findByCertificateNumberHash certificateHash = do
  findOneWithKV
    [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certificateHash]

findAllRCByStatusForFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> Maybe Documents.VerificationStatus -> Integer -> Integer -> Id Merchant.Merchant -> Maybe Text -> m [VehicleRegistrationCertificate]
findAllRCByStatusForFleet fleetOwnerId status limitVal offsetVal (Id merchantId') statusAwareVehicleNo = do
  dbConf <- getReplicaBeamConfig
  statusAwareVehicleNoHash <- mapM getDbHash statusAwareVehicleNo
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\rc' -> B.desc_ rc'.createdAt) $
                B.filter_'
                  ( \rc ->
                      rc.merchantId B.==?. B.val_ (Just merchantId')
                        B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\s -> rc.verificationStatus B.==?. B.val_ s) status
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> rc.certificateNumberHash B.==?. B.val_ cNum) statusAwareVehicleNoHash
                               )
                  )
                  do
                    rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                    pure rc
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

findAllInactiveRCForFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> Integer -> Integer -> Id Merchant.Merchant -> Maybe Text -> m [VehicleRegistrationCertificate]
findAllInactiveRCForFleet fleetOwnerId limitVal offsetVal merchantId statusAwareVehicleNo = do
  dbConf <- getReplicaBeamConfig
  allActiveRCs <- findAllActiveRCForFleet fleetOwnerId merchantId
  let allActiveRCIds = map (.id.getId) allActiveRCs
  statusAwareVehicleNoHash <- mapM getDbHash statusAwareVehicleNo
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
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> rc.certificateNumberHash B.==?. B.val_ cNum) statusAwareVehicleNoHash
                               )
                  )
                  do
                    rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                    pure rc
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

findAllActiveRCForFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Id Merchant.Merchant -> m [VehicleRegistrationCertificate]
findAllActiveRCForFleet fleetOwnerId (Id merchantId') = do
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
                    B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                    B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                    B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
              )
              do
                rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                pure (rc, driverRcAssociation)
  case res of
    Right res' -> do
      let rcList = (\(rc, _) -> rc) <$> res'
      catMaybes <$> mapM fromTType' rcList
    Left _ -> pure []

findAllActiveRCForFleetByLimitOffset :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> Id Merchant.Merchant -> Integer -> Integer -> Maybe Text -> Maybe Text -> m [VehicleRegistrationCertificate]
findAllActiveRCForFleetByLimitOffset fleetOwnerId (Id merchantId') limitVal offsetVal mbSearchString statusAwareVehicleNo = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  mbSearchHash <- mapM getDbHash mbSearchString
  case mbSearchString of
    Just searchString -> do
      res <-
        L.runDB dbConf $
          L.findRows $
            B.select $
              B.limit_ limitVal $
                B.offset_ offsetVal $
                  B.orderBy_ (\(rc', _, _) -> B.desc_ rc'.createdAt) $
                    B.filter_'
                      ( \(rc, driverRcAssociation, driver) ->
                          rc.merchantId B.==?. B.val_ (Just merchantId')
                            B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                            B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                            B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                            B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                            B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
                            B.&&?. ( B.sqlBool_ (B.like_ (B.lower_ driver.firstName) (B.val_ ("%" <> toLower searchString <> "%")))
                                       B.||?. B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> searchString <> "%")))
                                       B.||?. maybe (B.sqlBool_ $ B.val_ True) (\hash -> driver.mobileNumberHash B.==?. B.val_ (Just hash)) mbSearchHash
                                   )
                      )
                      do
                        rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                        driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                        driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\person -> BeamPerson.id person B.==. BeamDRA.driverId driverRcAssociation)
                        pure (rc, driverRcAssociation, driver)
      case res of
        Right res' -> do
          let rcList = (\(rc, _, _) -> rc) <$> res'
          catMaybes <$> mapM fromTType' rcList
        Left _ -> pure []
    Nothing -> do
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
                            B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                            B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                            B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
                            B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                      )
                      do
                        rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                        driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                        pure (rc, driverRcAssociation)
      case res of
        Right res' -> do
          let rcList = (\(rc, _) -> rc) <$> res'
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

countAllRCForFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Id Merchant.Merchant -> m Int
countAllRCForFleet fleetOwnerId (Id merchantId') = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
            B.filter_'
              ( \rc ->
                  rc.merchantId B.==?. B.val_ (Just merchantId')
                    B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
              )
              do
                B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if null r then 0 else head r) res

updateVerificationStatusAndRejectReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Documents.VerificationStatus -> Text -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatusAndRejectReason verificationStatus rejectReason (Kernel.Types.Id.Id imageId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamVRC.verificationStatus verificationStatus, Se.Set BeamVRC.rejectReason (Just rejectReason), Se.Set BeamVRC.updatedAt _now] [Se.Is BeamVRC.documentImageId $ Se.Eq imageId]

findAllValidRcByFleetOwnerIdAndSearchString :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Integer -> Integer -> Id Merchant.Merchant -> Text -> Maybe Text -> Maybe DbHash -> m [VehicleRegistrationCertificate]
findAllValidRcByFleetOwnerIdAndSearchString limit offset (Id merchantId') fleetOwnerId mbSearchString mbSearchStringHash = do
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
                        B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                        B.&&?. ( maybe
                                   (B.sqlBool_ $ B.val_ True)
                                   (\cNum -> B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))))
                                   mbSearchString
                                   B.||?. maybe
                                     (B.sqlBool_ $ B.val_ True)
                                     (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash)
                                     mbSearchStringHash
                               )
                  )
                  $ B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

findAllVehicleByStatusForFleetByLimitOffset :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> Id Merchant.Merchant -> Integer -> Integer -> Maybe Text -> Maybe Text -> DTT.TripStatus -> m [VehicleRegistrationCertificate]
findAllVehicleByStatusForFleetByLimitOffset fleetOwnerId (Id merchantId') limitVal offsetVal mbSearchString statusAwareVehicleNo tripStatus = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  mbSearchHash <- mapM getDbHash mbSearchString
  case mbSearchString of
    Just searchString -> do
      res <-
        L.runDB dbConf $
          L.findRows $
            B.select $
              B.limit_ limitVal $
                B.offset_ offsetVal $
                  B.orderBy_ (\(rc', _, _, _) -> B.desc_ rc'.createdAt) $
                    B.filter_'
                      ( \(rc, driverRcAssociation, driver, tripTransaction) ->
                          rc.merchantId B.==?. B.val_ (Just merchantId')
                            B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                            B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                            B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                            B.&&?. tripTransaction.status B.==?. B.val_ tripStatus
                            B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                            B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
                            B.&&?. ( B.sqlBool_ (B.like_ (B.lower_ driver.firstName) (B.val_ ("%" <> toLower searchString <> "%")))
                                       B.||?. B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> searchString <> "%")))
                                       B.||?. maybe (B.sqlBool_ $ B.val_ True) (\hash -> driver.mobileNumberHash B.==?. B.val_ (Just hash)) mbSearchHash
                                   )
                      )
                      do
                        rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                        driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                        driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\person -> BeamPerson.id person B.==. BeamDRA.driverId driverRcAssociation)
                        tripTransaction <- B.join_ (BeamCommon.tripTransaction BeamCommon.atlasDB) (\tripTransaction -> BeamDRA.driverId driverRcAssociation B.==. BeamTripTransaction.driverId tripTransaction)
                        pure (rc, driverRcAssociation, driver, tripTransaction)
      case res of
        Right res' -> do
          let rcList = (\(rc, _, _, _) -> rc) <$> res'
          catMaybes <$> mapM fromTType' rcList
        Left _ -> pure []
    Nothing -> do
      res <-
        L.runDB dbConf $
          L.findRows $
            B.select $
              B.limit_ limitVal $
                B.offset_ offsetVal $
                  B.orderBy_ (\(rc', _, _) -> B.desc_ rc'.createdAt) $
                    B.filter_'
                      ( \(rc, driverRcAssociation, tripTransaction) ->
                          rc.merchantId B.==?. B.val_ (Just merchantId')
                            B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                            B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                            B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                            B.&&?. tripTransaction.status B.==?. B.val_ tripStatus
                            B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
                            B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                      )
                      do
                        rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                        driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                        tripTransaction <- B.join_ (BeamCommon.tripTransaction BeamCommon.atlasDB) (\tripTransaction -> BeamDRA.driverId driverRcAssociation B.==. BeamTripTransaction.driverId tripTransaction)
                        pure (rc, driverRcAssociation, tripTransaction)
      case res of
        Right res' -> do
          let rcList = (\(rc, _, _) -> rc) <$> res'
          catMaybes <$> mapM fromTType' rcList
        Left _ -> pure []

-------------------------------------------- Queries for multi fleet owner ids --------------------------------------------

findAllValidRcByFleetOwnerIdsAndSearchString :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Integer -> Integer -> Id Merchant.Merchant -> [Text] -> Maybe Text -> Maybe DbHash -> m [VehicleRegistrationCertificate]
findAllValidRcByFleetOwnerIdsAndSearchString limit offset (Id merchantId') fleetOwnerIds mbSearchString mbSearchStringHash = do
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
                        B.&&?. (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                        B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                        B.&&?. ( maybe
                                   (B.sqlBool_ $ B.val_ True)
                                   (\cNum -> B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))))
                                   mbSearchString
                                   B.||?. maybe
                                     (B.sqlBool_ $ B.val_ True)
                                     (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash)
                                     mbSearchStringHash
                               )
                  )
                  $ B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

findAllValidRcByFleetOwnerIdsAndSearchStringWithoutVerificationStatusMF :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Integer -> Integer -> Id Merchant.Merchant -> [Text] -> Maybe Text -> Maybe DbHash -> m [VehicleRegistrationCertificate]
findAllValidRcByFleetOwnerIdsAndSearchStringWithoutVerificationStatusMF limit offset (Id merchantId') fleetOwnerIds mbSearchString mbSearchStringHash = do
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
                        B.&&?. (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                        B.&&?. ( maybe
                                   (B.sqlBool_ $ B.val_ True)
                                   (\cNum -> B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))))
                                   mbSearchString
                                   B.||?. maybe
                                     (B.sqlBool_ $ B.val_ True)
                                     (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash)
                                     mbSearchStringHash
                               )
                  )
                  $ B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

findAllRCByStatusForFleetMF :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => [Text] -> Maybe Documents.VerificationStatus -> Integer -> Integer -> Id Merchant.Merchant -> Maybe Text -> m [VehicleRegistrationCertificate]
findAllRCByStatusForFleetMF fleetOwnerIds status limitVal offsetVal (Id merchantId') statusAwareVehicleNo = do
  dbConf <- getReplicaBeamConfig
  statusAwareVehicleNoHash <- mapM getDbHash statusAwareVehicleNo
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\rc' -> B.desc_ rc'.createdAt) $
                B.filter_'
                  ( \rc ->
                      rc.merchantId B.==?. B.val_ (Just merchantId')
                        B.&&?. (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\s -> rc.verificationStatus B.==?. B.val_ s) status
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> rc.certificateNumberHash B.==?. B.val_ cNum) statusAwareVehicleNoHash
                               )
                  )
                  do
                    rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                    pure rc
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

findAllVehicleByStatusForFleetByLimitOffsetMF :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => [Text] -> Id Merchant.Merchant -> Integer -> Integer -> Maybe Text -> Maybe Text -> DTT.TripStatus -> m [VehicleRegistrationCertificate]
findAllVehicleByStatusForFleetByLimitOffsetMF fleetOwnerIds (Id merchantId') limitVal offsetVal mbSearchString statusAwareVehicleNo tripStatus = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  mbSearchHash <- mapM getDbHash mbSearchString
  case mbSearchString of
    Just searchString -> do
      res <-
        L.runDB dbConf $
          L.findRows $
            B.select $
              B.limit_ limitVal $
                B.offset_ offsetVal $
                  B.orderBy_ (\(rc', _, _, _) -> B.desc_ rc'.createdAt) $
                    B.filter_'
                      ( \(rc, driverRcAssociation, driver, tripTransaction) ->
                          rc.merchantId B.==?. B.val_ (Just merchantId')
                            B.&&?. (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                            B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                            B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                            B.&&?. tripTransaction.status B.==?. B.val_ tripStatus
                            B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                            B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
                            B.&&?. ( B.sqlBool_ (B.like_ (B.lower_ driver.firstName) (B.val_ ("%" <> toLower searchString <> "%")))
                                       B.||?. B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> searchString <> "%")))
                                       B.||?. maybe (B.sqlBool_ $ B.val_ True) (\hash -> driver.mobileNumberHash B.==?. B.val_ (Just hash)) mbSearchHash
                                   )
                      )
                      do
                        rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                        driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                        driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\person -> BeamPerson.id person B.==. BeamDRA.driverId driverRcAssociation)
                        tripTransaction <- B.join_ (BeamCommon.tripTransaction BeamCommon.atlasDB) (\tripTransaction -> BeamDRA.driverId driverRcAssociation B.==. BeamTripTransaction.driverId tripTransaction)
                        pure (rc, driverRcAssociation, driver, tripTransaction)
      case res of
        Right res' -> do
          let rcList = (\(rc, _, _, _) -> rc) <$> res'
          catMaybes <$> mapM fromTType' rcList
        Left _ -> pure []
    Nothing -> do
      res <-
        L.runDB dbConf $
          L.findRows $
            B.select $
              B.limit_ limitVal $
                B.offset_ offsetVal $
                  B.orderBy_ (\(rc', _, _) -> B.desc_ rc'.createdAt) $
                    B.filter_'
                      ( \(rc, driverRcAssociation, tripTransaction) ->
                          rc.merchantId B.==?. B.val_ (Just merchantId')
                            B.&&?. (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                            B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                            B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                            B.&&?. tripTransaction.status B.==?. B.val_ tripStatus
                            B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
                            B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                      )
                      do
                        rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                        driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                        tripTransaction <- B.join_ (BeamCommon.tripTransaction BeamCommon.atlasDB) (\tripTransaction -> BeamDRA.driverId driverRcAssociation B.==. BeamTripTransaction.driverId tripTransaction)
                        pure (rc, driverRcAssociation, tripTransaction)
      case res of
        Right res' -> do
          let rcList = (\(rc, _, _) -> rc) <$> res'
          catMaybes <$> mapM fromTType' rcList
        Left _ -> pure []

findAllActiveRCForFleetMF :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> Id Merchant.Merchant -> m [VehicleRegistrationCertificate]
findAllActiveRCForFleetMF fleetOwnerIds (Id merchantId') = do
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
                    B.&&?. (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                    B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                    B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                    B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
              )
              do
                rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                pure (rc, driverRcAssociation)
  case res of
    Right res' -> do
      let rcList = (\(rc, _) -> rc) <$> res'
      catMaybes <$> mapM fromTType' rcList
    Left _ -> pure []

findAllInactiveRCForFleetMF :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => [Text] -> Integer -> Integer -> Id Merchant.Merchant -> Maybe Text -> m [VehicleRegistrationCertificate]
findAllInactiveRCForFleetMF fleetOwnerIds limitVal offsetVal merchantId statusAwareVehicleNo = do
  dbConf <- getReplicaBeamConfig
  allActiveRCs <- findAllActiveRCForFleetMF fleetOwnerIds merchantId
  let allActiveRCIds = map (.id.getId) allActiveRCs
  statusAwareVehicleNoHash <- mapM getDbHash statusAwareVehicleNo
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
                        B.&&?. (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                        B.&&?. B.sqlBool_ (B.not_ (rc.id `B.in_` (B.val_ <$> allActiveRCIds)))
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> rc.certificateNumberHash B.==?. B.val_ cNum) statusAwareVehicleNoHash
                               )
                  )
                  do
                    rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                    pure rc
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

findAllActiveRCForFleetByLimitOffsetMF :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => [Text] -> Id Merchant.Merchant -> Integer -> Integer -> Maybe Text -> Maybe Text -> m [VehicleRegistrationCertificate]
findAllActiveRCForFleetByLimitOffsetMF fleetOwnerIds (Id merchantId') limitVal offsetVal mbSearchString statusAwareVehicleNo = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  mbSearchHash <- mapM getDbHash mbSearchString
  case mbSearchString of
    Just searchString -> do
      res <-
        L.runDB dbConf $
          L.findRows $
            B.select $
              B.limit_ limitVal $
                B.offset_ offsetVal $
                  B.orderBy_ (\(rc', _, _) -> B.desc_ rc'.createdAt) $
                    B.filter_'
                      ( \(rc, driverRcAssociation, driver) ->
                          rc.merchantId B.==?. B.val_ (Just merchantId')
                            B.&&?. (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                            B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                            B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                            B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                            B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
                            B.&&?. ( B.sqlBool_ (B.like_ (B.lower_ driver.firstName) (B.val_ ("%" <> toLower searchString <> "%")))
                                       B.||?. B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> searchString <> "%")))
                                       B.||?. maybe (B.sqlBool_ $ B.val_ True) (\hash -> driver.mobileNumberHash B.==?. B.val_ (Just hash)) mbSearchHash
                                   )
                      )
                      do
                        rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                        driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                        driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\person -> BeamPerson.id person B.==. BeamDRA.driverId driverRcAssociation)
                        pure (rc, driverRcAssociation, driver)
      case res of
        Right res' -> do
          let rcList = (\(rc, _, _) -> rc) <$> res'
          catMaybes <$> mapM fromTType' rcList
        Left _ -> pure []
    Nothing -> do
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
                            B.&&?. (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                            B.&&?. rc.verificationStatus B.==?. B.val_ Documents.VALID
                            B.&&?. driverRcAssociation.isRcActive B.==?. B.val_ True
                            B.&&?. B.sqlBool_ (driverRcAssociation.associatedTill B.>=. B.val_ (Just now))
                            B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\cNum -> B.sqlBool_ $ B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))) statusAwareVehicleNo
                      )
                      do
                        rc <- B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
                        driverRcAssociation <- B.join_ (BeamCommon.driverRCAssociation BeamCommon.atlasDB) (\driverRcAssociation -> BeamVRC.id rc B.==. BeamDRA.rcId driverRcAssociation)
                        pure (rc, driverRcAssociation)
      case res of
        Right res' -> do
          let rcList = (\(rc, _) -> rc) <$> res'
          catMaybes <$> mapM fromTType' rcList
        Left _ -> pure []

partialFindLastVehicleRCFleetMF :: (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) => Text -> [Text] -> Integer -> Integer -> m [VehicleRegistrationCertificate]
partialFindLastVehicleRCFleetMF certNumber fleetOwnerIds limit offset = do
  dbConf <- getReplicaBeamConfig
  certNumberHash <- getDbHash certNumber
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limit $
            B.offset_ offset $
              B.orderBy_ (\rc' -> B.desc_ rc'.updatedAt) $
                B.filter_'
                  ( \rc ->
                      (B.sqlBool_ $ rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                        B.&&?. ( B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower certNumber <> "%")))
                                   B.||?. (rc.certificateNumberHash B.==?. B.val_ certNumberHash)
                               )
                  )
                  $ B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []

deleteByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
deleteByFleetOwnerId fleetOwnerId = deleteWithKV [Se.Is BeamVRC.fleetOwnerId (Se.Eq $ Just fleetOwnerId)]
