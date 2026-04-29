module Storage.Queries.DriverInformationExtra where

import qualified Data.Either as Either
import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import qualified Domain.Types.Common as Common
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverFlowStatus as DFS
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person as Person
import Domain.Types.Plan as P
import qualified Domain.Types.ServiceTierType as DVST
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (find, foldl, foldl', id, map, null)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SL
import qualified Sequelize as Se
import qualified SharedLogic.DriverPool.LTSDataSync as LTSSync
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Queries.DriverBlockTransactions as QDBT
import Storage.Queries.OrphanInstances.DriverInformation ()
import Storage.Queries.OrphanInstances.Person ()
import qualified Storage.Queries.Transformers.FleetOwnerInformation as Transformers
import Tools.Error

-- Extra code goes here --
findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> m (Maybe DriverInformation)
findById (Id driverInformationId) = findOneWithKV [Se.Is BeamDI.driverId $ Se.Eq driverInformationId]

getEnabledAt :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> m (Maybe UTCTime)
getEnabledAt driverId = do
  dInfo <- findById driverId
  return (dInfo >>= (.enabledAt))

findAllDriverIdExceptProvided :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Merchant -> DMOC.MerchantOperatingCity -> [Id Driver] -> m [Id Driver]
findAllDriverIdExceptProvided merchant opCity driverIdsToBeExcluded = do
  dbConf <- getReplicaBeamConfig
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.filter_'
          ( \driverInfo ->
              driverInfo.merchantId B.==?. B.val_ (Just $ getId merchant.id)
                B.&&?. (driverInfo.merchantOperatingCityId B.==?. B.val_ (Just $ getId opCity.id) B.||?. (B.sqlBool_ (B.isNothing_ driverInfo.merchantOperatingCityId) B.&&?. B.sqlBool_ (B.val_ (merchant.city == opCity.city))))
                B.&&?. driverInfo.verified B.==?. B.val_ True
                B.&&?. driverInfo.enabled B.==?. B.val_ True
                B.&&?. driverInfo.blocked B.==?. B.val_ False
                B.&&?. B.sqlBool_ (B.not_ (driverInfo.driverId `B.in_` (B.val_ . getId <$> driverIdsToBeExcluded)))
          )
          do
            B.all_ (BeamCommon.driverInformation BeamCommon.atlasDB)
  case result of
    Right res' -> do
      driverInfos <- catMaybes <$> mapM fromTType' res'
      pure $ DriverInfo.driverId <$> driverInfos
    Left _ -> pure []

findAllByEnabledAtInWindow :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe UTCTime -> Maybe UTCTime -> m [DriverInformation]
findAllByEnabledAtInWindow merchantOpCityId from to = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDI.enabledAt $ Se.GreaterThanOrEq from,
          Se.Is BeamDI.enabledAt $ Se.LessThanOrEq to,
          Se.Is BeamDI.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId)
        ]
    ]

findAllByAutoPayStatusAndMerchantIdInDriverIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Maybe DriverAutoPayStatus -> [Id Person] -> m [DriverInformation]
findAllByAutoPayStatusAndMerchantIdInDriverIds merchantId autoPayStatus driverIds = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDI.merchantId $ Se.Eq (Just merchantId.getId),
          Se.Is BeamDI.autoPayStatus $ Se.Eq autoPayStatus,
          Se.Is BeamDI.driverId $ Se.In (getId <$> driverIds)
        ]
    ]

fetchAllDriversWithPaymentPending :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [DriverInformation]
fetchAllDriversWithPaymentPending merchantOpCityId = do
  findAllWithDb
    [ Se.And
        [ Se.Is BeamDI.paymentPending $ Se.Eq True,
          Se.Is BeamDI.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId)
        ]
    ]

fetchAllBlockedDriversWithSubscribedFalse :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [DriverInformation]
fetchAllBlockedDriversWithSubscribedFalse merchantOpCityId = do
  findAllWithDb
    [ Se.And
        [ Se.Is BeamDI.subscribed $ Se.Eq False,
          Se.Is BeamDI.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId)
        ]
    ]

fetchAllAvailableByIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Person.Driver] -> m [DriverInformation]
fetchAllAvailableByIds driversIds = findAllWithKV [Se.And [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds), Se.Is BeamDI.active $ Se.Eq True, Se.Is BeamDI.onRide $ Se.Eq False]]

updateEnabledVerifiedState :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> Bool -> Maybe Bool -> m ()
updateEnabledVerifiedState (Id driverId) isEnabled isVerified = do
  now <- getCurrentTime
  enabledAt <- getEnabledAt (Id driverId)
  updateOneWithKV
    ( [ Se.Set BeamDI.enabled isEnabled,
        Se.Set BeamDI.updatedAt now
      ]
        <> ([Se.Set BeamDI.verified (fromJust isVerified) | isJust isVerified])
        <> ([Se.Set BeamDI.lastEnabledOn (Just now) | isEnabled])
        <> ([Se.Set BeamDI.enabledAt (Just now) | isEnabled && isNothing enabledAt])
    )
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateActivityWithDriverFlowStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Bool -> Maybe Common.DriverMode -> Maybe DFS.DriverFlowStatus -> Maybe Bool -> Maybe UTCTime -> Id Person.Driver -> m ())
updateActivityWithDriverFlowStatus active mode driverFlowStatus mbHasRideStarted lastOfflineTime driverId = do
  now <- getCurrentTime
  updateOneWithKV
    ( [ Se.Set BeamDI.active active,
        Se.Set BeamDI.updatedAt now
      ]
        <> [Se.Set BeamDI.mode mode | isJust mode]
        <> [Se.Set BeamDI.driverFlowStatus driverFlowStatus | isJust driverFlowStatus]
        <> [Se.Set BeamDI.hasRideStarted mbHasRideStarted | isJust mbHasRideStarted]
        <> [Se.Set BeamDI.lastOfflineTime lastOfflineTime | isJust lastOfflineTime]
    )
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.active = LTSSync.Set active,
        LTSSync.mode = LTSSync.Set mode
      }

updateDynamicBlockedStateWithActivity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Maybe Text -> Maybe Int -> Text -> Id Merchant -> Text -> Id DMOC.MerchantOperatingCity -> DTDBT.BlockedBy -> Bool -> Maybe Bool -> Maybe Common.DriverMode -> BlockReasonFlag -> m ()
updateDynamicBlockedStateWithActivity driverId blockedReason blockedExpiryTime dashboardUserName merchantId reasonCode merchantOperatingCityId blockedBy isBlocked mbActive mbMode blockReasonFlag = do
  now <- getCurrentTime
  driverInfo <- findById driverId
  let expiryTime = (\hrs -> addUTCTime (fromIntegral hrs * 3600) now) <$> blockedExpiryTime

  uid <- generateGUID
  let driverBlockDetails =
        DTDBT.DriverBlockTransactions
          { blockLiftTime = expiryTime,
            blockReason = blockedReason,
            blockTimeInHours = blockedExpiryTime,
            driverId = driverId,
            id = uid,
            reasonCode = Just reasonCode,
            reportedAt = now,
            merchantId = Just merchantId,
            createdAt = now,
            updatedAt = now,
            merchantOperatingCityId = Just merchantOperatingCityId,
            blockedBy = blockedBy,
            requestorId = Just dashboardUserName,
            actionType = Just $ if isBlocked then DTDBT.BLOCK else DTDBT.UNBLOCK,
            blockReasonFlag = Just blockReasonFlag
          }

  QDBT.create driverBlockDetails
  let numOfLocks' = case driverInfo of
        Just driverInfoResult -> driverInfoResult.numOfLocks
        Nothing -> 0
  let activeState = (.active) <$> driverInfo
  let modeState = driverInfo >>= mode
  updateOneWithKV
    ( [ Se.Set BeamDI.blocked isBlocked,
        Se.Set BeamDI.blockedReason blockedReason,
        Se.Set BeamDI.blockExpiryTime expiryTime,
        Se.Set BeamDI.blockStateModifier (Just dashboardUserName),
        Se.Set BeamDI.active $ fromMaybe True (mbActive <|> activeState),
        Se.Set BeamDI.mode $ mbMode <|> modeState,
        Se.Set BeamDI.blockReasonFlag (Just blockReasonFlag),
        Se.Set BeamDI.updatedAt now
      ]
        <> ([Se.Set BeamDI.numOfLocks (numOfLocks' + 1) | isBlocked])
    )
    [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.blocked = LTSSync.Set isBlocked,
        LTSSync.active = LTSSync.Set $ fromMaybe True (mbActive <|> activeState),
        LTSSync.mode = LTSSync.Set $ mbMode <|> modeState
      }

updateBlockedState :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Bool -> Maybe Text -> Id Merchant -> Id DMOC.MerchantOperatingCity -> DTDBT.BlockedBy -> m ()
updateBlockedState driverId isBlocked blockStateModifier merchantId merchantOperatingCityId blockedBy = do
  now <- getCurrentTime
  driverInfo <- findById driverId
  uid <- generateGUID

  let driverBlockDetails =
        DTDBT.DriverBlockTransactions
          { blockLiftTime = Nothing,
            blockReason = Nothing,
            blockTimeInHours = Nothing,
            driverId = driverId,
            id = uid,
            reasonCode = Nothing,
            blockReasonFlag = Nothing,
            reportedAt = now,
            merchantId = Just merchantId,
            createdAt = now,
            updatedAt = now,
            merchantOperatingCityId = Just merchantOperatingCityId,
            blockedBy = blockedBy,
            actionType = Just $ if isBlocked then DTDBT.BLOCK else DTDBT.UNBLOCK,
            requestorId = Nothing
          }

  QDBT.create driverBlockDetails
  let numOfLocks' = case driverInfo of
        Just driverInfoResult -> driverInfoResult.numOfLocks
        Nothing -> 0
  updateOneWithKV
    ( [ Se.Set BeamDI.blocked isBlocked,
        Se.Set BeamDI.blockStateModifier blockStateModifier,
        Se.Set BeamDI.blockExpiryTime Nothing, --this prevents wrong date to be in this column because same is passed in error to frontend
        Se.Set BeamDI.updatedAt now
      ]
        <> ([Se.Set BeamDI.numOfLocks (numOfLocks' + 1) | isBlocked])
    )
    [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.blocked = LTSSync.Set isBlocked}

findByDriverIdActiveRide :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> m (Maybe DriverInformation)
findByDriverIdActiveRide (Id driverId) = findOneWithKV [Se.And [Se.Is BeamDI.driverId $ Se.Eq driverId, Se.Is BeamDI.onRide $ Se.Eq True]]

updateNotOnRideMultiple :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Person.Driver] -> m ()
updateNotOnRideMultiple driverIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDI.onRide False,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.In (getId <$> driverIds))]
  Kernel.Prelude.mapM_ (\did -> LTSSync.syncDriverPoolDataToLTS (cast did) $ LTSSync.emptyUpdate {LTSSync.onRide = LTSSync.Set False}) driverIds

deleteById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamDI.driverId (Se.Eq driverId)]

findAllWithLimitOffsetByMerchantId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Id Merchant ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByMerchantId mbSearchString mbSearchStrDBHash mbLimit mbOffset merchantId = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromMaybe 100 mbLimit) $
          B.offset_ (fromMaybe 0 mbOffset) $
            B.orderBy_ (\(_person, driverInfo) -> B.desc_ (driverInfo.createdAt)) $
              B.filter_'
                ( \(person, _driverInfo) ->
                    person.role B.==?. B.val_ Person.DRIVER
                      B.&&?. person.merchantId B.==?. B.val_ (getId merchantId)
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchString -> B.sqlBool_ (B.concat_ [person.firstName, B.val_ "", B.fromMaybe_ (B.val_ "") person.middleName, B.val_ "", B.fromMaybe_ (B.val_ "") person.lastName] `B.like_` B.val_ ("%" <> searchString <> "%"))) mbSearchString
                      B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStrDBHash -> person.mobileNumberHash B.==?. B.val_ (Just searchStrDBHash)) mbSearchStrDBHash
                )
                do
                  person' <- B.all_ (SBC.person SBC.atlasDB)
                  driverInfo' <- B.join_' (SBC.driverInformation SBC.atlasDB) (\driverInfo'' -> BeamDI.driverId driverInfo'' B.==?. BeamP.id person')
                  pure (person', driverInfo')
  case res of
    Right res' -> do
      let p' = fst <$> res'
          di' = snd <$> res'
      p <- catMaybes <$> mapM fromTType' p'
      di <- catMaybes <$> mapM fromTType' di'
      pure $ zip p di
    Left _ -> pure []

getDrivers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [DriverInformation] ->
  m [Person]
getDrivers driverInfos = findAllWithKV [Se.Is BeamP.id $ Se.In personKeys]
  where
    personKeys = getId <$> fetchDriverIDsFromInfo driverInfos

getDriverInfos ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInfos driverLocations = findAllWithKV [Se.And [Se.Is BeamDI.driverId $ Se.In personsKeys, Se.Is BeamDI.active $ Se.Eq True]]
  where
    personsKeys = getId <$> fetchDriverIDsFromLocations driverLocations

fetchDriverIDsFromLocations :: [DriverLocation] -> [Id Person]
fetchDriverIDsFromLocations = map DriverLocation.driverId

fetchDriverIDsFromInfo :: [DriverInformation] -> [Id Person]
fetchDriverIDsFromInfo = map DriverInfo.driverId

countDrivers :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m (Int, Int)
countDrivers merchantID =
  getResults <$> do
    dbConf <- getReplicaBeamConfig
    res <- L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_ (\(driverInformation, _) -> (B.group_ (BeamDI.active driverInformation), B.as_ @Int B.countAll_)) $
            B.filter_' (\(_, BeamP.PersonT {..}) -> merchantId B.==?. B.val_ (getId merchantID)) $
              do
                driverInformation <- B.all_ (BeamCommon.driverInformation BeamCommon.atlasDB)
                person <- B.join_' (BeamCommon.person BeamCommon.atlasDB) (\person -> BeamP.id person B.==?. BeamDI.driverId driverInformation)
                pure (driverInformation, person)
    pure (Either.fromRight [] res)
  where
    getResults :: [(Bool, Int)] -> (Int, Int)
    getResults = foldl func (0, 0)

    func (active, inactive) (activity, counter) =
      if activity then (active + counter, inactive) else (active, inactive + counter)

updatPayerVpa :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Id Person.Driver -> m ()
updatPayerVpa payerVpa (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    ( [ Se.Set BeamDI.updatedAt now
      ]
        <> [Se.Set BeamDI.payerVpa payerVpa | isJust payerVpa]
    )
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateHasAdvancedRide :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Bool -> m ()
updateHasAdvancedRide (Id driverId) isOnAdvancedRide = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.hasAdvanceBooking (Just isOnAdvancedRide),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]
  LTSSync.syncDriverPoolDataToLTS (Id driverId) $
    LTSSync.emptyUpdate {LTSSync.hasAdvanceBooking = LTSSync.Set (Just isOnAdvancedRide)}

updatePayoutVpaAndStatusByDriverIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Maybe PayoutVpaStatus -> [Id Person.Driver] -> m ()
updatePayoutVpaAndStatusByDriverIds payoutVpa payoutVpaStatus driverIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDI.payoutVpaStatus payoutVpaStatus,
      Se.Set BeamDI.payoutVpa payoutVpa,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.In (getId <$> driverIds))]

updateTripCategoryAndTripEndLocationByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Maybe Common.TripCategory -> Maybe Maps.LatLong -> m ()
updateTripCategoryAndTripEndLocationByDriverId driverId tripCategory tripEndLocation = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set BeamDI.onRideTripCategory tripCategory | isJust tripCategory]
        <> [ Se.Set BeamDI.driverTripEndLocationLat (fmap (.lat) tripEndLocation),
             Se.Set BeamDI.driverTripEndLocationLon (fmap (.lon) tripEndLocation),
             Se.Set BeamDI.updatedAt now
           ]
    )
    [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.onRideTripCategory = LTSSync.Set (show <$> tripCategory)}

updateOnRideAndTripEndLocationByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Bool -> Maybe Maps.LatLong -> m ()
updateOnRideAndTripEndLocationByDriverId driverId onRide tripEndLocation = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set BeamDI.onRide onRide]
        <> [ Se.Set BeamDI.driverTripEndLocationLat (fmap (.lat) tripEndLocation),
             Se.Set BeamDI.driverTripEndLocationLon (fmap (.lon) tripEndLocation),
             Se.Set BeamDI.updatedAt now
           ]
    )
    [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.onRide = LTSSync.Set onRide}

updateHasRideStarted :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Bool -> m ()
updateHasRideStarted driverId hasRideStarted = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.hasRideStarted (Just hasRideStarted),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.hasRideStarted = LTSSync.Set (Just hasRideStarted)}

updateIsBlockedForReferralPayout :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Person.Driver] -> Bool -> m ()
updateIsBlockedForReferralPayout driverIds isBlocked = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDI.isBlockedForReferralPayout (Just isBlocked),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.In (getId <$> driverIds))]

updateForwardBatchingEnabledOrIsInteroperable :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Maybe Bool -> Maybe Bool -> m ()
updateForwardBatchingEnabledOrIsInteroperable driverId isAdvancedBookingEnabled isInteroperable = do
  if isNothing isAdvancedBookingEnabled && isNothing isInteroperable
    then pure ()
    else do
      now <- getCurrentTime
      updateOneWithKV
        ( [Se.Set BeamDI.updatedAt now]
            <> [Se.Set BeamDI.forwardBatchingEnabled isAdvancedBookingEnabled | isJust isAdvancedBookingEnabled]
            <> [Se.Set BeamDI.isInteroperable isInteroperable | isJust isInteroperable]
        )
        [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]
      Kernel.Prelude.whenJust isAdvancedBookingEnabled $ \fbe ->
        LTSSync.syncDriverPoolDataToLTS (cast driverId) $
          LTSSync.emptyUpdate {LTSSync.forwardBatchingEnabled = LTSSync.Set fbe}

updateServicesEnabled :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> [P.ServiceNames] -> m ()
updateServicesEnabled driverIds services = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDI.servicesEnabledForSubscription (Just services),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.In driverIds)]

findByIdAndVerified ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person.Driver ->
  Maybe Bool ->
  m (Maybe DriverInformation)
findByIdAndVerified (Id driverInformationId) mbVerified =
  findOneWithKV
    [ Se.And $
        [Se.Is BeamDI.driverId $ Se.Eq driverInformationId]
          <> [Se.Is BeamDI.verified $ Se.Eq (fromJust mbVerified) | isJust mbVerified]
    ]

updateAadhaarNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe (EncryptedHashed Text) -> Id Person.Driver -> m ()
updateAadhaarNumber aadhaarNumber driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.aadhaarNumberEncrypted (Transformers.mkFieldEncrypted aadhaarNumber),
      Se.Set BeamDI.aadhaarNumberHash (Transformers.mkFieldHash aadhaarNumber),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]

updatePanNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe (EncryptedHashed Text) -> Id Person.Driver -> m ()
updatePanNumber panNumber driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.panNumberEncrypted (Transformers.mkFieldEncrypted panNumber),
      Se.Set BeamDI.panNumberHash (Transformers.mkFieldHash panNumber),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]

updateApproved :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Bool -> Id Person.Driver -> m ()
updateApproved approved driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.approved approved,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]

updateDlNumber :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe (EncryptedHashed Text) -> Id Person.Driver -> m ()
updateDlNumber dlNumber driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.dlNumberEncrypted (Transformers.mkFieldEncrypted dlNumber),
      Se.Set BeamDI.dlNumberHash (Transformers.mkFieldHash dlNumber),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]

updateOnlineDurationRefreshedAt :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> UTCTime -> m ()
updateOnlineDurationRefreshedAt (Id driverId) onlineDurationRefreshedAt = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.onlineDurationRefreshedAt $ Just onlineDurationRefreshedAt,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

findAllByDriverIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> m [DriverInformation]
findAllByDriverIds driverIds = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            (\driverInfo -> B.sqlBool_ $ driverInfo.driverId `B.in_` (B.val_ <$> driverIds))
            $ B.all_ (SBC.driverInformation SBC.atlasDB)
  case res of
    Right driverInfoList -> catMaybes <$> mapM fromTType' driverInfoList
    Left _ -> pure []

countEnabledByDriverIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> m Int
countEnabledByDriverIds driverIds =
  if null driverIds
    then pure 0
    else do
      dbConf <- getReplicaBeamConfig
      res <-
        L.runDB dbConf $
          L.findRows $
            B.select $
              B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
                B.filter_'
                  ( \driverInfo ->
                      B.sqlBool_ (driverInfo.driverId `B.in_` (B.val_ <$> driverIds))
                        B.&&?. driverInfo.enabled B.==?. B.val_ True
                  )
                  $ B.all_ (SBC.driverInformation SBC.atlasDB)
      pure $ either (const 0) (\r -> if null r then 0 else head r) res

updateLastOfflineTime :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Person -> UTCTime -> m ()
updateLastOfflineTime driverId offlineTime = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.lastOfflineTime (Just offlineTime),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]

updateDob :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Maybe UTCTime -> m ()
updateDob (Id driverId) driverDob = do
  now <- getCurrentTime
  updateOneWithKV
    ( [ Se.Set BeamDI.updatedAt now
      ]
        <> [Se.Set BeamDI.driverDob driverDob | isJust driverDob]
    )
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateMerchantIdAndCityIdByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Person -> Id Merchant -> Id DMOC.MerchantOperatingCity -> m ()
updateMerchantIdAndCityIdByDriverId driverId merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.merchantId (Just $ getId merchantId),
      Se.Set BeamDI.merchantOperatingCityId (Just $ getId merchantOperatingCityId),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq $ getId driverId)]

findEligibleForScheduledPayout :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Int -> Maybe (Id Person.Driver) -> m [DriverInformation]
findEligibleForScheduledPayout merchantOpCityId batchSize mbLastDriverId = do
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamDI.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId),
          Se.Is BeamDI.enabled $ Se.Eq True,
          Se.Is BeamDI.blocked $ Se.Eq False,
          Se.Is BeamDI.payoutVpa $ Se.Not (Se.Eq Nothing),
          Se.Is BeamDI.isBlockedForScheduledPayout $ Se.Not (Se.Eq (Just True))
        ]
          <> maybe [] (\lastId -> [Se.Is BeamDI.driverId $ Se.GreaterThan (getId lastId)]) mbLastDriverId
    ]
    (Se.Asc BeamDI.driverId)
    (Just batchSize)
    Nothing

-- | Fetch drivers who are currently on a ride with ride started, belonging to any of the given cities.
-- Uses limit/offset for pagination.
findOnRideDriversWithRideStartedByMerchantOpCityIds ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id DMOC.MerchantOperatingCity] ->
  Int ->
  Int ->
  m [DriverInformation]
findOnRideDriversWithRideStartedByMerchantOpCityIds merchantOpCityIds limitVal offsetVal =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDI.onRide $ Se.Eq True,
          Se.Is BeamDI.hasRideStarted $ Se.Eq (Just True),
          Se.Is BeamDI.merchantOperatingCityId $ Se.In (Just . getId <$> merchantOpCityIds)
        ]
    ]
    (Se.Asc BeamDI.driverId)
    (Just limitVal)
    (Just offsetVal)

-- Wrappers for src-read-only functions with LTS sync

updateOnRide :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Bool -> Id Person.Person -> m ()
updateOnRide onRide driverId = do
  now <- getCurrentTime
  LTSSync.runPoolFieldUpdate (cast driverId) $
    LTSSync.mkPoolFieldUpdate
      (updateOneWithKV [Se.Set BeamDI.onRide onRide, Se.Set BeamDI.updatedAt now] [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)])
      (LTSSync.emptyUpdate {LTSSync.onRide = LTSSync.Set onRide})

updateOnRideAndLatestScheduledBookingAndPickup ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Bool ->
  Maybe UTCTime ->
  Maybe Maps.LatLong ->
  Id Person.Person ->
  m ()
updateOnRideAndLatestScheduledBookingAndPickup onRide latestScheduledBooking latestScheduledPickup driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.onRide onRide,
      Se.Set BeamDI.latestScheduledBooking latestScheduledBooking,
      Se.Set BeamDI.latestScheduledPickupLat (fmap (.lat) latestScheduledPickup),
      Se.Set BeamDI.latestScheduledPickupLon (fmap (.lon) latestScheduledPickup),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.onRide = LTSSync.Set onRide,
        LTSSync.latestScheduledBooking = LTSSync.Set latestScheduledBooking,
        LTSSync.latestScheduledPickup = LTSSync.Set latestScheduledPickup
      }

-- Class 1 wrappers for src-read-only functions with LTS sync

updateDriverInformation ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Maybe Text ->
  Bool ->
  Maybe Meters ->
  Maybe Meters ->
  Maybe Meters ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DriverInfo.OnboardingAs ->
  Maybe Text ->
  Maybe DriverInfo.AddressDocumentType ->
  Maybe Text ->
  Maybe Text ->
  Id Person.Person ->
  m ()
updateDriverInformation canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi canSwitchToRental canSwitchToInterCity canSwitchToIntraCity availableUpiApps isPetModeEnabled tripDistanceMaxThreshold tripDistanceMinThreshold maxPickupRadius isSilentModeEnabled rideRequestVolume isTTSEnabled isHighAccuracyLocationEnabled rideRequestVolumeEnabled onboardingAs address addressDocumentType nomineeName nomineeRelationship driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.canDowngradeToSedan canDowngradeToSedan,
      Se.Set BeamDI.canDowngradeToHatchback canDowngradeToHatchback,
      Se.Set BeamDI.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set BeamDI.canSwitchToRental (Just canSwitchToRental),
      Se.Set BeamDI.canSwitchToInterCity (Just canSwitchToInterCity),
      Se.Set BeamDI.canSwitchToIntraCity (Just canSwitchToIntraCity),
      Se.Set BeamDI.availableUpiApps availableUpiApps,
      Se.Set BeamDI.isPetModeEnabled (Just isPetModeEnabled),
      Se.Set BeamDI.tripDistanceMaxThreshold tripDistanceMaxThreshold,
      Se.Set BeamDI.tripDistanceMinThreshold tripDistanceMinThreshold,
      Se.Set BeamDI.maxPickupRadius maxPickupRadius,
      Se.Set BeamDI.isSilentModeEnabled isSilentModeEnabled,
      Se.Set BeamDI.rideRequestVolume rideRequestVolume,
      Se.Set BeamDI.isTTSEnabled isTTSEnabled,
      Se.Set BeamDI.isHighAccuracyLocationEnabled isHighAccuracyLocationEnabled,
      Se.Set BeamDI.rideRequestVolumeEnabled rideRequestVolumeEnabled,
      Se.Set BeamDI.onboardingAs onboardingAs,
      Se.Set BeamDI.address address,
      Se.Set BeamDI.addressDocumentType addressDocumentType,
      Se.Set BeamDI.nomineeName nomineeName,
      Se.Set BeamDI.nomineeRelationship nomineeRelationship,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.canSwitchToRental = LTSSync.Set canSwitchToRental,
        LTSSync.canSwitchToInterCity = LTSSync.Set canSwitchToInterCity,
        LTSSync.canSwitchToIntraCity = LTSSync.Set canSwitchToIntraCity,
        LTSSync.isPetModeEnabled = LTSSync.Set isPetModeEnabled,
        LTSSync.tripDistanceMaxThreshold = LTSSync.Set tripDistanceMaxThreshold,
        LTSSync.tripDistanceMinThreshold = LTSSync.Set tripDistanceMinThreshold,
        LTSSync.maxPickupRadius = LTSSync.Set maxPickupRadius
      }

updateSubscription :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Bool -> Id Person.Person -> m ()
updateSubscription subscribed driverId = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamDI.subscribed subscribed, Se.Set BeamDI.updatedAt now] [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.subscribed = LTSSync.Set subscribed}

updateSoftBlock :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe [DVST.ServiceTierType] -> Maybe UTCTime -> Maybe Text -> Id Person.Person -> m ()
updateSoftBlock softBlockStiers softBlockExpiryTime softBlockReasonFlag driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.softBlockStiers softBlockStiers,
      Se.Set BeamDI.softBlockExpiryTime softBlockExpiryTime,
      Se.Set BeamDI.softBlockReasonFlag softBlockReasonFlag,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.softBlockStiers = LTSSync.Set softBlockStiers}

updateSpecialLocWarriorInfo :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Bool -> Maybe (Id SL.SpecialLocation) -> [Id SL.SpecialLocation] -> Maybe UTCTime -> Id Person.Person -> m ()
updateSpecialLocWarriorInfo isSpecialLocWarrior preferredPrimarySpecialLocId preferredSecondarySpecialLocIds specialLocWarriorEnabledAt driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.isSpecialLocWarrior (Just isSpecialLocWarrior),
      Se.Set BeamDI.preferredPrimarySpecialLocId (getId <$> preferredPrimarySpecialLocId),
      Se.Set BeamDI.preferredSecondarySpecialLocIds (Just (map getId preferredSecondarySpecialLocIds)),
      Se.Set BeamDI.specialLocWarriorEnabledAt specialLocWarriorEnabledAt,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.isSpecialLocWarrior = LTSSync.Set isSpecialLocWarrior}

updateTollRouteBlockedTill :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe UTCTime -> Id Person.Person -> m ()
updateTollRouteBlockedTill tollRouteBlockedTill driverId = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamDI.tollRouteBlockedTill tollRouteBlockedTill, Se.Set BeamDI.updatedAt now] [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.tollRouteBlockedTill = LTSSync.Set tollRouteBlockedTill}

removeAcUsageRestriction :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Double -> DriverInfo.AirConditionedRestrictionType -> Int -> Id Person.Person -> m ()
removeAcUsageRestriction airConditionScore acUsageRestrictionType acRestrictionLiftCount driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.airConditionScore airConditionScore,
      Se.Set BeamDI.acUsageRestrictionType (Just acUsageRestrictionType),
      Se.Set BeamDI.acRestrictionLiftCount acRestrictionLiftCount,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.acUsageRestrictionType = LTSSync.Set (Just $ show acUsageRestrictionType),
        LTSSync.acRestrictionLiftCount = LTSSync.Set acRestrictionLiftCount
      }

updateAcUsageRestrictionAndScore :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DriverInfo.AirConditionedRestrictionType -> Maybe Double -> Id Person.Person -> m ()
updateAcUsageRestrictionAndScore acUsageRestrictionType airConditionScore driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.acUsageRestrictionType (Just acUsageRestrictionType),
      Se.Set BeamDI.airConditionScore airConditionScore,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.acUsageRestrictionType = LTSSync.Set (Just $ show acUsageRestrictionType)}

updateRentalInterCityAndIntraCitySwitch :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Bool -> Bool -> Bool -> Id Person.Person -> m ()
updateRentalInterCityAndIntraCitySwitch canSwitchToRental canSwitchToInterCity canSwitchToIntraCity driverId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.canSwitchToRental (Just canSwitchToRental),
      Se.Set BeamDI.canSwitchToInterCity (Just canSwitchToInterCity),
      Se.Set BeamDI.canSwitchToIntraCity (Just canSwitchToIntraCity),
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.canSwitchToRental = LTSSync.Set canSwitchToRental,
        LTSSync.canSwitchToInterCity = LTSSync.Set canSwitchToInterCity,
        LTSSync.canSwitchToIntraCity = LTSSync.Set canSwitchToIntraCity
      }

updateForwardBatchingEnabled :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Bool -> Id Person.Person -> m ()
updateForwardBatchingEnabled forwardBatchingEnabled driverId = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamDI.forwardBatchingEnabled (Just forwardBatchingEnabled), Se.Set BeamDI.updatedAt now] [Se.Is BeamDI.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.forwardBatchingEnabled = LTSSync.Set forwardBatchingEnabled}
