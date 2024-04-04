{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverInformationExtra where

import qualified Data.Either as Either
import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person as Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.DriverInformation
import Storage.Queries.PersonExtra (findAllPersonWithDriverInfos)

-- Extra code goes here --
findById :: KvDbFlow m r => Id Person.Driver -> m (Maybe DriverInformation)
findById (Id driverInformationId) = findOneWithKV [Se.Is BeamDI.driverId $ Se.Eq driverInformationId]

getEnabledAt :: KvDbFlow m r => Id Person.Driver -> m (Maybe UTCTime)
getEnabledAt driverId = do
  dInfo <- findById driverId
  return (dInfo >>= (.enabledAt))

findAllDriverIdExceptProvided :: KvDbFlow m r => Merchant -> DMOC.MerchantOperatingCity -> [Id Driver] -> m [Id Driver]
findAllDriverIdExceptProvided merchant opCity driverIdsToBeExcluded = do
  dbConf <- getMasterBeamConfig
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

findAllByEnabledAtInWindow :: KvDbFlow m r => Id DMOC.MerchantOperatingCity -> Maybe UTCTime -> Maybe UTCTime -> m [DriverInformation]
findAllByEnabledAtInWindow merchantOpCityId from to = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDI.enabledAt $ Se.GreaterThanOrEq from,
          Se.Is BeamDI.enabledAt $ Se.LessThanOrEq to,
          Se.Is BeamDI.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId)
        ]
    ]

findAllByAutoPayStatusAndMerchantIdInDriverIds :: KvDbFlow m r => Id Merchant -> Maybe DriverAutoPayStatus -> [Id Person] -> m [DriverInformation]
findAllByAutoPayStatusAndMerchantIdInDriverIds merchantId autoPayStatus driverIds = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDI.merchantId $ Se.Eq (Just merchantId.getId),
          Se.Is BeamDI.autoPayStatus $ Se.Eq autoPayStatus,
          Se.Is BeamDI.driverId $ Se.In (getId <$> driverIds)
        ]
    ]

fetchAllByIds :: KvDbFlow m r => Id Merchant -> [Id Driver] -> m [DriverInformation]
fetchAllByIds merchantId driversIds = do
  dInfos <- findAllWithKV [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds)]
  persons <- findAllPersonWithDriverInfos dInfos merchantId
  pure $ foldl' (getDriverInfoWithPerson persons) [] dInfos
  where
    getDriverInfoWithPerson persons acc dInfo' =
      case find (\person -> person.id == dInfo'.driverId) persons of
        Just _person -> dInfo' : acc
        Nothing -> acc

fetchAllDriversWithPaymentPending :: KvDbFlow m r => Id DMOC.MerchantOperatingCity -> m [DriverInformation]
fetchAllDriversWithPaymentPending merchantOpCityId = do
  findAllWithDb
    [ Se.And
        [ Se.Is BeamDI.paymentPending $ Se.Eq True,
          Se.Is BeamDI.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId)
        ]
    ]

fetchAllBlockedDriversWithSubscribedFalse :: KvDbFlow m r => Id DMOC.MerchantOperatingCity -> m [DriverInformation]
fetchAllBlockedDriversWithSubscribedFalse merchantOpCityId = do
  findAllWithDb
    [ Se.And
        [ Se.Is BeamDI.subscribed $ Se.Eq False,
          Se.Is BeamDI.merchantOperatingCityId $ Se.Eq (Just merchantOpCityId.getId)
        ]
    ]

fetchAllAvailableByIds :: KvDbFlow m r => [Id Person.Driver] -> m [DriverInformation]
fetchAllAvailableByIds driversIds = findAllWithKV [Se.And [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds), Se.Is BeamDI.active $ Se.Eq True, Se.Is BeamDI.onRide $ Se.Eq False]]

updateEnabledVerifiedState :: KvDbFlow m r => Id Driver -> Bool -> Maybe Bool -> m ()
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

updateDynamicBlockedState :: KvDbFlow m r => Id Person.Driver -> Maybe Text -> Maybe Int -> Text -> Bool -> m ()
updateDynamicBlockedState driverId blockedReason blockedExpiryTime dashboardUserName isBlocked = do
  now <- getCurrentTime
  driverInfo <- findById driverId
  let expiryTime = (\secs -> addUTCTime (fromIntegral secs * 3600) now) <$> blockedExpiryTime
  let numOfLocks' = case driverInfo of
        Just driverInfoResult -> driverInfoResult.numOfLocks
        Nothing -> 0
  updateOneWithKV
    ( [ Se.Set BeamDI.blocked isBlocked,
        Se.Set BeamDI.blockedReason blockedReason,
        Se.Set BeamDI.blockExpiryTime expiryTime,
        Se.Set BeamDI.blockStateModifier (Just dashboardUserName),
        Se.Set BeamDI.updatedAt now
      ]
        <> ([Se.Set BeamDI.numOfLocks (numOfLocks' + 1) | isBlocked])
    )
    [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]

updateBlockedState :: KvDbFlow m r => Id Person.Driver -> Bool -> Maybe Text -> m ()
updateBlockedState driverId isBlocked blockStateModifier = do
  now <- getCurrentTime
  driverInfo <- findById driverId
  let numOfLocks' = case driverInfo of
        Just driverInfoResult -> driverInfoResult.numOfLocks
        Nothing -> 0
  updateOneWithKV
    ( [ Se.Set BeamDI.blocked isBlocked,
        Se.Set BeamDI.blockStateModifier blockStateModifier,
        Se.Set BeamDI.updatedAt now
      ]
        <> ([Se.Set BeamDI.numOfLocks (numOfLocks' + 1) | isBlocked])
    )
    [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]

findByDriverIdActiveRide :: KvDbFlow m r => Id Person.Driver -> m (Maybe DriverInformation)
findByDriverIdActiveRide (Id driverId) = findOneWithKV [Se.And [Se.Is BeamDI.driverId $ Se.Eq driverId, Se.Is BeamDI.onRide $ Se.Eq True]]

updateNotOnRideMultiple :: KvDbFlow m r => [Id Person.Driver] -> m ()
updateNotOnRideMultiple driverIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDI.onRide False,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.In (getId <$> driverIds))]

deleteById :: KvDbFlow m r => Id Person.Driver -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamDI.driverId (Se.Eq driverId)]

findAllWithLimitOffsetByMerchantId ::
  KvDbFlow m r =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Id Merchant ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByMerchantId mbSearchString mbSearchStrDBHash mbLimit mbOffset merchantId = do
  dbConf <- getMasterBeamConfig
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
  KvDbFlow m r =>
  [DriverInformation] ->
  m [Person]
getDrivers driverInfos = findAllWithKV [Se.Is BeamP.id $ Se.In personKeys]
  where
    personKeys = getId <$> fetchDriverIDsFromInfo driverInfos

getDriverInfos ::
  KvDbFlow m r =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInfos driverLocations = findAllWithKV [Se.And [Se.Is BeamDI.driverId $ Se.In personsKeys, Se.Is BeamDI.active $ Se.Eq True]]
  where
    personsKeys = getId <$> fetchDriverIDsFromLocations driverLocations

fetchDriverIDsFromLocations :: [DriverLocation] -> [Id Person]
fetchDriverIDsFromLocations = map DriverLocation.driverId

fetchDriverIDsFromInfo :: [DriverInformation] -> [Id Person]
fetchDriverIDsFromInfo = map DriverInfo.driverId

countDrivers :: KvDbFlow m r => Id Merchant -> m (Int, Int)
countDrivers merchantID =
  getResults <$> do
    dbConf <- getMasterBeamConfig
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

updatPayerVpa :: KvDbFlow m r => Maybe Text -> Id Person.Driver -> m ()
updatPayerVpa payerVpa (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    ( [ Se.Set BeamDI.updatedAt now
      ]
        <> [Se.Set BeamDI.payerVpa payerVpa | isJust payerVpa]
    )
    [Se.Is BeamDI.driverId (Se.Eq driverId)]
