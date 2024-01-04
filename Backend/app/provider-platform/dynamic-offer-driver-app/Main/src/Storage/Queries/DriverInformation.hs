{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverInformation where

import qualified Data.Either as Either
import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.Instances.DriverInformation ()
import Storage.Queries.Person (findAllPersonWithDriverInfos)

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverInfo.DriverInformation -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> m (Maybe DriverInformation)
findById (Id driverInformationId) = findOneWithKV [Se.Is BeamDI.driverId $ Se.Eq driverInformationId]

getEnabledAt :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> m (Maybe UTCTime)
getEnabledAt driverId = do
  dInfo <- findById driverId
  return (dInfo >>= (.enabledAt))

findAllByEnabledAtInWindow :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Maybe UTCTime -> Maybe UTCTime -> m [DriverInformation]
findAllByEnabledAtInWindow merchantId from to = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamDI.enabledAt $ Se.GreaterThanOrEq from,
          Se.Is BeamDI.enabledAt $ Se.LessThanOrEq to,
          Se.Is BeamDI.merchantId $ Se.Eq (Just merchantId.getId)
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

fetchAllByIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> [Id Driver] -> m [DriverInformation]
fetchAllByIds merchantId driversIds = do
  dInfos <- findAllWithKV [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds)]
  persons <- findAllPersonWithDriverInfos dInfos merchantId
  pure $ foldl' (getDriverInfoWithPerson persons) [] dInfos
  where
    getDriverInfoWithPerson persons acc dInfo' =
      case find (\person -> person.id == dInfo'.driverId) persons of
        Just _person -> dInfo' : acc
        Nothing -> acc

fetchAllDriversWithPaymentPending :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m [DriverInformation]
fetchAllDriversWithPaymentPending merchantId = do
  findAllWithDb
    [ Se.And
        [ Se.Is BeamDI.paymentPending $ Se.Eq True,
          Se.Is BeamDI.merchantId $ Se.Eq (Just merchantId.getId)
        ]
    ]

fetchAllBlockedDriversWithSubscribedFalse :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m [DriverInformation]
fetchAllBlockedDriversWithSubscribedFalse merchantId = do
  findAllWithDb
    [ Se.And
        [ Se.Is BeamDI.subscribed $ Se.Eq False,
          Se.Is BeamDI.merchantId $ Se.Eq (Just merchantId.getId)
        ]
    ]

fetchAllAvailableByIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Person.Driver] -> m [DriverInformation]
fetchAllAvailableByIds driversIds = findAllWithKV [Se.And [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds), Se.Is BeamDI.active $ Se.Eq True, Se.Is BeamDI.onRide $ Se.Eq False]]

updateActivity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Bool -> Maybe DriverMode -> m ()
updateActivity (Id driverId) isActive mode = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.active isActive,
      Se.Set BeamDI.mode mode,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

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

updateDynamicBlockedState :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Maybe Text -> Maybe Int -> Text -> Bool -> m ()
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

updateBlockedState :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Bool -> Maybe Text -> m ()
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

updateOnRide :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Bool -> m ()
updateOnRide (Id driverId) onRide = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.onRide onRide,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

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

addReferralCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Text -> Id Person -> m ()
addReferralCode (Id personId) code referredByDriverId = do
  updateWithKV
    [ Se.Set BeamDI.referralCode (Just code),
      Se.Set BeamDI.referredByDriverId (Just referredByDriverId.getId)
    ]
    [Se.Is BeamDI.driverId (Se.Eq personId)]

incrementReferralCountByPersonId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Int -> m ()
incrementReferralCountByPersonId (Id driverId) value = do
  updateOneWithKV
    [ Se.Set BeamDI.totalReferred (Just value)
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

countDrivers :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> m (Int, Int)
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

updateDriverInformation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Bool -> Bool -> Bool -> Maybe Text -> m ()
updateDriverInformation (Id driverId) canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi availableUpiApps = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.canDowngradeToSedan canDowngradeToSedan,
      Se.Set BeamDI.canDowngradeToHatchback canDowngradeToHatchback,
      Se.Set BeamDI.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set BeamDI.availableUpiApps availableUpiApps,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateDriverDowngradeTaxiForSuv :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Bool -> m ()
updateDriverDowngradeTaxiForSuv (Id driverId) canDowngradeToTaxi = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateAutoPayStatusAndPayerVpa :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe DriverAutoPayStatus -> Maybe Text -> Id Person.Driver -> m ()
updateAutoPayStatusAndPayerVpa autoPayStatus payerVpa (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    ( [ Se.Set BeamDI.autoPayStatus autoPayStatus,
        Se.Set BeamDI.updatedAt now
      ]
        <> [Se.Set BeamDI.payerVpa payerVpa | isJust payerVpa]
    )
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateSubscription :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Bool -> Id Person.Driver -> m ()
updateSubscription isSubscribed (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.subscribed isSubscribed,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateAadhaarVerifiedState :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Bool -> m ()
updateAadhaarVerifiedState (Id personId) isVerified = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.aadhaarVerified isVerified,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq personId)]

updatePendingPayment :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Bool -> Id Person.Driver -> m ()
updatePendingPayment isPending (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.paymentPending isPending,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateCompAadhaarImagePath :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Text -> m ()
updateCompAadhaarImagePath (Id driverId) compAadhaarImagePath =
  updateOneWithKV
    [ Se.Set BeamDI.compAadhaarImagePath (Just compAadhaarImagePath)
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateDriverDob :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person.Driver -> Maybe UTCTime -> m ()
updateDriverDob (Id driverId) driverDob = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.driverDob driverDob,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]
