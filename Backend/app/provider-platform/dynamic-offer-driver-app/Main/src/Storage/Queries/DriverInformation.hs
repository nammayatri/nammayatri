{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromRight" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Storage.Queries.DriverInformation where

-- import Control.Applicative (liftA2)
import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (findById)
import Kernel.Storage.Esqueleto.Config (EsqLocRepDBFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
-- import Storage.Tabular.DriverInformation
-- import Storage.Tabular.Person

import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Queries.DriverLocation as QDL
import Storage.Queries.Person (findAllPersonWithDriverInfos)
import qualified Prelude

data DatabaseWith2 table1 table2 f = DatabaseWith2
  { dwTable1 :: f (B.TableEntity table1),
    dwTable2 :: f (B.TableEntity table2)
  }
  deriving (Generic, B.Database be)

create :: (L.MonadFlow m, Log m) => DriverInfo.DriverInformation -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id Person.Driver -> m (Maybe DriverInformation)
findById (Id driverInformationId) = findOneWithKV [Se.Is BeamDI.driverId $ Se.Eq driverInformationId]

-- fetchAllByIds :: (L.MonadFlow m, Log m) => Id Merchant -> [Id Driver] -> m [DriverInformation]
-- fetchAllByIds merchantId driversIds = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   let modelName = Se.modelTableName @BeamDI.DriverInformationT
--   updatedMeshConfig <- setMeshConfig modelName
--   case dbConf of
--     Just dbCOnf' -> do
--       dInfos <- either (pure []) (transformBeamDriverInformationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds)]
--       foldM (fn' dbCOnf' updatedMeshConfig) [] dInfos
--     Nothing -> pure []
--   where
--     fn' dbCOnf' updatedMeshConfig b a = do
--       id' <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamP.id $ Se.Eq (getId a.driverId), Se.Is BeamP.merchantId $ Se.Eq (getId merchantId)]]
--       case id' of
--         Right (Just _) -> pure (a : b)
--         _ -> pure b

fetchAllByIds :: (L.MonadFlow m, Log m) => Id Merchant -> [Id Driver] -> m [DriverInformation]
fetchAllByIds merchantId driversIds = do
  dInfos <- findAllWithKV [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds)]
  persons <- findAllPersonWithDriverInfos dInfos merchantId
  pure $ foldl' (getDriverInfoWithPerson persons) [] dInfos
  where
    getDriverInfoWithPerson persons acc dInfo' =
      case find (\person -> person.id == dInfo'.driverId) persons of
        Just person -> dInfo' : acc
        Nothing -> acc

fetchAllAvailableByIds :: (L.MonadFlow m, Log m) => [Id Person.Driver] -> m [DriverInformation]
fetchAllAvailableByIds driversIds = findAllWithKV [Se.And [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds), Se.Is BeamDI.active $ Se.Eq True, Se.Is BeamDI.onRide $ Se.Eq False]]

updateActivity :: (L.MonadFlow m, MonadTime m, Log m) => Id Person.Driver -> Bool -> Maybe DriverMode -> m ()
updateActivity (Id driverId) isActive mode = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.active isActive,
      Se.Set BeamDI.mode mode,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateEnabledState :: (L.MonadFlow m, MonadTime m, Log m) => Id Driver -> Bool -> m ()
updateEnabledState (Id driverId) isEnabled = do
  now <- getCurrentTime
  updateOneWithKV
    ( [ Se.Set BeamDI.enabled isEnabled,
        Se.Set BeamDI.updatedAt now
      ]
        <> ([Se.Set BeamDI.lastEnabledOn (Just now) | isEnabled])
    )
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateEnabledVerifiedState :: (L.MonadFlow m, MonadTime m, Log m) => Id Driver -> Bool -> Bool -> m ()
updateEnabledVerifiedState (Id driverId) isEnabled isVerified = do
  now <- getCurrentTime
  updateOneWithKV
    ( [ Se.Set BeamDI.enabled isEnabled,
        Se.Set BeamDI.verified isVerified,
        Se.Set BeamDI.updatedAt now
      ]
        <> ([Se.Set BeamDI.lastEnabledOn (Just now) | isEnabled])
    )
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

-- TODO @Vijay Gupta: update the following function according to main
-- updateBlockedState :: Id Person.Driver -> Bool -> SqlDB ()
-- updateBlockedState driverId isBlocked = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       ( [ DriverInformationBlocked =. val isBlocked,
--           DriverInformationUpdatedAt =. val now
--         ]
--           <> [DriverInformationNumOfLocks +=. val 1 | isBlocked]
--       )
--     where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateBlockedState :: (L.MonadFlow m, MonadTime m, Log m) => Id Person.Driver -> Bool -> m ()
updateBlockedState driverId isBlocked = do
  now <- getCurrentTime
  driverInfo <- findById driverId
  let numOfLocks' = case driverInfo of
        Just driverInfoResult -> driverInfoResult.numOfLocks
        Nothing -> 0
  updateOneWithKV
    ( [ Se.Set BeamDI.blocked isBlocked,
        Se.Set BeamDI.updatedAt now
      ]
        <> ([Se.Set BeamDI.numOfLocks (numOfLocks' + 1) | isBlocked])
    )
    [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]

verifyAndEnableDriver :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> m ()
verifyAndEnableDriver (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.enabled True,
      Se.Set BeamDI.verified True,
      Se.Set BeamDI.lastEnabledOn $ Just now,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateEnabledStateReturningIds :: (L.MonadFlow m, MonadTime m, Log m) => Id Merchant -> [Id Driver] -> Bool -> m [Id Driver]
updateEnabledStateReturningIds merchantId driverIds isEnabled = do
  present <- fmap (cast . (.driverId)) <$> fetchAllByIds merchantId driverIds
  updateEnabledStateForIds present
  pure present
  where
    updateEnabledStateForIds :: (L.MonadFlow m, MonadTime m, Log m) => [Id Driver] -> m ()
    updateEnabledStateForIds present = do
      now <- getCurrentTime
      updateWithKV
        ( [ Se.Set BeamDI.enabled isEnabled,
            Se.Set BeamDI.updatedAt now
          ]
            <> ([Se.Set BeamDI.lastEnabledOn (Just now) | isEnabled])
        )
        [Se.Is BeamDI.driverId (Se.In (getId <$> present))]

updateOnRide :: (L.MonadFlow m, MonadTime m, Log m) => Id Person.Driver -> Bool -> m ()
updateOnRide (Id driverId) onRide = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.onRide onRide,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

updateNotOnRideMultiple :: (L.MonadFlow m, MonadTime m, Log m) => [Id Person.Driver] -> m ()
updateNotOnRideMultiple driverIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDI.onRide False,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.In (getId <$> driverIds))]

deleteById :: (L.MonadFlow m, Log m) => Id Person.Driver -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamDI.driverId (Se.Eq driverId)]

findAllWithLimitOffsetByMerchantId ::
  (L.MonadFlow m, Log m) =>
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
        B.orderBy_ (\(person, driverInfo) -> B.desc_ (driverInfo.createdAt)) $
          B.limit_ (fromMaybe 100 mbLimit) $
            B.offset_ (fromMaybe 0 mbOffset) $
              B.filter_'
                ( \(person, driverInfo) ->
                    person.role B.==?. B.val_ Person.DRIVER
                      B.&&?. person.merchantId B.==?. B.val_ (getId merchantId)
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\searchString -> B.sqlBool_ (B.concat_ [person.firstName, B.val_ "", B.fromMaybe_ (B.val_ "") person.middleName, B.val_ "", B.fromMaybe_ (B.val_ "") person.lastName] `B.like_` B.val_ ("%" <> searchString <> "%"))) mbSearchString
                      B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStrDBHash -> person.mobileNumberHash B.==?. B.val_ (Just searchStrDBHash)) mbSearchStrDBHash
                )
                do
                  person' <- B.all_ (SBC.person SBC.atlasDB)
                  driverInfo' <- B.join_' (SBC.dInformation SBC.atlasDB) (\driverInfo'' -> BeamDI.driverId driverInfo'' B.==?. BeamP.id person')
                  pure (person', driverInfo')
  case res of
    Right res' -> do
      let p' = fst <$> res'
          di' = snd <$> res'
      p <- catMaybes <$> (mapM fromTType' p')
      di <- catMaybes <$> (mapM fromTType' di')
      pure $ zip p di
    Left _ -> pure []

getDriversWithOutdatedLocationsToMakeInactive :: (Transactionable m, EsqLocRepDBFlow m r) => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive before = do
  driverLocations <- QDL.getDriverLocations before
  driverInfos <- getDriverInfos driverLocations
  getDrivers driverInfos

-- logDebug $ "GetDriversWithOutdatedLocationsToMakeInactive - DLoc:- " <> show (length driverLocations) <> " DInfo:- " <> show (length driverInfos) <> " Drivers:- " <> show (length drivers)

-- getDrivers ::
--   Transactionable m =>
--   [DriverInformation] ->
--   m [Person]
-- getDrivers driverInfos = do
--   Esq.findAll $ do
--     persons <- from $ table @PersonT
--     where_ $
--       persons ^. PersonTId `in_` valList personsKeys
--     return persons
--   where
--     personsKeys = toKey . cast <$> fetchDriverIDsFromInfo driverInfos

getDrivers ::
  (L.MonadFlow m, Log m) =>
  [DriverInformation] ->
  m [Person]
getDrivers driverInfos = findAllWithKV [Se.Is BeamP.id $ Se.In personKeys]
  where
    personKeys = getId <$> fetchDriverIDsFromInfo driverInfos

-- getDriverInfos ::
--   Transactionable m =>
--   [DriverLocation] ->
--   m [DriverInformation]
-- getDriverInfos driverLocations = do
--   Esq.findAll $ do
--     driverInfos <- from $ table @DriverInformationT
--     where_ $
--       driverInfos ^. DriverInformationDriverId `in_` valList personsKeys
--         &&. driverInfos ^. DriverInformationActive
--     return driverInfos
--   where
--     personsKeys = toKey . cast <$> fetchDriverIDsFromLocations driverLocations

getDriverInfos ::
  (L.MonadFlow m, Log m) =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInfos driverLocations = findAllWithKV [Se.And [Se.Is BeamDI.driverId $ Se.In personsKeys, Se.Is BeamDI.active $ Se.Eq True]]
  where
    personsKeys = getId <$> fetchDriverIDsFromLocations driverLocations

-- imported from driverlocation query file
-- getDriverLocs ::
--   (Transactionable m, EsqLocRepDBFlow m r) =>
--   UTCTime ->
--   m [DriverLocation]
-- getDriverLocs before = do
--   runInLocReplica $
--     Esq.findAll $ do
--       driverLocs <- from $ table @DriverLocationT
--       where_ $
--         driverLocs ^. DriverLocationUpdatedAt <. val before
--       return driverLocs

fetchDriverIDsFromLocations :: [DriverLocation] -> [Id Person]
fetchDriverIDsFromLocations = map DriverLocation.driverId

fetchDriverIDsFromInfo :: [DriverInformation] -> [Id Person]
fetchDriverIDsFromInfo = map DriverInfo.driverId

addReferralCode :: (L.MonadFlow m, Log m) => Id Person -> EncryptedHashedField 'AsEncrypted Text -> m ()
addReferralCode (Id personId) code = do
  updateWithKV
    [ Se.Set BeamDI.referralCode (Just (code & unEncrypted . (.encrypted)))
    ]
    [Se.Is BeamDI.driverId (Se.Eq personId)]

countDrivers :: (L.MonadFlow m, Log m) => Id Merchant -> m (Int, Int)
countDrivers merchantID =
  getResults <$> do
    dbConf <- getMasterBeamConfig
    res <- L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_ (\(driverInformation, _) -> (B.group_ (BeamDI.active driverInformation), B.as_ @Int B.countAll_)) $
            B.filter_' (\(_, BeamP.PersonT {..}) -> merchantId B.==?. B.val_ (getId merchantID)) $
              do
                driverInformation <- B.all_ (BeamCommon.dInformation BeamCommon.atlasDB)
                person <- B.join_' (BeamCommon.person BeamCommon.atlasDB) (\person -> BeamP.id person B.==?. BeamDI.driverId driverInformation)
                pure (driverInformation, person)
    pure (either (const []) Prelude.id res)
  where
    getResults :: [(Bool, Int)] -> (Int, Int)
    getResults = foldl func (0, 0)

    func (active, inactive) (activity, counter) =
      if activity then (active + counter, inactive) else (active, inactive + counter)

countDriversInReplica :: (L.MonadFlow m, Log m) => Id Merchant -> m (Int, Int)
countDriversInReplica merchantID =
  getResults <$> do
    dbConf <- getReplicaBeamConfig
    res <- L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_ (\(driverInformation, _) -> (B.group_ (BeamDI.active driverInformation), B.as_ @Int B.countAll_)) $
            B.filter_' (\(_, BeamP.PersonT {..}) -> merchantId B.==?. B.val_ (getId merchantID)) $
              do
                driverInformation <- B.all_ (BeamCommon.dInformation BeamCommon.atlasDB)
                person <- B.join_' (BeamCommon.person BeamCommon.atlasDB) (\person -> BeamP.id person B.==?. BeamDI.driverId driverInformation)
                pure (driverInformation, person)
    pure (either (const []) Prelude.id res)
  where
    getResults :: [(Bool, Int)] -> (Int, Int)
    getResults = foldl func (0, 0)

    func (active, inactive) (activity, counter) =
      if activity then (active + counter, inactive) else (active, inactive + counter)

updateDowngradingOptions :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Bool -> Bool -> Bool -> m ()
updateDowngradingOptions (Id driverId) canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.canDowngradeToSedan canDowngradeToSedan,
      Se.Set BeamDI.canDowngradeToHatchback canDowngradeToHatchback,
      Se.Set BeamDI.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

-- updateSubscription :: Bool -> Id Person.Driver -> SqlDB ()
-- updateSubscription isSubscribed driverId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverInformationSubscribed =. val isSubscribed,
--         DriverInformationUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateSubscription :: (L.MonadFlow m, MonadTime m, Log m) => Bool -> Id Person.Driver -> m ()
updateSubscription isSubscribed (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.subscribed isSubscribed,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]

-- updateAadhaarVerifiedState :: Id Person.Driver -> Bool -> SqlDB ()
-- updateAadhaarVerifiedState personId isVerified = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverInformationAadhaarVerified =. val isVerified,
--         DriverInformationUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast personId)

updateAadhaarVerifiedState :: (L.MonadFlow m, MonadTime m, Log m) => Id Person.Driver -> Bool -> m ()
updateAadhaarVerifiedState (Id personId) isVerified = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.aadhaarVerified isVerified,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq personId)]

-- updatePendingPayment ::
--   Bool ->
--   Id Person.Driver ->
--   SqlDB ()
-- updatePendingPayment isPending driverId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverInformationPaymentPending =. val isPending,
--         DriverInformationUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updatePendingPayment :: (L.MonadFlow m, MonadTime m, Log m) => Bool -> Id Person.Driver -> m ()
updatePendingPayment isPending (Id driverId) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDI.paymentPending isPending,
      Se.Set BeamDI.updatedAt now
    ]
    [Se.Is BeamDI.driverId (Se.Eq driverId)]
