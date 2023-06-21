{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverInformation where

import Control.Applicative (liftA2)
import qualified Data.ByteString as BS
import qualified Database.Beam as B
import Database.Beam.Postgres hiding ((++.))
import qualified Database.Beam.Query ()
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (findById)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Logging
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.Person as BeamP
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.Person
import qualified Prelude

data DatabaseWith2 table1 table2 f = DatabaseWith2
  { dwTable1 :: f (B.TableEntity table1),
    dwTable2 :: f (B.TableEntity table2)
  }
  deriving (Generic, B.Database be)

create :: L.MonadFlow m => DriverInfo.DriverInformation -> m ()
create driverInformation = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainDriverInformationToBeam driverInformation)
    Nothing -> pure ()

findById :: L.MonadFlow m => Id Person.Driver -> m (Maybe DriverInformation)
findById (Id driverInformationId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverInformationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDI.driverId $ Se.Eq driverInformationId]
    Nothing -> pure Nothing

fetchAllByIds :: L.MonadFlow m => Id Merchant -> [Id Driver] -> m [DriverInformation]
fetchAllByIds merchantId driversIds = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      dInfos <- either (pure []) (transformBeamDriverInformationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds)]
      foldM (fn' dbCOnf' updatedMeshConfig) [] dInfos
    Nothing -> pure []
  where
    fn' dbCOnf' updatedMeshConfig b a = do
      id' <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamP.id $ Se.Eq (getId a.driverId), Se.Is BeamP.merchantId $ Se.Eq (getId merchantId)]]
      case id' of
        Right (Just _) -> pure (a : b)
        _ -> pure b

fetchAllAvailableByIds :: L.MonadFlow m => [Id Person.Driver] -> m [DriverInformation]
fetchAllAvailableByIds driversIds = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamDriverInformationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds)]
    Nothing -> pure []

updateActivity :: (L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> Maybe DriverMode -> m (MeshResult ())
updateActivity (Id driverId) isActive mode = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamDI.active isActive,
          Se.Set BeamDI.mode mode,
          Se.Set BeamDI.updatedAt now
        ]
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateEnabledState :: (L.MonadFlow m, MonadTime m) => Id Driver -> Bool -> m (MeshResult ())
updateEnabledState (Id driverId) isEnabled = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        ( [ Se.Set BeamDI.enabled isEnabled,
            Se.Set BeamDI.updatedAt now
          ]
            <> ([Se.Set BeamDI.lastEnabledOn (Just now) | isEnabled])
        )
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateEnabledVerifiedState :: (L.MonadFlow m, MonadTime m) => Id Driver -> Bool -> Bool -> m (MeshResult ())
updateEnabledVerifiedState (Id driverId) isEnabled isVerified = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        ( [ Se.Set BeamDI.enabled isEnabled,
            Se.Set BeamDI.verified isVerified,
            Se.Set BeamDI.updatedAt now
          ]
            <> ([Se.Set BeamDI.lastEnabledOn (Just now) | isEnabled])
        )
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

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

updateBlockedState :: (L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> m (MeshResult ())
updateBlockedState driverId isBlocked = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  driverInfo <- findById driverId
  let numOfLocks' = case driverInfo of
        Just driverInfoResult -> driverInfoResult.numOfLocks
        Nothing -> 0
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        ( [ Se.Set BeamDI.blocked isBlocked,
            Se.Set BeamDI.updatedAt now
          ]
            <> ([Se.Set BeamDI.numOfLocks (numOfLocks' + 1) | isBlocked])
        )
        [Se.Is BeamDI.driverId (Se.Eq (getId driverId))]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

verifyAndEnableDriver :: (L.MonadFlow m, MonadTime m) => Id Person -> m (MeshResult ())
verifyAndEnableDriver (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamDI.enabled True,
          Se.Set BeamDI.verified True,
          Se.Set BeamDI.lastEnabledOn $ Just now,
          Se.Set BeamDI.updatedAt now
        ]
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateEnabledStateReturningIds :: (L.MonadFlow m, MonadTime m) => Id Merchant -> [Id Driver] -> Bool -> m [Id Driver]
updateEnabledStateReturningIds merchantId driverIds isEnabled = do
  -- Esq.runTransaction $ do
  present <- fmap (cast . (.driverId)) <$> fetchAllByIds merchantId driverIds
  updateEnabledStateForIds present
  pure present
  where
    updateEnabledStateForIds :: (L.MonadFlow m, MonadTime m) => [Id Driver] -> m ()
    updateEnabledStateForIds present = do
      dbConf <- L.getOption KBT.PsqlDbCfg
      let modelName = Se.modelTableName @BeamDI.DriverInformationT
      let updatedMeshConfig = setMeshConfig modelName
      now <- getCurrentTime
      case dbConf of
        Just dbConf' ->
          void $
            KV.updateWoReturningWithKVConnector
              dbConf'
              updatedMeshConfig
              ( [ Se.Set BeamDI.enabled isEnabled,
                  Se.Set BeamDI.updatedAt now,
                  Se.Set BeamDI.lastEnabledOn (Just now)
                ]
                  <> ([Se.Set BeamDI.lastEnabledOn (Just now) | isEnabled])
              )
              [Se.Is BeamDI.driverId (Se.In (getId <$> present))]
        Nothing -> pure ()

updateOnRide :: (L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> m (MeshResult ())
updateOnRide (Id driverId) onRide = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamDI.onRide onRide,
          Se.Set BeamDI.updatedAt now
        ]
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateNotOnRideMultiple :: (L.MonadFlow m, MonadTime m) => [Id Person.Driver] -> m (MeshResult ())
updateNotOnRideMultiple driverIds = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamDI.onRide False,
          Se.Set BeamDI.updatedAt now
        ]
        [Se.Is BeamDI.driverId (Se.In (getId <$> driverIds))]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

deleteById :: L.MonadFlow m => Id Person.Driver -> m ()
deleteById (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure ()

findAllWithLimitOffsetByMerchantId ::
  Transactionable m =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Id Merchant ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByMerchantId mbSearchString mbSearchStrDBHash mbLimit mbOffset merchantId = do
  findAll $ do
    (person :& driverInformation) <-
      from $
        table @PersonT
          `innerJoin` table @DriverInformationT
            `Esq.on` ( \(person :& driverInformation) ->
                         driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                     )
    where_ $
      person ^. PersonRole ==. val Person.DRIVER
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
        &&. Esq.whenJust_ (liftA2 (,) mbSearchString mbSearchStrDBHash) (filterBySearchString person)
    orderBy [desc $ driverInformation ^. DriverInformationCreatedAt]
    limit limitVal
    offset offsetVal
    return (person, driverInformation)
  where
    limitVal = maybe 100 fromIntegral mbLimit
    offsetVal = maybe 0 fromIntegral mbOffset

    filterBySearchString person (searchStr, searchStrDBHash) = do
      let likeSearchStr = (%) ++. val searchStr ++. (%)
      ( concat_ @Text [person ^. PersonFirstName, val " ", unMaybe $ person ^. PersonMiddleName, val " ", unMaybe $ person ^. PersonLastName]
          `ilike` likeSearchStr
        )
        ||. person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)
    unMaybe = maybe_ (val "") identity

getDriversWithOutdatedLocationsToMakeInactive :: Transactionable m => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive before = do
  driverLocations <- getDriverLocs before
  driverInfos <- getDriverInfos driverLocations
  drivers <- getDrivers driverInfos
  logDebug $ "GetDriversWithOutdatedLocationsToMakeInactive - DLoc:- " <> show (length driverLocations) <> " DInfo:- " <> show (length driverInfos) <> " Drivers:- " <> show (length drivers)
  return drivers

getDrivers ::
  Transactionable m =>
  [DriverInformation] ->
  m [Person]
getDrivers driverInfos = do
  Esq.findAll $ do
    persons <- from $ table @PersonT
    where_ $
      persons ^. PersonTId `in_` valList personsKeys
    return persons
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromInfo driverInfos

getDriverInfos ::
  Transactionable m =>
  [DriverLocation] ->
  m [DriverInformation]
getDriverInfos driverLocations = do
  Esq.findAll $ do
    driverInfos <- from $ table @DriverInformationT
    where_ $
      driverInfos ^. DriverInformationDriverId `in_` valList personsKeys
        &&. driverInfos ^. DriverInformationActive
    return driverInfos
  where
    personsKeys = toKey . cast <$> fetchDriverIDsFromLocations driverLocations

getDriverLocs ::
  Transactionable m =>
  UTCTime ->
  m [DriverLocation]
getDriverLocs before = do
  Esq.findAll $ do
    driverLocs <- from $ table @DriverLocationT
    where_ $
      driverLocs ^. DriverLocationUpdatedAt <. val before
    return driverLocs

fetchDriverIDsFromLocations :: [DriverLocation] -> [Id Person]
fetchDriverIDsFromLocations = map DriverLocation.driverId

fetchDriverIDsFromInfo :: [DriverInformation] -> [Id Person]
fetchDriverIDsFromInfo = map DriverInfo.driverId

addReferralCode :: (L.MonadFlow m) => Id Person -> EncryptedHashedField 'AsEncrypted Text -> m (MeshResult ())
addReferralCode (Id personId) code = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamDI.referralCode (Just (code & unEncrypted . (.encrypted)))
        ]
        [Se.Is BeamDI.driverId (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

countDrivers :: L.MonadFlow m => Id Merchant -> m (Int, Int)
countDrivers merchantID =
  getResults <$> do
    dbConf <- L.getOption KBT.PsqlDbCfg
    conn <- L.getOrInitSqlConn (fromJust dbConf)
    case conn of
      Right c -> do
        resp <- L.runDB c $
          L.findRows $
            B.select $
              B.aggregate_ (\(driverInformation, _) -> (B.group_ (BeamDI.active driverInformation), B.as_ @Int B.countAll_)) $
                B.filter_' (\(_, BeamP.PersonT {..}) -> merchantId B.==?. B.val_ (getId merchantID)) $
                  do
                    driverInformation <- B.all_ (meshModelTableEntity @BeamDI.DriverInformationT @Postgres @(DatabaseWith2 BeamDI.DriverInformationT BeamP.PersonT))
                    person <- B.join_' (meshModelTableEntity @BeamP.PersonT @Postgres @(DatabaseWith2 BeamDI.DriverInformationT BeamP.PersonT)) (\person -> BeamP.id person B.==?. BeamDI.driverId driverInformation)
                    pure (driverInformation, person)
        pure (either (const []) Prelude.id resp)
      Left _ -> pure []
  where
    getResults :: [(Bool, Int)] -> (Int, Int)
    getResults = foldl func (0, 0)

    func (active, inactive) (activity, counter) =
      if activity then (active + counter, inactive) else (active, inactive + counter)

updateDowngradingOptions :: (L.MonadFlow m, MonadTime m) => Id Person -> Bool -> Bool -> Bool -> m (MeshResult ())
updateDowngradingOptions (Id driverId) canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDI.DriverInformationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamDI.canDowngradeToSedan canDowngradeToSedan,
          Se.Set BeamDI.canDowngradeToHatchback canDowngradeToHatchback,
          Se.Set BeamDI.canDowngradeToTaxi canDowngradeToTaxi,
          Se.Set BeamDI.updatedAt now
        ]
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamDriverInformationToDomain :: BeamDI.DriverInformation -> DriverInformation
transformBeamDriverInformationToDomain BeamDI.DriverInformationT {..} = do
  DriverInformation
    { driverId = Id driverId,
      adminId = Id <$> adminId,
      merchantId = Id <$> merchantId,
      active = active,
      onRide = onRide,
      enabled = enabled,
      blocked = blocked,
      verified = verified,
      numOfLocks = numOfLocks,
      referralCode = EncryptedHashed <$> (Encrypted <$> referralCode) <*> Just (DbHash BS.empty),
      lastEnabledOn = lastEnabledOn,
      canDowngradeToSedan = canDowngradeToSedan,
      canDowngradeToHatchback = canDowngradeToHatchback,
      canDowngradeToTaxi = canDowngradeToTaxi,
      mode = mode,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainDriverInformationToBeam :: DriverInformation -> BeamDI.DriverInformation
transformDomainDriverInformationToBeam DriverInformation {..} =
  BeamDI.DriverInformationT
    { BeamDI.driverId = getId driverId,
      BeamDI.adminId = getId <$> adminId,
      BeamDI.merchantId = getId <$> merchantId,
      BeamDI.active = active,
      BeamDI.onRide = onRide,
      BeamDI.enabled = enabled,
      BeamDI.blocked = blocked,
      BeamDI.numOfLocks = numOfLocks,
      BeamDI.verified = verified,
      BeamDI.referralCode = referralCode <&> unEncrypted . (.encrypted),
      BeamDI.lastEnabledOn = lastEnabledOn,
      BeamDI.canDowngradeToSedan = canDowngradeToSedan,
      BeamDI.canDowngradeToHatchback = canDowngradeToHatchback,
      BeamDI.canDowngradeToTaxi = canDowngradeToTaxi,
      BeamDI.mode = mode,
      BeamDI.createdAt = createdAt,
      BeamDI.updatedAt = updatedAt
    }
