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
import Domain.Types.DriverInformation as DDI
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.DriverLocation as BeamDL
import qualified Storage.Beam.Person as BeamP hiding (Id)
import qualified Storage.Queries.DriverLocation as QueriesDL
import qualified Storage.Queries.Person as QueriesP
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.Person

create :: L.MonadFlow m => DDI.DriverInformation -> m ()
create driverInformation = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainDriverInformationToBeam driverInformation)
    Nothing -> pure ()

findById :: L.MonadFlow m => Id Person.Driver -> m (Maybe DriverInformation)
findById (Id driverInformationId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverInformationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDI.driverId $ Se.Eq driverInformationId]
    Nothing -> pure Nothing

fetchAllByIds :: L.MonadFlow m => Id Merchant -> [Id Driver] -> m [DriverInformation]
fetchAllByIds merchantId driversIds = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      dInfos <- either (pure []) (transformBeamDriverInformationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds)]
      foldM (fn' dbCOnf') [] dInfos
    Nothing -> pure []
  where
    fn' dbCOnf' b a = do
      id' <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.And [Se.Is BeamP.id $ Se.Eq (getId a.driverId), Se.Is BeamP.merchantId $ Se.Eq (getId merchantId)]]
      case id' of
        Right (Just _) -> pure (a : b)
        _ -> pure b

fetchAllAvailableByIds :: L.MonadFlow m => [Id Person.Driver] -> m [DriverInformation]
fetchAllAvailableByIds driversIds = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamDriverInformationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDI.driverId $ Se.In (getId <$> driversIds)]
    Nothing -> pure []

updateActivity :: (L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> Maybe DriverMode -> m (MeshResult ())
updateActivity (Id driverId) isActive mode = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamDI.active isActive,
          Se.Set BeamDI.mode mode,
          Se.Set BeamDI.updatedAt now
        ]
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateEnabledState :: (L.MonadFlow m, MonadTime m) => Id Driver -> Bool -> m (MeshResult ())
updateEnabledState (Id driverId) isEnabled = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        ( [ Se.Set BeamDI.enabled isEnabled,
            Se.Set BeamDI.updatedAt now
          ]
            <> ([Se.Set BeamDI.lastEnabledOn (Just now) | isEnabled])
        )
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateEnabledVerifiedState :: (L.MonadFlow m, MonadTime m) => Id Driver -> Bool -> Bool -> m (MeshResult ())
updateEnabledVerifiedState (Id driverId) isEnabled isVerified = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        ( [ Se.Set BeamDI.enabled isEnabled,
            Se.Set BeamDI.verified isVerified,
            Se.Set BeamDI.updatedAt now
          ]
            <> ([Se.Set BeamDI.lastEnabledOn (Just now) | isEnabled])
        )
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateBlockedState :: (L.MonadFlow m, MonadTime m) => Id Person.Driver -> Bool -> m (MeshResult ())
updateBlockedState (Id driverId) isBlocked = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamDI.blocked isBlocked,
          Se.Set BeamDI.updatedAt now
        ]
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

verifyAndEnableDriver :: (L.MonadFlow m, MonadTime m) => Id Person -> m (MeshResult ())
verifyAndEnableDriver (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
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
      dbConf <- L.getOption Extra.EulerPsqlDbCfg
      now <- getCurrentTime
      case dbConf of
        Just dbConf' ->
          void $
            KV.updateWoReturningWithKVConnector
              dbConf'
              Mesh.meshConfig
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
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamDI.onRide onRide,
          Se.Set BeamDI.updatedAt now
        ]
        [Se.Is BeamDI.driverId (Se.Eq driverId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateNotOnRideMultiple :: (L.MonadFlow m, MonadTime m) => [Id Person.Driver] -> m (MeshResult ())
updateNotOnRideMultiple driverIds = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamDI.onRide False,
          Se.Set BeamDI.updatedAt now
        ]
        [Se.Is BeamDI.driverId (Se.In (getId <$> driverIds))]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

deleteById :: L.MonadFlow m => Id Person.Driver -> m ()
deleteById (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
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
  findAll $ do
    (driverInformation :& _ :& person) <-
      from $
        table @DriverInformationT
          `innerJoin` table @DriverLocationT
            `Esq.on` ( \(driverInformation :& drLoc) ->
                         driverInformation ^. DriverInformationDriverId ==. drLoc ^. DriverLocationDriverId
                           &&. drLoc ^. DriverLocationUpdatedAt <. val before
                     )
          `innerJoin` table @PersonT
            `Esq.on` ( \(driverInformation :& _ :& person) ->
                         driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                     )
    where_ $ driverInformation ^. DriverInformationActive
    orderBy [asc $ driverInformation ^. DriverInformationUpdatedAt]
    pure person

getDriversWithOutdatedLocationsToMakeInactive' :: (L.MonadFlow m, Log m) => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive' before = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      -- KV.findAllWithOptionsKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDS.driverId $ Se.In (getId <$> ids)] (Se.Asc BeamDS.idleSince) (Just count_) Nothing
      dInfos <- either (pure []) (transformBeamDriverInformationToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDI.active $ Se.Eq (True)] (Se.Asc BeamDI.updatedAt) Nothing Nothing
      drLocs <- findAllDriverLocations' dbCOnf' (getId . DDI.driverId <$> dInfos)
      persons <- do
        persons' <- KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamP.id $ Se.In (getId . DDI.driverId <$> dInfos)]
        either (pure . const []) (mapM QueriesP.transformBeamPersonToDomain) persons'
      let dInfosWithLocs = foldl' (getDriverInfoWithDL drLocs) [] dInfos
          dInfosWithLocsAndPersons = foldl' (getDriverInfoWithDLAndPerson persons) [] dInfosWithLocs
      pure ((\(_, _, person) -> person) <$> dInfosWithLocsAndPersons)
    Nothing -> pure []
  where
    findAllDriverLocations' dbCOnf' driverIds = do
      conn <- L.getOrInitSqlConn dbCOnf'
      case conn of
        Right c -> do
          geoms <-
            L.runDB c $
              L.findRows $
                B.select $
                  B.filter_' (\BeamDL.DriverLocationT {..} -> B.sqlBool_ (driverId `B.in_` (B.val_ <$> driverIds)) B.&&?. (updatedAt B.==?. B.val_ before)) $
                    B.all_ (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
          pure (either (const []) (QueriesDL.transformBeamDriverLocationToDomain <$>) geoms)
        Left _ -> pure []

    getDriverInfoWithDL drLocs dInfosWithLocs dInfo =
      let drLocs' = filter (\drLoc -> drLoc.driverId == dInfo.driverId) drLocs
       in dInfosWithLocs <> ((\drLoc -> (dInfo, drLoc)) <$> drLocs')

    getDriverInfoWithDLAndPerson persons dInfosWithLocsAndPersons (dInfo, drLoc) =
      let persons'' = filter (\person -> person.id == dInfo.driverId) persons
       in dInfosWithLocsAndPersons <> ((\person -> (dInfo, drLoc, person)) <$> persons'')

-- addReferralCode :: Id Person -> EncryptedHashedField 'AsEncrypted Text -> SqlDB ()
-- addReferralCode personId code = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverInformationReferralCode =. val (Just (code & unEncrypted . (.encrypted)))
--       ]
--     where_ $ tbl ^. DriverInformationDriverId ==. val (toKey personId)

addReferralCode :: (L.MonadFlow m) => Id Person -> EncryptedHashedField 'AsEncrypted Text -> m (MeshResult ())
addReferralCode (Id personId) code = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamDI.referralCode (Just (code & unEncrypted . (.encrypted)))
        ]
        [Se.Is BeamDI.driverId (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

countDrivers :: Transactionable m => Id Merchant -> m (Int, Int)
countDrivers merchantId =
  getResults <$> do
    findAll $ do
      (driverInformation :& person) <-
        from $
          table @DriverInformationT
            `innerJoin` table @PersonT
              `Esq.on` ( \(driverInformation :& person) ->
                           driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                       )
      where_ $
        person ^. PersonMerchantId ==. (val . toKey $ merchantId)
      groupBy (driverInformation ^. DriverInformationActive)
      pure (driverInformation ^. DriverInformationActive, count @Int $ person ^. PersonId)
  where
    getResults :: [(Bool, Int)] -> (Int, Int)
    getResults = foldl func (0, 0)

    func (active, inactive) (activity, counter) =
      if activity then (active + counter, inactive) else (active, inactive + counter)

updateDowngradingOptions :: (L.MonadFlow m, MonadTime m) => Id Person -> Bool -> Bool -> Bool -> m (MeshResult ())
updateDowngradingOptions (Id driverId) canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
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
      active = active,
      onRide = onRide,
      enabled = enabled,
      blocked = blocked,
      verified = verified,
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
      BeamDI.active = active,
      BeamDI.onRide = onRide,
      BeamDI.enabled = enabled,
      BeamDI.blocked = blocked,
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
