module Storage.Queries.FleetOperatorAssociationExtra where

import Control.Applicative (liftA3)
import Data.Text (toLower)
import qualified Database.Beam as B
import Domain.Types.FleetOperatorAssociation
import qualified Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.Person as DP
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EncFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.FleetOperatorAssociation as BeamFOA
import qualified Storage.Beam.FleetOwnerInformation as BeamFOI
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.FleetOperatorAssociation ()
import Storage.Queries.OrphanInstances.FleetOwnerInformation ()
import Storage.Queries.OrphanInstances.Person ()

findAllActiveByOperatorIdWithLimitOffsetSearch ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EncFlow m r
  ) =>
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  m [(FleetOperatorAssociation, DP.Person, FOI.FleetOwnerInformation)]
findAllActiveByOperatorIdWithLimitOffsetSearch operatorId mbLimit mbOffset mbSearchString mbIsActive mbEnabled mbVerified = do
  dbConf <- getReplicaBeamConfig
  now <- getCurrentTime
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString
  let limit = min 10 $ fromMaybe 5 mbLimit
      offset = fromMaybe 0 mbOffset
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\(foa', _, _) -> B.desc_ foa'.associatedOn) $
                B.filter_'
                  ( \(foa, person, fleetOwnerInfo) ->
                      foa.operatorId B.==?. B.val_ operatorId
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> foa.isActive B.==?. B.val_ isActive) mbIsActive
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\enabled -> fleetOwnerInfo.enabled B.==?. B.val_ enabled) mbEnabled
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\verified -> fleetOwnerInfo.verified B.==?. B.val_ verified) mbVerified
                        B.&&?. B.sqlBool_ (foa.associatedTill B.>=. B.val_ (Just now))
                        B.&&?. ( case mbSearchString of
                                   Nothing -> B.sqlBool_ $ B.val_ True
                                   Just searchString ->
                                     B.sqlBool_
                                       (B.lower_ person.firstName `B.like_` B.lower_ (B.val_ ("%" <> toLower searchString <> "%")))
                                       B.||?. ( B.sqlBool_
                                                  (B.lower_ (B.coalesce_ [person.lastName] (B.val_ "")) `B.like_` B.lower_ (B.val_ ("%" <> toLower searchString <> "%")))
                                              )
                                       B.||?. B.sqlBool_
                                         (B.like_ (B.coalesce_ [person.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> searchString <> "%")))
                                       B.||?. maybe
                                         (B.sqlBool_ $ B.val_ False)
                                         (\mobileNumberSearchStringDB -> person.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB))
                                         encryptedMobileNumberHash
                               )
                  )
                  do
                    foa <- B.all_ (BeamCommon.fleetOperatorAssociation BeamCommon.atlasDB)
                    person <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\person -> BeamFOA.fleetOwnerId foa B.==. BeamP.id person)
                    fleetOwnerInfo <- B.join_ (BeamCommon.fleetOwnerInformation BeamCommon.atlasDB) (\fleetOwnerInfo -> BeamFOA.fleetOwnerId foa B.==. BeamFOI.fleetOwnerPersonId fleetOwnerInfo)
                    pure (foa, person, fleetOwnerInfo)
  case res of
    Right foaList ->
      catMaybes <$> mapM (\(f, p, fi) -> liftA3 (,,) <$> fromTType' f <*> fromTType' p <*> fromTType' fi) foaList
    Left _ -> pure []

findAllByFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Bool ->
  m [FleetOperatorAssociation]
findAllByFleetOwnerId fleetOwnerId isActive = do
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamFOA.fleetOwnerId $ Se.Eq fleetOwnerId.getId,
          Se.Is BeamFOA.isActive $ Se.Eq isActive,
          Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
        ]
    ]
    (Se.Desc BeamFOA.createdAt)
    Nothing
    Nothing

findActiveByFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  m (Maybe FleetOperatorAssociation)
findActiveByFleetOwnerId fleetOwnerId = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV'
      [ Se.And
          [ Se.Is BeamFOA.fleetOwnerId $ Se.Eq fleetOwnerId.getId,
            Se.Is BeamFOA.isActive $ Se.Eq True,
            Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
      (Just 1)
      Nothing

findByFleetOwnerIdAndIsActive ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Bool ->
  m (Maybe FleetOperatorAssociation)
findByFleetOwnerIdAndIsActive fleetOwnerId isActive = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is BeamFOA.fleetOwnerId $ Se.Eq fleetOwnerId.getId,
            Se.Is BeamFOA.isActive $ Se.Eq isActive,
            Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
      (Se.Desc BeamFOA.createdAt)
      (Just 1)
      Nothing

-- including inactive
findAllByFleetIdAndOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DP.Person ->
  m [FleetOperatorAssociation]
findAllByFleetIdAndOperatorId fleetOwnerId operatorId = do
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamFOA.fleetOwnerId $ Se.Eq fleetOwnerId.getId,
          Se.Is BeamFOA.operatorId $ Se.Eq operatorId.getId,
          Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
        ]
    ]
    (Se.Desc BeamFOA.createdAt)
    Nothing
    Nothing

endFleetOperatorAssociation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m ()
endFleetOperatorAssociation fleetOwnerId operatorId = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamFOA.associatedTill $ Just now, Se.Set BeamFOA.isActive False]
    [ Se.And
        [ Se.Is BeamFOA.operatorId (Se.Eq operatorId.getId),
          Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now),
          Se.Is BeamFOA.fleetOwnerId (Se.Eq fleetOwnerId.getId)
        ]
    ]

findByFleetOwnerIdAndOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DP.Person ->
  Bool ->
  m (Maybe FleetOperatorAssociation)
findByFleetOwnerIdAndOperatorId fleetOwnerId operatorId isActive = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is BeamFOA.fleetOwnerId $ Se.Eq fleetOwnerId.getId,
            Se.Is BeamFOA.operatorId $ Se.Eq operatorId.getId,
            Se.Is BeamFOA.isActive $ Se.Eq isActive,
            Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
      (Se.Desc BeamFOA.createdAt)
      (Just 1)
      Nothing

findAllActiveByOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  m [FleetOperatorAssociation]
findAllActiveByOperatorId operatorId = do
  now <- getCurrentTime
  findAllWithKV
    [ Se.And
        [ Se.Is BeamFOA.operatorId $ Se.Eq operatorId,
          Se.Is BeamFOA.isActive $ Se.Eq True,
          Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
        ]
    ]

findActiveAssociationByOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  m (Maybe FleetOperatorAssociation)
findActiveAssociationByOperatorId (Id operatorId) = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV'
      [ Se.And
          [ Se.Is BeamFOA.operatorId $ Se.Eq operatorId,
            Se.Is BeamFOA.isActive $ Se.Eq True,
            Se.Is BeamFOA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
      (Just 1)
      Nothing

deleteByOperatorId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m ()
deleteByOperatorId (Id operatorId) = deleteWithKV [Se.Is BeamFOA.operatorId (Se.Eq operatorId)]
