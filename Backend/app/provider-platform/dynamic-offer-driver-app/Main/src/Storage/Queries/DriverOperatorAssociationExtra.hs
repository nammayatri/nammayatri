module Storage.Queries.DriverOperatorAssociationExtra where

import Control.Applicative (liftA2)
import Data.Text (toLower)
import qualified Database.Beam as B
import Domain.Types.DriverOperatorAssociation
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as DVeh
import Domain.Utils (convertTextToUTC)
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverOperatorAssociation as BeamDOA
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.DriverOperatorAssociation ()
import Storage.Queries.OrphanInstances.Person ()

findAllByOperatorIdWithLimitOffsetSearch ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Maybe (Id DP.Person) ->
  m [(Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation, DP.Person)]
findAllByOperatorIdWithLimitOffsetSearch operatorId mbIsActive mbLimit mbOffset mbSearchString mbDriverId =
  do
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
                B.orderBy_ (\(doa', _) -> B.desc_ doa'.associatedOn) $
                  B.filter_'
                    ( \(doa, person) ->
                        doa.operatorId B.==?. B.val_ operatorId
                          B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\driverId -> person.id B.==?. B.val_ driverId.getId) mbDriverId
                          B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> doa.isActive B.==?. B.val_ isActive) mbIsActive
                          B.&&?. B.sqlBool_ (doa.associatedTill B.>=. B.val_ (Just now))
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
                      doa <- B.all_ (BeamCommon.driverOperatorAssociation BeamCommon.atlasDB)
                      person <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\person -> BeamDOA.driverId doa B.==. BeamP.id person)
                      pure (doa, person)
    case res of
      Right doaList ->
        catMaybes <$> mapM (\(d, p) -> liftA2 (,) <$> fromTType' d <*> fromTType' p) doaList
      Left _ -> pure []

createDriverOperatorAssociationIfNotExists ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Id DP.Person ->
  DVeh.VehicleCategory ->
  Bool ->
  m ()
createDriverOperatorAssociationIfNotExists moc driverId operatorId onboardingVehicleCategory isActive = do
  now <- getCurrentTime

  mbDriverOperatorAssociation <- findByDriverIdAndOperatorId driverId operatorId isActive
  case mbDriverOperatorAssociation of
    Just driverOperatorAssociation ->
      when (isNothing driverOperatorAssociation.onboardingVehicleCategory) $ do
        updateWithKV
          [ Se.Set BeamDOA.onboardingVehicleCategory (Just onboardingVehicleCategory),
            Se.Set BeamDOA.updatedAt now
          ]
          [Se.And [Se.Is BeamDOA.id $ Se.Eq driverOperatorAssociation.id.getId]]
    Nothing -> do
      id <- generateGUID
      createWithKV $
        DriverOperatorAssociation
          { associatedTill = convertTextToUTC (Just "2099-12-12"),
            driverId = driverId,
            operatorId = operatorId.getId,
            associatedOn = Just now,
            onboardingVehicleCategory = Just onboardingVehicleCategory,
            createdAt = now,
            updatedAt = now,
            merchantId = Just moc.merchantId,
            merchantOperatingCityId = Just moc.id,
            ..
          }

findByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Bool ->
  m (Maybe DriverOperatorAssociation)
findByDriverId driverId isActive = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is BeamDOA.driverId $ Se.Eq (driverId.getId),
            Se.Is BeamDOA.isActive $ Se.Eq isActive,
            Se.Is BeamDOA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
      (Se.Desc BeamDOA.createdAt)
      (Just 1)
      Nothing

findAllByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Bool ->
  m [DriverOperatorAssociation]
findAllByDriverId driverId isActive = do
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamDOA.driverId $ Se.Eq (driverId.getId),
          Se.Is BeamDOA.isActive $ Se.Eq isActive,
          Se.Is BeamDOA.associatedTill (Se.GreaterThan $ Just now)
        ]
    ]
    (Se.Desc BeamDOA.createdAt)
    Nothing
    Nothing

endOperatorDriverAssociation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Id DP.Person -> m ()
endOperatorDriverAssociation operatorId (Id driverId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamDOA.associatedTill $ Just now, Se.Set BeamDOA.isActive False]
    [ Se.And
        [ Se.Is BeamDOA.operatorId (Se.Eq operatorId),
          Se.Is BeamDOA.associatedTill (Se.GreaterThan $ Just now),
          Se.Is BeamDOA.driverId (Se.Eq driverId)
        ]
    ]

findByDriverIdAndOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DP.Person ->
  Bool ->
  m (Maybe DriverOperatorAssociation)
findByDriverIdAndOperatorId driverId operatorId isActive = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And
          [ Se.Is BeamDOA.driverId $ Se.Eq (driverId.getId),
            Se.Is BeamDOA.operatorId $ Se.Eq operatorId.getId,
            Se.Is BeamDOA.isActive $ Se.Eq isActive,
            Se.Is BeamDOA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
      (Se.Desc BeamDOA.createdAt)
      (Just 1)
      Nothing

deleteByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  m ()
deleteByDriverId driverId = do
  deleteWithKV [Se.And [Se.Is BeamDOA.driverId $ Se.Eq (driverId.getId)]]

findActiveAssociationByOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  m (Maybe DriverOperatorAssociation)
findActiveAssociationByOperatorId operatorId = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV'
      [ Se.And
          [ Se.Is BeamDOA.operatorId $ Se.Eq operatorId.getId,
            Se.Is BeamDOA.isActive $ Se.Eq True,
            Se.Is BeamDOA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
      (Just 1)
      Nothing

deleteByOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> m ()
deleteByOperatorId operatorId = do
  deleteWithKV [Se.Is BeamDOA.operatorId $ Se.Eq (getId operatorId)]
