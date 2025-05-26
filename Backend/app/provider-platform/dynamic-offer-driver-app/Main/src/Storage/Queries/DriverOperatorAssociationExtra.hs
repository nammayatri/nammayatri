module Storage.Queries.DriverOperatorAssociationExtra where

import Domain.Types.DriverOperatorAssociation
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as DVeh
import Domain.Utils (convertTextToUTC)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOperatorAssociation as Beam
import qualified Storage.Beam.DriverOperatorAssociation as BeamDOA
import Storage.Queries.OrphanInstances.DriverOperatorAssociation ()

findAllByOperatorIdWithLimitOffsetDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  m [Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation]
findAllByOperatorIdWithLimitOffsetDriverId operatorId mbIsActive mbLimit mbOffset mbDriverId =
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.operatorId $ Se.Eq operatorId]
          <> [Se.Is Beam.isActive $ Se.Eq (fromJust mbIsActive) | isJust mbIsActive]
          <> [Se.Is Beam.driverId $ Se.Eq (fromJust mbDriverId) | isJust mbDriverId]
    ]
    (Se.Asc Beam.associatedOn)
    (Just . min 10 . fromMaybe 5 $ mbLimit)
    (Just $ fromMaybe 0 mbOffset)

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

checkDriverOperatorAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkDriverOperatorAssociation driverId operatorId = do
  mbDriverOperatorAssociation <- findByDriverIdAndOperatorId driverId operatorId True
  case mbDriverOperatorAssociation of
    Nothing -> return False
    Just driverOperatorAssociation -> return (isJust driverOperatorAssociation.onboardingVehicleCategory)

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
