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
import qualified Storage.Beam.DriverOperatorAssociation as BeamDOA
import Storage.Queries.OrphanInstances.DriverOperatorAssociation ()

-- Extra code goes here --

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
          { associatedTill = convertTextToUTC (Just "2099-12-12"), -- why it is hardcoded everywhere?
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

-- TODO check associatedTill manually instead of this query
-- findByDriverId ::
--   (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
--   Id DP.Person ->
--   Bool ->
--   m (Maybe DriverOperatorAssociation)
-- findByDriverId driverId isActive = do
--   now <- getCurrentTime
--   listToMaybe
--     <$> findAllWithOptionsKV
--       [ Se.And
--           [ Se.Is BeamDOA.driverId $ Se.Eq (driverId.getId),
--             Se.Is BeamDOA.isActive $ Se.Eq isActive,
--             Se.Is BeamDOA.associatedTill (Se.GreaterThan $ Just now)
--           ]
--       ]
--       (Se.Desc BeamDOA.createdAt)
--       (Just 1)
--       Nothing

checkDriverOperatorAssociation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> Id DP.Person -> m Bool
checkDriverOperatorAssociation driverId operatorId = do
  mbDriverOperatorAssociation <- findByDriverIdAndOperatorId driverId operatorId True
  case mbDriverOperatorAssociation of
    Nothing -> return False
    Just driverOperatorAssociation -> return (isJust driverOperatorAssociation.onboardingVehicleCategory)

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
