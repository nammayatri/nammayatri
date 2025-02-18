module Storage.Queries.VehicleExtra where

import Data.Either (fromRight)
import qualified Database.Beam as B
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.ServiceTierType
import Domain.Types.Vehicle
import Domain.Types.VehicleCategory
import Domain.Types.VehicleVariant
import qualified Domain.Types.VehicleVariant as Variant
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Vehicle as BeamV
import qualified Storage.Queries.DriverInformation.Internal as QDriverInfoInternal
import Storage.Queries.OrphanInstances.Vehicle ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Vehicle -> m ())
create vehicle = do
  createWithKV vehicle
  whenJust vehicle.category $ \category -> QDriverInfoInternal.updateOnboardingVehicleCategory (Just category) vehicle.driverId

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Vehicle] -> m ())
createMany = traverse_ create

-- Extra code goes here --
upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Vehicle -> m ()
upsert a@Vehicle {..} = do
  res <- findOneWithKV [Se.Is BeamV.registrationNo $ Se.Eq a.registrationNo]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamV.capacity capacity,
          Se.Set BeamV.category category,
          Se.Set BeamV.make make,
          Se.Set BeamV.model model,
          Se.Set BeamV.size size,
          Se.Set BeamV.variant variant,
          Se.Set BeamV.color color,
          Se.Set BeamV.energyType energyType,
          Se.Set BeamV.registrationCategory registrationCategory,
          Se.Set BeamV.vehicleClass vehicleClass,
          Se.Set BeamV.luggageCapacity luggageCapacity,
          Se.Set BeamV.airConditioned airConditioned,
          Se.Set BeamV.vehicleRating vehicleRating,
          Se.Set BeamV.selectedServiceTiers selectedServiceTiers,
          Se.Set BeamV.mYManufacturing mYManufacturing,
          Se.Set BeamV.updatedAt updatedAt
        ]
        [Se.Is BeamV.registrationNo (Se.Eq a.registrationNo)]
    else do
      createWithKV a
  whenJust category $ \category' -> QDriverInfoInternal.updateOnboardingVehicleCategory (Just category') driverId

updateVehicleVariant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (VehicleVariant -> Maybe VehicleCategory -> Id Person -> m ())
updateVehicleVariant variant category driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set BeamV.variant variant, Se.Set BeamV.category category, Se.Set BeamV.updatedAt _now] [Se.Is BeamV.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
  whenJust category $ \category' -> QDriverInfoInternal.updateOnboardingVehicleCategory (Just category') driverId

updateVariantAndServiceTiers ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (VehicleVariant -> [ServiceTierType] -> Maybe VehicleCategory -> Id Person -> m ())
updateVariantAndServiceTiers variant selectedServiceTiers category driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamV.variant variant,
      Se.Set BeamV.selectedServiceTiers selectedServiceTiers,
      Se.Set BeamV.category category,
      Se.Set BeamV.updatedAt _now
    ]
    [Se.Is BeamV.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
  whenJust category $ \category' -> QDriverInfoInternal.updateOnboardingVehicleCategory (Just category') driverId

deleteById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamV.driverId (Se.Eq driverId)]

findByAnyOf :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Maybe (Id Person) -> m (Maybe Vehicle)
findByAnyOf registrationNoM vehicleIdM =
  findOneWithKV
    [ Se.And
        ( []
            <> if isJust vehicleIdM
              then [Se.Is BeamV.driverId $ Se.Eq (getId (fromJust vehicleIdM))]
              else
                []
                  <> ([Se.Is BeamV.registrationNo $ Se.Eq (fromJust registrationNoM) | isJust registrationNoM])
        )
    ]

findAllByVariantRegNumMerchantId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Variant.VehicleVariant -> Maybe Text -> Integer -> Integer -> Id Merchant -> m [Vehicle]
findAllByVariantRegNumMerchantId variantM mbRegNum limitVal offsetVal (Id merchantId') = do
  dbConf <- getReplicaBeamConfig
  vehicles <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\vehicle -> B.desc_ vehicle.createdAt) $
                B.filter_'
                  ( \BeamV.VehicleT {..} ->
                      merchantId B.==?. B.val_ merchantId'
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\variant' -> B.sqlBool_ (variant B.==. B.val_ variant')) variantM
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\regNoStr -> B.sqlBool_ (registrationNo `B.like_` B.val_ ("%" <> regNoStr <> "%"))) mbRegNum
                  )
                  $ B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] vehicles)
