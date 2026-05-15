module Storage.Queries.VehicleExtra where

import Control.Applicative ((<|>))
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Time.Calendar as Days
import qualified Database.Beam as B
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.ServiceTierType
import Domain.Types.Vehicle
import Domain.Types.VehicleCategory
import Domain.Types.VehicleVariant
import qualified Domain.Types.VehicleVariant as Variant
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified SharedLogic.DriverPool.LTSDataSync as LTSSync
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.Vehicle as BeamV
import qualified Storage.Queries.DriverInformation.Internal as QDriverInfoInternal
import Storage.Queries.OrphanInstances.Vehicle ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => (Vehicle -> m ())
create vehicle = do
  createWithKV vehicle
  whenJust vehicle.category $ \category -> QDriverInfoInternal.updateOnboardingVehicleCategory (Just category) vehicle.driverId
  LTSSync.syncDriverPoolDataToLTS (cast vehicle.driverId) $
    LTSSync.emptyUpdate
      { LTSSync.variant = LTSSync.Set vehicle.variant,
        LTSSync.selectedServiceTiers = LTSSync.Set vehicle.selectedServiceTiers,
        LTSSync.vehicleTags = LTSSync.Set vehicle.vehicleTags,
        LTSSync.mYManufacturing = LTSSync.Set vehicle.mYManufacturing,
        LTSSync.airConditioned = LTSSync.Set vehicle.airConditioned,
        LTSSync.luggageCapacity = LTSSync.Set vehicle.luggageCapacity,
        LTSSync.vehicleRating = LTSSync.Set vehicle.vehicleRating
      }

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => ([Vehicle] -> m ())
createMany = traverse_ create

-- Extra code goes here --
upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisLTSFlowEnv r) => Vehicle -> m ()
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
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.variant = LTSSync.Set variant,
        LTSSync.selectedServiceTiers = LTSSync.Set selectedServiceTiers,
        LTSSync.mYManufacturing = LTSSync.Set mYManufacturing,
        LTSSync.vehicleTags = LTSSync.Set vehicleTags,
        LTSSync.airConditioned = LTSSync.Set airConditioned,
        LTSSync.luggageCapacity = LTSSync.Set luggageCapacity,
        LTSSync.vehicleRating = LTSSync.Set vehicleRating
      }

updateVehicleVariant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  (VehicleVariant -> Maybe VehicleCategory -> Id Person -> m ())
updateVehicleVariant variant category driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set BeamV.variant variant, Se.Set BeamV.category category, Se.Set BeamV.updatedAt _now] [Se.Is BeamV.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
  whenJust category $ \category' -> QDriverInfoInternal.updateOnboardingVehicleCategory (Just category') driverId
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.variant = LTSSync.Set variant}

updateVariantAndServiceTiers ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
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
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate
      { LTSSync.variant = LTSSync.Set variant,
        LTSSync.selectedServiceTiers = LTSSync.Set selectedServiceTiers
      }

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

findEnabledByVariantCityAndManufacturingDateAfter :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => VehicleVariant -> Days.Day -> Id Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Double -> Int -> Int -> m [Vehicle]
findEnabledByVariantCityAndManufacturingDateAfter variantVal cutoffDate (Id merchantId') opCityId mbMaxVehicleRating limitVal offsetVal = do
  dbConf <- getReplicaBeamConfig
  result <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limitVal) $
            B.offset_ (fromIntegral offsetVal) $
              B.orderBy_ (\(vehicle, _driverInfo) -> B.desc_ vehicle.createdAt) $
                B.filter_'
                  ( \(vehicle, driverInfo) ->
                      vehicle.merchantId B.==?. B.val_ merchantId'
                        B.&&?. B.sqlBool_ (vehicle.variant B.==. B.val_ variantVal)
                        B.&&?. B.maybe_ (B.sqlBool_ $ B.val_ True) (\mfgDate -> B.sqlBool_ (mfgDate B.>=. B.val_ cutoffDate)) vehicle.mYManufacturing
                        B.&&?. B.sqlBool_ (driverInfo.enabled B.==. B.val_ True)
                        B.&&?. B.sqlBool_ (driverInfo.blocked B.==. B.val_ False)
                        B.&&?. (driverInfo.merchantOperatingCityId B.==?. B.val_ (Just $ getId opCityId))
                        B.&&?. vehicleRatingFilter vehicle.vehicleRating
                  )
                  do
                    vehicle <- B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)
                    driverInfo <- B.join_' (BeamCommon.driverInformation BeamCommon.atlasDB) (\di -> BeamV.driverId vehicle B.==?. BeamDI.driverId di)
                    pure (vehicle, driverInfo)
  case result of
    Right rows -> catMaybes <$> mapM (fromTType' . fst) rows
    Left _ -> pure []
  where
    vehicleRatingFilter vr = case mbMaxVehicleRating of
      Nothing -> B.sqlBool_ (B.isNothing_ vr)
      Just maxRating ->
        B.sqlBool_ (B.isNothing_ vr) B.||?. B.maybe_ (B.sqlBool_ $ B.val_ False) (\r -> B.sqlBool_ (r B.<. B.val_ maxRating)) vr

findAllByDriverIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Id Person] -> m [Vehicle]
findAllByDriverIds driverIds =
  if null driverIds
    then pure []
    else findAllWithKV [Se.Is BeamV.driverId $ Se.In (getId <$> driverIds)]

-- Wrapper for src-read-only function with LTS sync

updateSelectedServiceTiers :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => [ServiceTierType] -> Id Person -> m ()
updateSelectedServiceTiers tiers driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamV.selectedServiceTiers tiers, Se.Set BeamV.updatedAt _now] [Se.Is BeamV.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.selectedServiceTiers = LTSSync.Set tiers}

updateManufacturing :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => Maybe Days.Day -> Id Person -> m ()
updateManufacturing mYManufacturing driverId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set BeamV.mYManufacturing mYManufacturing, Se.Set BeamV.updatedAt _now] [Se.Is BeamV.driverId $ Se.Eq (getId driverId)]
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.mYManufacturing = LTSSync.Set mYManufacturing}

findByDriverIdAndRegistrationNo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Text -> m (Maybe Vehicle)
findByDriverIdAndRegistrationNo driverId registrationNo = do
  findOneWithKV [Se.Is BeamV.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is BeamV.registrationNo $ Se.Eq registrationNo]

updateMerchantIdAndCityIdByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id Merchant -> Maybe Text -> m ()
updateMerchantIdAndCityIdByDriverId driverId merchantId mbMerchantOperatingCityId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamV.merchantId (getId merchantId),
      Se.Set BeamV.merchantOperatingCityId mbMerchantOperatingCityId,
      Se.Set BeamV.updatedAt now
    ]
    [Se.Is BeamV.driverId (Se.Eq $ getId driverId)]

-- | Update vehicle row by registration number only (works even when RC is unassigned to a driver).
updateVehicleFromRcEdit ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  m ()
updateVehicleFromRcEdit oldRegistrationNo newRegistrationNo mbMake mbModel mbColor mbYearInt = do
  mbVehicle <- findOneWithKV [Se.Is BeamV.registrationNo $ Se.Eq oldRegistrationNo]
  whenJust mbVehicle $ \veh -> do
    now <- getCurrentTime
    let make' = mbMake <|> veh.make
        model' = maybe veh.model T.strip mbModel
        color' = maybe veh.color T.strip mbColor
        mYManufacturing =
          maybe veh.mYManufacturing (\y -> Just $ Days.fromGregorian (fromIntegral y) 1 1) mbYearInt
    updateOneWithKV
      [ Se.Set BeamV.registrationNo newRegistrationNo,
        Se.Set BeamV.make make',
        Se.Set BeamV.model model',
        Se.Set BeamV.color color',
        Se.Set BeamV.mYManufacturing mYManufacturing,
        Se.Set BeamV.updatedAt now
      ]
      [Se.Is BeamV.registrationNo $ Se.Eq oldRegistrationNo]
    LTSSync.syncDriverPoolDataToLTS (cast veh.driverId) $
      LTSSync.emptyUpdate {LTSSync.mYManufacturing = LTSSync.Set mYManufacturing, LTSSync.registrationNo = LTSSync.Set newRegistrationNo}

-- | When the driver's vehicle row still matches the old RC registration number, sync fields from optional dashboard edits.
updateFleetVehicleFromDashboardRcEdit ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  m ()
updateFleetVehicleFromDashboardRcEdit driverId oldRegistrationNo newRegistrationNo mbMake mbModel mbColor mbYearInt = do
  mbVehicle <- findByDriverIdAndRegistrationNo driverId oldRegistrationNo
  whenJust mbVehicle $ \veh -> do
    now <- getCurrentTime
    let make' = mbMake <|> veh.make
        model' = maybe veh.model T.strip mbModel
        color' = maybe veh.color T.strip mbColor
        mYManufacturing =
          maybe veh.mYManufacturing (\y -> Just $ Days.fromGregorian (fromIntegral y) 1 1) mbYearInt
    updateOneWithKV
      [ Se.Set BeamV.registrationNo newRegistrationNo,
        Se.Set BeamV.make make',
        Se.Set BeamV.model model',
        Se.Set BeamV.color color',
        Se.Set BeamV.mYManufacturing mYManufacturing,
        Se.Set BeamV.updatedAt now
      ]
      [ Se.Is BeamV.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
        Se.Is BeamV.registrationNo $ Se.Eq oldRegistrationNo
      ]
    LTSSync.syncDriverPoolDataToLTS (cast driverId) $
      LTSSync.emptyUpdate {LTSSync.mYManufacturing = LTSSync.Set mYManufacturing, LTSSync.registrationNo = LTSSync.Set newRegistrationNo}
