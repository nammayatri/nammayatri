{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleExtra where

import Data.Either (fromRight)
import qualified Database.Beam as B
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Vehicle
import qualified Domain.Types.Vehicle as Variant
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Vehicle as BeamV
import Storage.Queries.OrphanInstances.Vehicle

-- Extra code goes here --
upsert :: KvDbFlow m r => Vehicle -> m ()
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
          Se.Set BeamV.updatedAt updatedAt
        ]
        [Se.Is BeamV.registrationNo (Se.Eq a.registrationNo)]
    else createWithKV a

deleteById :: KvDbFlow m r => Id Person -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamV.driverId (Se.Eq driverId)]

findByAnyOf :: KvDbFlow m r => Maybe Text -> Maybe (Id Person) -> m (Maybe Vehicle)
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

findAllByVariantRegNumMerchantId :: KvDbFlow m r => Maybe Variant.Variant -> Maybe Text -> Integer -> Integer -> Id Merchant -> m [Vehicle]
findAllByVariantRegNumMerchantId variantM mbRegNum limitVal offsetVal (Id merchantId') = do
  dbConf <- getMasterBeamConfig
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
