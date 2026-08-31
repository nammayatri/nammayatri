{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.AutoApplyCohortMappingExtra where

import qualified Data.Text as T
import Database.Beam.Postgres (Postgres)
import Domain.Types.VehicleCategory (VehicleCategory)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import qualified Lib.IncentiveJourney.Domain.Types.AutoApplyCohortMapping as DAuto
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Storage.Beam.AutoApplyCohortMapping as Beam
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.AutoApplyCohortMapping ()
import qualified Sequelize as Se

createAutoApplyCohortMapping ::
  (BeamFlow m r) =>
  Id DCJM.CohortJourneyMapping ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Maybe VehicleCategory ->
  Bool ->
  Bool ->
  m DAuto.AutoApplyCohortMapping
createAutoApplyCohortMapping cohortJourneyMappingId merchantId merchantOperatingCityId vehicleCategory allowIfNoMapping enabled = do
  now <- getCurrentTime
  rowId <- generateGUID
  let row =
        DAuto.AutoApplyCohortMapping
          { id = rowId,
            cohortJourneyMappingId = cohortJourneyMappingId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            vehicleCategory = vehicleCategory,
            allowIfNoMapping = allowIfNoMapping,
            enabled = enabled,
            createdAt = now,
            updatedAt = now
          }
  createWithKV row
  pure row

findAutoApplyCohortMappingById ::
  (BeamFlow m r) =>
  Id DAuto.AutoApplyCohortMapping ->
  m (Maybe DAuto.AutoApplyCohortMapping)
findAutoApplyCohortMappingById rowId =
  findOneWithKV [Se.Is Beam.id $ Se.Eq (getId rowId)]

updateAutoApplyCohortMapping ::
  (BeamFlow m r) =>
  DAuto.AutoApplyCohortMapping ->
  m DAuto.AutoApplyCohortMapping
updateAutoApplyCohortMapping updated = do
  now <- getCurrentTime
  let row = updated {DAuto.updatedAt = now}
  updateWithKV
    [ Se.Set Beam.vehicleCategory (T.pack . show <$> row.vehicleCategory),
      Se.Set Beam.allowIfNoMapping row.allowIfNoMapping,
      Se.Set Beam.enabled row.enabled,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId row.id)]
  pure row

findByCohortJourneyMappingIdAndVehicleCategory ::
  (BeamFlow m r) =>
  Id DCJM.CohortJourneyMapping ->
  Maybe VehicleCategory ->
  m (Maybe DAuto.AutoApplyCohortMapping)
findByCohortJourneyMappingIdAndVehicleCategory cohortJourneyMappingId mbVehicleCategory =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.cohortJourneyMappingId $ Se.Eq (getId cohortJourneyMappingId),
          vehicleCategoryClause mbVehicleCategory
        ]
    ]

-- | Enabled rows for this merchant and city that cover the driver's category.
-- A null vehicle category on the row covers every category. A driver with no
-- category only matches those city-wide rows.
findApplicableByMerchantAndCity ::
  (BeamFlow m r) =>
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Maybe Text ->
  m [DAuto.AutoApplyCohortMapping]
findApplicableByMerchantAndCity merchantId merchantOperatingCityId mbDriverVehicleCategory =
  findAllWithKV
    [ Se.And
        ( [ Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
            Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
            Se.Is Beam.enabled $ Se.Eq True,
            driverCategoryMatch mbDriverVehicleCategory
          ]
        )
    ]

findByMerchantAndCity ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Maybe VehicleCategory ->
  m [DAuto.AutoApplyCohortMapping]
findByMerchantAndCity mbLimit mbOffset merchantId merchantOperatingCityId mbVehicleCategory = do
  let categoryClause = case mbVehicleCategory of
        Just category -> [Se.Is Beam.vehicleCategory $ Se.Eq (Just (T.pack (show category)))]
        Nothing -> []
      whereClause =
        [ Se.And
            ( [ Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
                Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId)
              ]
                <> categoryClause
            )
        ]
  findAllWithOptionsKV whereClause (Se.Desc Beam.createdAt) mbLimit mbOffset

vehicleCategoryClause ::
  Maybe VehicleCategory ->
  Se.Clause Postgres Beam.AutoApplyCohortMappingT
vehicleCategoryClause = \case
  Nothing -> Se.Is Beam.vehicleCategory Se.Null
  Just category -> Se.Is Beam.vehicleCategory $ Se.Eq (Just (T.pack (show category)))

driverCategoryMatch ::
  Maybe Text ->
  Se.Clause Postgres Beam.AutoApplyCohortMappingT
driverCategoryMatch = \case
  Nothing -> Se.Is Beam.vehicleCategory Se.Null
  Just category ->
    Se.Or
      [ Se.Is Beam.vehicleCategory Se.Null,
        Se.Is Beam.vehicleCategory $ Se.Eq (Just category)
      ]
