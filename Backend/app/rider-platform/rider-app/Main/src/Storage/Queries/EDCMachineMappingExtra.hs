{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EDCMachineMappingExtra where

import Domain.Types.EDCMachineMapping
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.EDCMachineMapping as BeamT
import Storage.Queries.OrphanInstances.EDCMachineMapping

-- | Find active EDC machine mapping for a person, merchant, and operating city
findActiveByPersonIdAndMerchant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Person ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m (Maybe EDCMachineMapping)
findActiveByPersonIdAndMerchant personId merchantId merchantOperatingCityId =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamT.personId $ Se.Eq personId.getId,
          Se.Is BeamT.merchantId $ Se.Eq merchantId.getId,
          Se.Is BeamT.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
          Se.Is BeamT.isActive $ Se.Eq True
        ]
    ]

-- | Find all active EDC machine mappings for a merchant and operating city
findAllActiveByMerchant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [EDCMachineMapping]
findAllActiveByMerchant merchantId merchantOperatingCityId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamT.merchantId $ Se.Eq merchantId.getId,
          Se.Is BeamT.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
          Se.Is BeamT.isActive $ Se.Eq True
        ]
    ]
    (Se.Desc BeamT.createdAt)
    Nothing
    Nothing

-- | Find all active EDC machine mappings for a person
findAllActiveByPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Person ->
  m [EDCMachineMapping]
findAllActiveByPersonId personId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamT.personId $ Se.Eq personId.getId,
          Se.Is BeamT.isActive $ Se.Eq True
        ]
    ]
    (Se.Desc BeamT.createdAt)
    Nothing
    Nothing

-- | Deactivate existing mapping when creating a new one (to enforce unique constraint)
deactivateExistingMapping ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Person ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m ()
deactivateExistingMapping personId merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamT.isActive False,
      Se.Set BeamT.updatedAt now
    ]
    [ Se.And
        [ Se.Is BeamT.personId $ Se.Eq personId.getId,
          Se.Is BeamT.merchantId $ Se.Eq merchantId.getId,
          Se.Is BeamT.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
          Se.Is BeamT.isActive $ Se.Eq True
        ]
    ]

-- | Find EDC machine mappings with optional filters (for dashboard list API).
-- Always scoped to merchant and merchantOperatingCityId from path.
findAllByMerchantAndCityWithFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  Maybe (Id Person) ->
  Maybe Bool ->
  m [EDCMachineMapping]
findAllByMerchantAndCityWithFilters merchantId merchantOperatingCityId mbPersonId mbIsActive =
  findAllWithOptionsKV conditions (Se.Desc BeamT.createdAt) Nothing Nothing
  where
    isActiveVal = fromMaybe True mbIsActive
    conditions =
      [ Se.And $
          [ Se.Is BeamT.merchantId $ Se.Eq merchantId.getId,
            Se.Is BeamT.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
            Se.Is BeamT.isActive $ Se.Eq isActiveVal
          ]
            <> maybe [] (\p -> [Se.Is BeamT.personId $ Se.Eq p.getId]) mbPersonId
      ]
