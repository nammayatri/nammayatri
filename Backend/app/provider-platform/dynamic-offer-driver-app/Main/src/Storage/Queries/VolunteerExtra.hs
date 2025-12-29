{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VolunteerExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Volunteer as DV
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Volunteer as Beam
import Storage.Queries.OrphanInstances.Volunteer

-- Extra code goes here --

findAllWithFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id.Id DMOC.MerchantOperatingCity ->
  Int ->
  Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  m [DV.Volunteer]
findAllWithFilters merchantOpCityId limit offset mbVolunteerId mbVendorId mbIsActive mbPlace = do
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just $ Id.getId merchantOpCityId)
        ]
          <> catMaybes
            [ mbVolunteerId <&> \volunteerId -> Se.Is Beam.id $ Se.Eq volunteerId,
              mbVendorId <&> \vendorId -> Se.Is Beam.vendorId $ Se.Eq vendorId,
              mbIsActive <&> \isActive -> Se.Is Beam.isActive $ Se.Eq (Just isActive),
              mbPlace <&> \place -> Se.Is Beam.place $ Se.Eq place
            ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findActiveVolunteerByIdAndVendorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id.Id DV.Volunteer ->
  Text ->
  m (Maybe DV.Volunteer)
findActiveVolunteerByIdAndVendorId volunteerId vendorId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Id.getId volunteerId),
          Se.Is Beam.vendorId $ Se.Eq vendorId,
          Se.Is Beam.isActive $ Se.Eq (Just True)
        ]
    ]

updateIsActiveById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id.Id DV.Volunteer ->
  Text ->
  Maybe Bool ->
  m ()
updateIsActiveById volunteerId vendorId isActive = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.isActive isActive,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Id.getId volunteerId),
          Se.Is Beam.vendorId $ Se.Eq vendorId
        ]
    ]
