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
  Maybe Text ->
  m [DV.Volunteer]
findAllWithFilters merchantOpCityId limit offset mbVolunteerId mbPlace mbVendorId = do
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just $ Id.getId merchantOpCityId)
        ]
          <> catMaybes
            [ mbVolunteerId <&> \volunteerId -> Se.Is Beam.id $ Se.Eq volunteerId,
              mbPlace <&> \place -> Se.Is Beam.place $ Se.Eq place,
              mbVendorId <&> \vendorId -> Se.Is Beam.vendorId $ Se.Eq (Just vendorId)
            ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)
