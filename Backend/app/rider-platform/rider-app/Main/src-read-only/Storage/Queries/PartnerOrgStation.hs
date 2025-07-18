{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PartnerOrgStation where

import qualified Domain.Types.PartnerOrgStation
import qualified Domain.Types.PartnerOrganization
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PartnerOrgStation as Beam

findByPOrgIdAndPOrgStationId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization -> Kernel.Types.Id.Id Domain.Types.PartnerOrgStation.PartnerOrgStation -> m (Maybe Domain.Types.PartnerOrgStation.PartnerOrgStation))
findByPOrgIdAndPOrgStationId partnerOrgId partnerOrgStationId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.partnerOrgId $ Se.Eq (Kernel.Types.Id.getId partnerOrgId),
          Se.Is Beam.partnerOrgStationId $ Se.Eq (Kernel.Types.Id.getId partnerOrgStationId)
        ]
    ]

findByStationCodeAndPOrgId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization -> m (Maybe Domain.Types.PartnerOrgStation.PartnerOrgStation))
findByStationCodeAndPOrgId stationCode partnerOrgId = do findOneWithKV [Se.And [Se.Is Beam.stationId $ Se.Eq stationCode, Se.Is Beam.partnerOrgId $ Se.Eq (Kernel.Types.Id.getId partnerOrgId)]]

instance FromTType' Beam.PartnerOrgStation Domain.Types.PartnerOrgStation.PartnerOrgStation where
  fromTType' (Beam.PartnerOrgStationT {..}) = do
    pure $
      Just
        Domain.Types.PartnerOrgStation.PartnerOrgStation
          { name = name,
            partnerOrgId = Kernel.Types.Id.Id partnerOrgId,
            partnerOrgStationId = Kernel.Types.Id.Id partnerOrgStationId,
            stationCode = stationId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PartnerOrgStation Domain.Types.PartnerOrgStation.PartnerOrgStation where
  toTType' (Domain.Types.PartnerOrgStation.PartnerOrgStation {..}) = do
    Beam.PartnerOrgStationT
      { Beam.name = name,
        Beam.partnerOrgId = Kernel.Types.Id.getId partnerOrgId,
        Beam.partnerOrgStationId = Kernel.Types.Id.getId partnerOrgStationId,
        Beam.stationId = stationCode,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
