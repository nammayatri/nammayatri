{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PartnerOrgStationExtra where

import qualified Domain.Types.PartnerOrgStation
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Station
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PartnerOrgStation as BeamPOS

updatePartnerOrgStationId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> Text -> m ()
updatePartnerOrgStationId oldPartnerOrgStationID newPartnerOrgStationID partnerOrgID = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set BeamPOS.updatedAt now]
        <> [Se.Set BeamPOS.partnerOrgStationId newPartnerOrgStationID]
    )
    [ Se.And
        [ Se.Is BeamPOS.partnerOrgStationId $ Se.Eq oldPartnerOrgStationID,
          Se.Is BeamPOS.partnerOrgId $ Se.Eq partnerOrgID
        ]
    ]
