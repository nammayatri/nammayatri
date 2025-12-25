{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SuspectStatusHistoryExtra where

import qualified Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusHistory
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.SuspectStatusHistory as Beam
import Storage.Queries.OrphanInstances.SuspectStatusHistory ()

-- Extra code goes here --

findAllByDlAndAdminApprovalStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.SuspectFlagRequest.AdminApproval -> Maybe Text -> m [Domain.Types.SuspectStatusHistory.SuspectStatusHistory]
findAllByDlAndAdminApprovalStatus limit offset dl approval merchantShortId = do
  findAllWithOptionsKV
    [ Se.Or
        [ Se.And
            [ Se.Is Beam.dl $ Se.Eq dl,
              Se.Is Beam.adminApproval $ Se.Eq (Just approval)
            ],
          Se.And
            [ Se.Is Beam.dl $ Se.Eq dl,
              Se.Is Beam.adminApproval $ Se.Eq Nothing
            ],
          Se.And
            [ Se.Is Beam.dl $ Se.Eq dl,
              Se.Is Beam.merchantShortId $ Se.Eq merchantShortId
            ]
        ]
    ]
    (Se.Asc Beam.createdAt)
    limit
    offset

findAllByVoterIdAndAdminApprovalStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.SuspectFlagRequest.AdminApproval -> Maybe Text -> m [Domain.Types.SuspectStatusHistory.SuspectStatusHistory]
findAllByVoterIdAndAdminApprovalStatus limit offset voterId approval merchantShortId = do
  findAllWithOptionsKV
    [ Se.Or
        [ Se.And
            [ Se.Is Beam.voterId $ Se.Eq voterId,
              Se.Is Beam.adminApproval $ Se.Eq (Just approval)
            ],
          Se.And
            [ Se.Is Beam.voterId $ Se.Eq voterId,
              Se.Is Beam.adminApproval $ Se.Eq Nothing
            ],
          Se.And
            [ Se.Is Beam.voterId $ Se.Eq voterId,
              Se.Is Beam.merchantShortId $ Se.Eq merchantShortId
            ]
        ]
    ]
    (Se.Asc Beam.createdAt)
    limit
    offset
