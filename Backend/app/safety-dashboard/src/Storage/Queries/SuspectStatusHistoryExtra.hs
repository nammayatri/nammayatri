{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SuspectStatusHistoryExtra where

import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusHistory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SuspectStatusHistory as Beam
import Storage.Queries.OrphanInstances.SuspectStatusHistory

-- Extra code goes here --

findAllByDlAndNotAdminApprovalStatus :: KvDbFlow m r => Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.SuspectFlagRequest.AdminApproval -> m ([Domain.Types.SuspectStatusHistory.SuspectStatusHistory])
findAllByDlAndNotAdminApprovalStatus limit offset dl approval = do
  findAllWithOptionsKV
    [ Se.Or
        [ Se.And
            [ Se.Is Beam.dl $ Se.Eq dl,
              Se.Is Beam.adminApproval $ Se.Not $ Se.Eq (Just approval)
            ],
          Se.And
            [ Se.Is Beam.dl $ Se.Eq dl,
              Se.Is Beam.adminApproval $ Se.Eq Nothing
            ]
        ]
    ]
    (Se.Asc Beam.createdAt)
    limit
    offset

findAllByVoterIdAndNotAdminApprovalStatus :: KvDbFlow m r => Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.SuspectFlagRequest.AdminApproval -> m ([Domain.Types.SuspectStatusHistory.SuspectStatusHistory])
findAllByVoterIdAndNotAdminApprovalStatus limit offset voterId approval = do
  findAllWithOptionsKV
    [ Se.Or
        [ Se.And
            [ Se.Is Beam.voterId $ Se.Eq voterId,
              Se.Is Beam.adminApproval $ Se.Not $ Se.Eq (Just approval)
            ],
          Se.And
            [ Se.Is Beam.voterId $ Se.Eq voterId,
              Se.Is Beam.adminApproval $ Se.Eq Nothing
            ]
        ]
    ]
    (Se.Asc Beam.createdAt)
    limit
    offset

findAllByVoterIdAndMerchantIdAndNotFlagStatus :: KvDbFlow m r => Text -> Text -> Domain.Types.Suspect.FlaggedStatus -> m ([Domain.Types.SuspectStatusHistory.SuspectStatusHistory])
findAllByVoterIdAndMerchantIdAndNotFlagStatus voterId merchantShortId flaggedStatus = do
  findAllWithOptionsKV
    [ Se.Or
        [ Se.And
            [ Se.Is Beam.flaggedStatus $ Se.Not $ Se.Eq flaggedStatus,
              Se.Is Beam.voterId $ Se.Eq (Just voterId)
            ],
          Se.And
            [ Se.Is Beam.merchantShortId $ Se.Eq (Just merchantShortId),
              Se.Is Beam.voterId $ Se.Eq (Just voterId)
            ]
        ]
    ]
    (Se.Asc Beam.createdAt)
    Nothing
    Nothing

findAllByDlAndMerchantIdAndNotFlagStatus :: KvDbFlow m r => Text -> Text -> Domain.Types.Suspect.FlaggedStatus -> m ([Domain.Types.SuspectStatusHistory.SuspectStatusHistory])
findAllByDlAndMerchantIdAndNotFlagStatus dl merchantShortId flaggedStatus = do
  findAllWithOptionsKV
    [ Se.Or
        [ Se.And
            [ Se.Is Beam.flaggedStatus $ Se.Not $ Se.Eq flaggedStatus,
              Se.Is Beam.dl $ Se.Eq (Just dl)
            ],
          Se.And
            [ Se.Is Beam.merchantShortId $ Se.Eq (Just merchantShortId),
              Se.Is Beam.dl $ Se.Eq (Just dl)
            ]
        ]
    ]
    (Se.Asc Beam.createdAt)
    Nothing
    Nothing
