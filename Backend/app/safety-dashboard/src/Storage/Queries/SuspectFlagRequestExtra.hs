{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SuspectFlagRequestExtra where

import qualified "lib-dashboard" Domain.Types.Merchant
import Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SuspectFlagRequest as Beam
import Storage.Queries.OrphanInstances.SuspectFlagRequest ()

findAllByDlAndVoterIdAndMerchantIdAndAdminApproval :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Kernel.Prelude.Text] -> [Kernel.Prelude.Text] -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Domain.Types.SuspectFlagRequest.AdminApproval -> m [Domain.Types.SuspectFlagRequest.SuspectFlagRequest]
findAllByDlAndVoterIdAndMerchantIdAndAdminApproval dls voterIds merchantId adminApproval = do
  let dlList = map Just dls
      voterIdList = map Just voterIds
  findAllWithKV
    [ Se.And
        [ Se.Or
            [ Se.Is Beam.dl $ Se.In dlList,
              Se.Is Beam.voterId $ Se.In voterIdList
            ],
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.adminApproval $ Se.Eq adminApproval
        ]
    ]

findAllPAByDlAndVoterIdAndMerchantId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Maybe Kernel.Prelude.Text] -> [Maybe Kernel.Prelude.Text] -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findAllPAByDlAndVoterIdAndMerchantId dls voterIds merchantId = do
  findAllWithKV
    [ Se.And
        [ Se.Or
            [ Se.Is Beam.dl $ Se.In dls,
              Se.Is Beam.voterId $ Se.In voterIds
            ],
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Pending
        ]
    ]

findAllApprovedByDlAndVoterIdAndMerchantId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Maybe Kernel.Prelude.Text] -> [Maybe Kernel.Prelude.Text] -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findAllApprovedByDlAndVoterIdAndMerchantId dls voterIds merchantId = do
  findAllWithKV
    [ Se.And
        [ Se.Or
            [ Se.Is Beam.dl $ Se.In dls,
              Se.Is Beam.voterId $ Se.In voterIds
            ],
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Approved
        ]
    ]

findByMerchantIdAndDl' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Maybe Kernel.Prelude.Text -> m (Maybe (Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findByMerchantIdAndDl' merchantId dl = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.dl $ Se.Eq dl,
          Se.Or
            [ Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Pending,
              Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Approved
            ]
        ]
    ]

findByMerchantIdAndVoterId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Maybe Kernel.Prelude.Text -> m (Maybe (Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findByMerchantIdAndVoterId merchantId voterId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.voterId $ Se.Eq voterId,
          Se.Or
            [ Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Pending,
              Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Approved
            ]
        ]
    ]

updateAllWIthDlAndFlaggedStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> FlaggedStatus -> m ()
updateAllWIthDlAndFlaggedStatus dl flaggedStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt now
    ]
    [ Se.Is Beam.dl $ Se.Eq (Just dl)
    ]

updateAllWithVoteIdAndFlaggedStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> FlaggedStatus -> m ()
updateAllWithVoteIdAndFlaggedStatus voterId flaggedStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt now
    ]
    [ Se.Is Beam.voterId $ Se.Eq (Just voterId)
    ]

updateManyAdminApprovalById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.SuspectFlagRequest.AdminApproval -> Text -> [Kernel.Types.Id.Id Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> m ()
updateManyAdminApprovalById adminApproval approvedBy ids = traverse_ (updateAdminApprovalById' adminApproval approvedBy) ids

updateAdminApprovalById' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.SuspectFlagRequest.AdminApproval -> Text -> Kernel.Types.Id.Id Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> m ()
updateAdminApprovalById' adminApproval approvedBy (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.adminApproval $ adminApproval,
      Se.Set Beam.approvedBy $ Just approvedBy,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

findAllPendingRequestByRequestId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Kernel.Types.Id.Id Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findAllPendingRequestByRequestId ids merchantId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> ids),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Pending
        ]
    ]

findByFlaggedCategoryAndPartnerNameAndFlaggedStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> FlaggedStatus -> Int -> Int -> UTCTime -> UTCTime -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findByFlaggedCategoryAndPartnerNameAndFlaggedStatus flaggedCategory partnerName flaggedStatus limit offset startDate endDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.flaggedCategory $ Se.Eq flaggedCategory,
          Se.Is Beam.merchantShortId $ Se.Eq $ Just partnerName,
          Se.Is Beam.flaggedStatus $ Se.Eq flaggedStatus,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findByFlaggedCategoryAndPartnerName :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> Int -> Int -> UTCTime -> UTCTime -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findByFlaggedCategoryAndPartnerName flaggedCategory partnerName limit offset startDate endDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.flaggedCategory $ Se.Eq flaggedCategory,
          Se.Is Beam.merchantShortId $ Se.Eq $ Just partnerName,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findByFlaggedCategoryAndFlaggedStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> FlaggedStatus -> Int -> Int -> UTCTime -> UTCTime -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findByFlaggedCategoryAndFlaggedStatus flaggedCategory flaggedStatus limit offset startDate endDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.flaggedCategory $ Se.Eq flaggedCategory,
          Se.Is Beam.flaggedStatus $ Se.Eq flaggedStatus,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findByFlaggedCategory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> Int -> UTCTime -> UTCTime -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findByFlaggedCategory flaggedCategory limit offset startDate endDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.flaggedCategory $ Se.Eq flaggedCategory,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findByPartnerNameAndFlaggedStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> FlaggedStatus -> Int -> Int -> UTCTime -> UTCTime -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findByPartnerNameAndFlaggedStatus partnerName flaggedStatus limit offset startDate endDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantShortId $ Se.Eq $ Just partnerName,
          Se.Is Beam.flaggedStatus $ Se.Eq flaggedStatus,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findByPartnerName :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Int -> Int -> UTCTime -> UTCTime -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findByPartnerName partnerName limit offset startDate endDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantShortId $ Se.Eq $ Just partnerName,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findByFlaggedStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => FlaggedStatus -> Int -> Int -> UTCTime -> UTCTime -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findByFlaggedStatus flaggedStatus limit offset startDate endDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.flaggedStatus $ Se.Eq flaggedStatus,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findByMerchantIdAndAdminApproval :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Int -> Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Domain.Types.SuspectFlagRequest.AdminApproval -> Maybe Text -> Maybe Text -> UTCTime -> UTCTime -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findByMerchantIdAndAdminApproval limit offset merchantId adminApproval mbDl mbVoterId from to = do
  case (mbDl, mbVoterId) of
    (Nothing, Nothing) -> do
      findAllWithOptionsKV
        [ Se.And
            [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
              Se.Is Beam.adminApproval $ Se.Eq adminApproval,
              Se.Is Beam.createdAt $ Se.GreaterThanOrEq from,
              Se.Is Beam.createdAt $ Se.LessThanOrEq to
            ]
        ]
        (Se.Desc Beam.createdAt)
        (Just limit)
        (Just offset)
    (Just dl, _) -> do
      findAllWithOptionsKV
        [ Se.And
            [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
              Se.Is Beam.dl $ Se.Eq (Just dl)
            ]
        ]
        (Se.Desc Beam.createdAt)
        (Just limit)
        (Just offset)
    (_, Just voterId) -> do
      findAllWithOptionsKV
        [ Se.And
            [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
              Se.Is Beam.voterId $ Se.Eq (Just voterId)
            ]
        ]
        (Se.Desc Beam.createdAt)
        (Just limit)
        (Just offset)
