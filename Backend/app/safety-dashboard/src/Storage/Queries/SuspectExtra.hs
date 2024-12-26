{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SuspectExtra where

import qualified Domain.Types.Suspect
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Suspect as Beam
import Storage.Queries.OrphanInstances.Suspect ()
import Storage.Queries.Transformers.Suspect

-- Extra code goes here --

findAllByDlOrVoterId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Kernel.Prelude.Text] -> [Kernel.Prelude.Text] -> m [Domain.Types.Suspect.Suspect]
findAllByDlOrVoterId dls voterIds = do
  let dlList = map Just dls
      voterIdList = map Just voterIds
  findAllWithKV
    [ Se.Or
        [ Se.Is Beam.dl $ Se.In dlList,
          Se.Is Beam.voterId $ Se.In voterIdList
        ]
    ]

findByDlInRange :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Maybe Text -> m (Maybe Domain.Types.Suspect.Suspect)
findByDlInRange startDate endDate dl = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.dl $ Se.Eq dl,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]

updateFlaggedCounterByKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Domain.Types.Suspect.FlaggedStatus -> [Domain.Types.Suspect.FlaggedBy] -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
updateFlaggedCounterByKey flaggedCounter flaggedStatus flaggedBy key = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedCounter flaggedCounter,
      Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt now,
      Se.Set Beam.flaggedBy (convertFlaggedByToTable flaggedBy)
    ]
    [Se.Or [Se.Is Beam.voterId $ Se.Eq key, Se.Is Beam.dl $ Se.Eq key]]

findByVoterIdInRange :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Maybe Text -> m (Maybe Domain.Types.Suspect.Suspect)
findByVoterIdInRange startDate endDate voterId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.voterId $ Se.Eq voterId,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]

findAllByDlOrVoterIdAndFlaggedStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Kernel.Prelude.Text] -> [Kernel.Prelude.Text] -> Domain.Types.Suspect.FlaggedStatus -> m [Domain.Types.Suspect.Suspect]
findAllByDlOrVoterIdAndFlaggedStatus dls voterIds flaggedStatus = do
  let dlList = map Just dls
      voterIdList = map Just voterIds
  findAllWithKV
    [ Se.And
        [ Se.Or
            [ Se.Is Beam.dl $ Se.In dlList,
              Se.Is Beam.voterId $ Se.In voterIdList
            ],
          Se.Is Beam.flaggedStatus $ Se.Eq flaggedStatus
        ]
    ]

findAll :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Int -> Int -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> m [Domain.Types.Suspect.Suspect]
findAll limit offset startDate endDate = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Not $ Se.Eq "",
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.createdAt $ Se.LessThanOrEq endDate
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findAllByIds :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Kernel.Types.Id.Id Domain.Types.Suspect.Suspect] -> m [Domain.Types.Suspect.Suspect]
findAllByIds ids = do
  let idList = map Kernel.Types.Id.getId ids
  findAllWithKV
    [Se.Is Beam.id $ Se.In idList]
