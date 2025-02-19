{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyExtra where

import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Journey as Beam
import Storage.Queries.OrphanInstances.Journey

-- Extra code goes here --
findAllActiveByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> m [DJ.Journey]
findAllActiveByRiderId riderId = do findAllWithKV [Se.And [Se.Is Beam.status $ Se.In [Just DJ.CONFIRMED, Just DJ.INPROGRESS, Just DJ.FEEDBACK_PENDING], Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]]

findAllByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> Maybe Integer -> Maybe Integer -> Maybe UTCTime -> Maybe UTCTime -> [DJ.JourneyStatus] -> m [DJ.Journey]
findAllByRiderId (Kernel.Types.Id.Id personId) mbLimit mbOffset mbFromDate mbToDate mbJourneyStatusList = do
  let limit' = maybe 10 fromIntegral mbLimit
  let offset' = maybe 0 fromIntegral mbOffset
      mbJourneyStatus = Just <$> mbJourneyStatusList
  journeys <-
    findAllWithOptionsKV
      [ Se.And
          ( [Se.Is Beam.riderId $ Se.Eq personId]
              <> ([Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromJust mbFromDate) | isJust mbFromDate])
              <> ([Se.Is Beam.createdAt $ Se.LessThanOrEq (fromJust mbToDate) | isJust mbToDate])
              <> ([Se.Is Beam.status $ Se.Not $ Se.In [Just DJ.NEW, Just DJ.INITIATED]])
              <> ([Se.Is Beam.status $ Se.In mbJourneyStatus | not (null mbJourneyStatus)])
          )
      ]
      (Se.Desc Beam.startTime)
      (Just limit')
      (Just offset')
  pure journeys
