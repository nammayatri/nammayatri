{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourney as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.IncentiveJourney ()
import qualified Sequelize as Se

findByIds :: (BeamFlow m r) => [Id DIJ.IncentiveJourney] -> m [DIJ.IncentiveJourney]
findByIds ids
  | null ids = pure []
  | otherwise =
    findAllWithKV
      [Se.Is Beam.id $ Se.In (map getId ids)]

findByJourneyType ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  DIJ.IncentiveJourneyType ->
  m [DIJ.IncentiveJourney]
findByJourneyType limit offset journeyType =
  findAllWithOptionsKV
    [Se.Is Beam.journeyType $ Se.Eq journeyType]
    (Se.Desc Beam.createdAt)
    limit
    offset
