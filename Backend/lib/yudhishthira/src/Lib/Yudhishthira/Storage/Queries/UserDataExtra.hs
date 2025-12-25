{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.Queries.UserDataExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.UserData as Beam
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.UserData ()
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.UserData
import qualified Sequelize as Se

-- Extra code goes here --

deleteUserDataWithEventId :: (BeamFlow.BeamFlow m r) => Id Lib.Yudhishthira.Types.Event -> m ()
deleteUserDataWithEventId eventId = deleteWithKV [Se.And [Se.Is Beam.eventId $ Se.Eq (Kernel.Types.Id.getId eventId)]]

findAllByEventIdWithLimitOffset :: (BeamFlow.BeamFlow m r) => Id Lib.Yudhishthira.Types.Event -> Maybe Int -> Maybe Int -> m ([Lib.Yudhishthira.Types.UserData.UserData])
findAllByEventIdWithLimitOffset eventId limit offset = do
  findAllWithOptionsKV
    [Se.And [Se.Is Beam.eventId $ Se.Eq (Kernel.Types.Id.getId eventId)]]
    (Se.Asc Beam.userId)
    limit
    offset
