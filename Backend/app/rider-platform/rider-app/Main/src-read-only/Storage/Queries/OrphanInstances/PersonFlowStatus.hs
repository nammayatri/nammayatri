{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PersonFlowStatus where

import qualified Domain.Types.PersonFlowStatus
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PersonFlowStatus as Beam

instance FromTType' Beam.PersonFlowStatus Domain.Types.PersonFlowStatus.PersonFlowStatus where
  fromTType' (Beam.PersonFlowStatusT {..}) = do pure $ Just Domain.Types.PersonFlowStatus.PersonFlowStatus {flowStatus = flowStatus, personId = Kernel.Types.Id.Id personId, updatedAt = updatedAt}

instance ToTType' Beam.PersonFlowStatus Domain.Types.PersonFlowStatus.PersonFlowStatus where
  toTType' (Domain.Types.PersonFlowStatus.PersonFlowStatus {..}) = do
    Beam.PersonFlowStatusT
      { Beam.flowStatus = flowStatus,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.updatedAt = updatedAt
      }
