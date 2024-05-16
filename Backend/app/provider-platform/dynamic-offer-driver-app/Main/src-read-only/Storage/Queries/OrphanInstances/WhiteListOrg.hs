{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.WhiteListOrg where

import qualified Domain.Types.WhiteListOrg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.WhiteListOrg as Beam

instance FromTType' Beam.WhiteListOrg Domain.Types.WhiteListOrg.WhiteListOrg where
  fromTType' (Beam.WhiteListOrgT {..}) = do
    pure $
      Just
        Domain.Types.WhiteListOrg.WhiteListOrg
          { domain = domain,
            id = Kernel.Types.Id.Id id,
            subscriberId = Kernel.Types.Id.ShortId subscriberId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.WhiteListOrg Domain.Types.WhiteListOrg.WhiteListOrg where
  toTType' (Domain.Types.WhiteListOrg.WhiteListOrg {..}) = do
    Beam.WhiteListOrgT
      { Beam.domain = domain,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.subscriberId = Kernel.Types.Id.getShortId subscriberId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
