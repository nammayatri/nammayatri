{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FareBreakup where

import qualified Domain.Types.FareBreakup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FareBreakup as Beam

instance FromTType' Beam.FareBreakup Domain.Types.FareBreakup.FareBreakup where
  fromTType' (Beam.FareBreakupT {..}) = do
    pure $
      Just
        Domain.Types.FareBreakup.FareBreakup
          { id = Kernel.Types.Id.Id id,
            description = description,
            amount = Kernel.Types.Common.mkPrice currency amount,
            entityType = entityType,
            entityId = bookingId
          }

instance ToTType' Beam.FareBreakup Domain.Types.FareBreakup.FareBreakup where
  toTType' (Domain.Types.FareBreakup.FareBreakup {..}) = do
    Beam.FareBreakupT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.description = description,
        Beam.amount = (.amount) amount,
        Beam.currency = Just $ (.currency) amount,
        Beam.entityType = entityType,
        Beam.bookingId = entityId
      }
