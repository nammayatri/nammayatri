{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FareBreakup where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FareBreakup
import qualified Storage.Beam.FareBreakup as Beam
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id



instance FromTType' Beam.FareBreakup Domain.Types.FareBreakup.FareBreakup
    where fromTType' (Beam.FareBreakupT {..}) = do pure $ Just Domain.Types.FareBreakup.FareBreakup{amount = Kernel.Types.Common.mkPrice currency amount,
                                                                                                    description = description,
                                                                                                    entityId = bookingId,
                                                                                                    entityType = entityType,
                                                                                                    id = Kernel.Types.Id.Id id}
instance ToTType' Beam.FareBreakup Domain.Types.FareBreakup.FareBreakup
    where toTType' (Domain.Types.FareBreakup.FareBreakup {..}) = do Beam.FareBreakupT{Beam.amount = ((.amount) amount),
                                                                                      Beam.currency = (Just $ (.currency) amount),
                                                                                      Beam.description = description,
                                                                                      Beam.bookingId = entityId,
                                                                                      Beam.entityType = entityType,
                                                                                      Beam.id = Kernel.Types.Id.getId id}



