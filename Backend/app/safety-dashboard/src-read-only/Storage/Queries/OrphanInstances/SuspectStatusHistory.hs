{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.SuspectStatusHistory where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.SuspectStatusHistory
import qualified Storage.Beam.SuspectStatusHistory as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.SuspectStatusHistory Domain.Types.SuspectStatusHistory.SuspectStatusHistory
    where fromTType' (Beam.SuspectStatusHistoryT {..}) = do pure $ Just Domain.Types.SuspectStatusHistory.SuspectStatusHistory{adminApproval = adminApproval,
                                                                                                                               approvedBy = approvedBy,
                                                                                                                               createdAt = createdAt,
                                                                                                                               dl = dl,
                                                                                                                               firstName = firstName,
                                                                                                                               flaggedBy = flaggedBy,
                                                                                                                               flaggedStatus = flaggedStatus,
                                                                                                                               id = Kernel.Types.Id.Id id,
                                                                                                                               lastName = lastName,
                                                                                                                               merchantShortId = merchantShortId,
                                                                                                                               statusChangedReason = statusChangedReason,
                                                                                                                               updatedAt = updatedAt,
                                                                                                                               voterId = voterId,
                                                                                                                               merchantId = Kernel.Types.Id.Id <$> merchantId}
instance ToTType' Beam.SuspectStatusHistory Domain.Types.SuspectStatusHistory.SuspectStatusHistory
    where toTType' (Domain.Types.SuspectStatusHistory.SuspectStatusHistory {..}) = do Beam.SuspectStatusHistoryT{Beam.adminApproval = adminApproval,
                                                                                                                 Beam.approvedBy = approvedBy,
                                                                                                                 Beam.createdAt = createdAt,
                                                                                                                 Beam.dl = dl,
                                                                                                                 Beam.firstName = firstName,
                                                                                                                 Beam.flaggedBy = flaggedBy,
                                                                                                                 Beam.flaggedStatus = flaggedStatus,
                                                                                                                 Beam.id = Kernel.Types.Id.getId id,
                                                                                                                 Beam.lastName = lastName,
                                                                                                                 Beam.merchantShortId = merchantShortId,
                                                                                                                 Beam.statusChangedReason = statusChangedReason,
                                                                                                                 Beam.updatedAt = updatedAt,
                                                                                                                 Beam.voterId = voterId,
                                                                                                                 Beam.merchantId = Kernel.Types.Id.getId <$> merchantId}



