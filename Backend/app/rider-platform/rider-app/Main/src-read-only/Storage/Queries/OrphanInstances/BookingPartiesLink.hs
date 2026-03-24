{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.BookingPartiesLink where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.BookingPartiesLink
import qualified Storage.Beam.BookingPartiesLink as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.BookingPartiesLink Domain.Types.BookingPartiesLink.BookingPartiesLink
    where fromTType' (Beam.BookingPartiesLinkT {..}) = do pure $ Just Domain.Types.BookingPartiesLink.BookingPartiesLink{bookingId = Kernel.Types.Id.Id bookingId,
                                                                                                                         id = Kernel.Types.Id.Id id,
                                                                                                                         isActive = isActive,
                                                                                                                         partyId = Kernel.Types.Id.Id partyId,
                                                                                                                         partyName = partyName,
                                                                                                                         partyType = partyType,
                                                                                                                         createdAt = createdAt,
                                                                                                                         updatedAt = updatedAt}
instance ToTType' Beam.BookingPartiesLink Domain.Types.BookingPartiesLink.BookingPartiesLink
    where toTType' (Domain.Types.BookingPartiesLink.BookingPartiesLink {..}) = do Beam.BookingPartiesLinkT{Beam.bookingId = Kernel.Types.Id.getId bookingId,
                                                                                                           Beam.id = Kernel.Types.Id.getId id,
                                                                                                           Beam.isActive = isActive,
                                                                                                           Beam.partyId = Kernel.Types.Id.getId partyId,
                                                                                                           Beam.partyName = partyName,
                                                                                                           Beam.partyType = partyType,
                                                                                                           Beam.createdAt = createdAt,
                                                                                                           Beam.updatedAt = updatedAt}



