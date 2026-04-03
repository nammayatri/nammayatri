{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.DriverSSN where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.DriverSSN
import qualified Storage.Beam.DriverSSN as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.DriverSSN Domain.Types.DriverSSN.DriverSSN
    where fromTType' (Beam.DriverSSNT {..}) = do pure $ Just Domain.Types.DriverSSN.DriverSSN{driverId = Kernel.Types.Id.Id driverId,
                                                                                              id = Kernel.Types.Id.Id id,
                                                                                              rejectReason = rejectReason,
                                                                                              ssn = EncryptedHashed (Encrypted ssnEncrypted) ssnHash,
                                                                                              verificationStatus = verificationStatus}
instance ToTType' Beam.DriverSSN Domain.Types.DriverSSN.DriverSSN
    where toTType' (Domain.Types.DriverSSN.DriverSSN {..}) = do Beam.DriverSSNT{Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                Beam.id = Kernel.Types.Id.getId id,
                                                                                Beam.rejectReason = rejectReason,
                                                                                Beam.ssnEncrypted = ((ssn & unEncrypted . encrypted)),
                                                                                Beam.ssnHash = (ssn & hash),
                                                                                Beam.verificationStatus = verificationStatus}



