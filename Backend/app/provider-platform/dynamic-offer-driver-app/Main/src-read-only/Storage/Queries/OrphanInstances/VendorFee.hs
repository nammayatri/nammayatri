{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.VendorFee where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.VendorFee
import qualified Storage.Beam.VendorFee as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.VendorFee Domain.Types.VendorFee.VendorFee
    where fromTType' (Beam.VendorFeeT {..}) = do pure $ Just Domain.Types.VendorFee.VendorFee{amount = amount,
                                                                                              driverFeeId = Kernel.Types.Id.Id driverFeeId,
                                                                                              vendorId = vendorId,
                                                                                              createdAt = createdAt,
                                                                                              updatedAt = updatedAt}
instance ToTType' Beam.VendorFee Domain.Types.VendorFee.VendorFee
    where toTType' (Domain.Types.VendorFee.VendorFee {..}) = do Beam.VendorFeeT{Beam.amount = amount,
                                                                                Beam.driverFeeId = Kernel.Types.Id.getId driverFeeId,
                                                                                Beam.vendorId = vendorId,
                                                                                Beam.createdAt = createdAt,
                                                                                Beam.updatedAt = updatedAt}



