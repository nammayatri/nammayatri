{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.BBPS where

import qualified Data.Aeson
import qualified Domain.Types.BBPS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.BBPS as Beam

instance FromTType' Beam.BBPS Domain.Types.BBPS.BBPS where
  fromTType' (Beam.BBPST {..}) = do
    pure $
      Just
        Domain.Types.BBPS.BBPS
          { amount = amount,
            billerId = billerId,
            customerId = Kernel.Types.Id.Id customerId,
            customerMobileNumber = customerMobileNumber,
            customerParams = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< customerParams,
            errorMessage = errorMessage,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            paymentInformation = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< paymentInformation,
            paymentMode = paymentMode,
            paymentTxnId = paymentTxnId,
            refId = Kernel.Types.Id.Id refId,
            refShortId = Kernel.Types.Id.ShortId refShortId,
            status = status,
            transType = transType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BBPS Domain.Types.BBPS.BBPS where
  toTType' (Domain.Types.BBPS.BBPS {..}) = do
    Beam.BBPST
      { Beam.amount = amount,
        Beam.billerId = billerId,
        Beam.customerId = Kernel.Types.Id.getId customerId,
        Beam.customerMobileNumber = customerMobileNumber,
        Beam.customerParams = Kernel.Prelude.toJSON <$> customerParams,
        Beam.errorMessage = errorMessage,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paymentInformation = Kernel.Prelude.toJSON <$> paymentInformation,
        Beam.paymentMode = paymentMode,
        Beam.paymentTxnId = paymentTxnId,
        Beam.refId = Kernel.Types.Id.getId refId,
        Beam.refShortId = Kernel.Types.Id.getShortId refShortId,
        Beam.status = status,
        Beam.transType = transType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
