{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MessageReport where

import qualified Data.Aeson
import qualified Data.Time
import qualified Domain.Types.MessageReport
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MessageReport as Beam
import Storage.Queries.Transformers.MessageReport

instance FromTType' Beam.MessageReport Domain.Types.MessageReport.MessageReport where
  fromTType' (Beam.MessageReportT {..}) = do
    pure $
      Just
        Domain.Types.MessageReport.MessageReport
          { createdAt = Data.Time.localTimeToUTC Data.Time.utc createdAt,
            deliveryStatus = deliveryStatus,
            driverId = Kernel.Types.Id.Id driverId,
            likeStatus = likeStatus,
            messageDynamicFields = getMessageDynamicFields messageDynamicFields,
            messageId = Kernel.Types.Id.Id messageId,
            readStatus = readStatus,
            reply = reply,
            updatedAt = Data.Time.localTimeToUTC Data.Time.utc updatedAt
          }

instance ToTType' Beam.MessageReport Domain.Types.MessageReport.MessageReport where
  toTType' (Domain.Types.MessageReport.MessageReport {..}) = do
    Beam.MessageReportT
      { Beam.createdAt = Data.Time.utcToLocalTime Data.Time.utc createdAt,
        Beam.deliveryStatus = deliveryStatus,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.likeStatus = likeStatus,
        Beam.messageDynamicFields = Data.Aeson.toJSON messageDynamicFields,
        Beam.messageId = Kernel.Types.Id.getId messageId,
        Beam.readStatus = readStatus,
        Beam.reply = reply,
        Beam.updatedAt = Data.Time.utcToLocalTime Data.Time.utc updatedAt
      }
