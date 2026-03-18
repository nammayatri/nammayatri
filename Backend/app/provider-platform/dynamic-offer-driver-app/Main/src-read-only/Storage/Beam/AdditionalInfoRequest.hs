{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AdditionalInfoRequest where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.AdditionalInfoRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AdditionalInfoRequestT f = AdditionalInfoRequestT
  { id :: B.C f Kernel.Prelude.Text,
    operationHubRequestId :: B.C f Kernel.Prelude.Text,
    requestedBy :: B.C f Kernel.Prelude.Text,
    requestedFrom :: B.C f Kernel.Prelude.Text,
    requestedDocumentTypes :: B.C f [Domain.Types.AdditionalInfoRequest.DocumentType],
    message :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.AdditionalInfoRequest.AdditionalInfoStatus,
    responseRemarks :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    responseDocumentIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AdditionalInfoRequestT where
  data PrimaryKey AdditionalInfoRequestT f = AdditionalInfoRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AdditionalInfoRequestId . id

type AdditionalInfoRequest = AdditionalInfoRequestT Identity

$(enableKVPG ''AdditionalInfoRequestT ['id] [])

$(mkTableInstances ''AdditionalInfoRequestT "additional_info_request")
