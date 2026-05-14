{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MerchantDocuments.Storage.Beam.MerchantDocument where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified MerchantDocuments.Domain.Types.MerchantDocument

data MerchantDocumentT f = MerchantDocumentT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    documentType :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    language :: B.C f Kernel.External.Types.Language,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    platformType :: B.C f (Kernel.Prelude.Maybe MerchantDocuments.Domain.Types.MerchantDocument.PlatformType),
    role :: B.C f MerchantDocuments.Domain.Types.MerchantDocument.Role,
    title :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    url :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantDocumentT where
  data PrimaryKey MerchantDocumentT f = MerchantDocumentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantDocumentId . id

type MerchantDocument = MerchantDocumentT Identity

$(enableKVPG ''MerchantDocumentT ['id] [['documentType], ['role]])

$(mkTableInstancesGenericSchema ''MerchantDocumentT "merchant_document")
