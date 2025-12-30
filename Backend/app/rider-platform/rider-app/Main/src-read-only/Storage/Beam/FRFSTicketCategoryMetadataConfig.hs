{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicketCategoryMetadataConfig where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSQuoteCategorySpec
import qualified Domain.Types.FRFSQuoteCategoryType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSTicketCategoryMetadataConfigT f = FRFSTicketCategoryMetadataConfigT
  { category :: B.C f Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType,
    categoryOrder :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    code :: B.C f Kernel.Prelude.Text,
    description :: B.C f Kernel.Prelude.Text,
    domainCategoryValue :: B.C f Domain.Types.FRFSQuoteCategorySpec.OfferedValue,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    title :: B.C f Kernel.Prelude.Text,
    tnc :: B.C f Kernel.Prelude.Text,
    vehicleCategory :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketCategoryMetadataConfigT where
  data PrimaryKey FRFSTicketCategoryMetadataConfigT f = FRFSTicketCategoryMetadataConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketCategoryMetadataConfigId . id

type FRFSTicketCategoryMetadataConfig = FRFSTicketCategoryMetadataConfigT Identity

$(enableKVPG ''FRFSTicketCategoryMetadataConfigT ['id] [])

$(mkTableInstances ''FRFSTicketCategoryMetadataConfigT "frfs_ticket_category_metadata_config")
