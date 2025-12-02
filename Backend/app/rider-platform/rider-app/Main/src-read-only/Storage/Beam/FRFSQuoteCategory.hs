{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSQuoteCategory where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSQuoteCategoryType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSQuoteCategoryT f = FRFSQuoteCategoryT
  { bppItemId :: B.C f Kernel.Prelude.Text,
    category :: B.C f (Kernel.Prelude.Maybe Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType),
    code :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    title :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    tnc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    finalPrice :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    offeredPrice :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    price :: B.C f Kernel.Types.Common.HighPrecMoney,
    quoteId :: B.C f Kernel.Prelude.Text,
    selectedQuantity :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSQuoteCategoryT where
  data PrimaryKey FRFSQuoteCategoryT f = FRFSQuoteCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSQuoteCategoryId . id

type FRFSQuoteCategory = FRFSQuoteCategoryT Identity

$(enableKVPG ''FRFSQuoteCategoryT ['id] [['quoteId]])

$(mkTableInstances ''FRFSQuoteCategoryT "frfs_quote_category")
