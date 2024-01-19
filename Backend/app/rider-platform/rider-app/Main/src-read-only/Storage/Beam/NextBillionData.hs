{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.NextBillionData where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SearchRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data NextBillionDataT f = NextBillionDataT
  { routes :: B.C f [Kernel.Prelude.Text],
    searchRequestId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table NextBillionDataT where
  data PrimaryKey NextBillionDataT f = NextBillionDataId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = NextBillionDataId . searchRequestId

type NextBillionData = NextBillionDataT Identity

$(enableKVPG ''NextBillionDataT ['searchRequestId] [])

$(mkTableInstances ''NextBillionDataT "next_billion_data")
