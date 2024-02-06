{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AutoCompleteData where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data AutoCompleteDataT f = AutoCompleteDataT
  { autocompleteInputs :: B.C f Kernel.Prelude.Text,
    customerId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    isLocationSelectedOnMap :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    searchRequestId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    searchType :: B.C f Kernel.Prelude.Text,
    sessionToken :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AutoCompleteDataT where
  data PrimaryKey AutoCompleteDataT f = AutoCompleteDataId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = AutoCompleteDataId . id

type AutoCompleteData = AutoCompleteDataT Identity

$(enableKVPG ''AutoCompleteDataT ['id] [])

$(mkTableInstances ''AutoCompleteDataT "auto_complete_data")
