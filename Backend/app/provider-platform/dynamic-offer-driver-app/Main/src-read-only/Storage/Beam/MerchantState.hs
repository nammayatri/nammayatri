{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantState where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Tools.Beam.UtilsTH

data MerchantStateT f = MerchantStateT
  { allowedDestinationStates :: B.C f [Kernel.Types.Beckn.Context.IndianState],
    merchantId :: B.C f Kernel.Prelude.Text,
    state :: B.C f Kernel.Types.Beckn.Context.IndianState,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantStateT where
  data PrimaryKey MerchantStateT f = MerchantStateId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Types.Beckn.Context.IndianState) deriving (Generic, B.Beamable)
  primaryKey = MerchantStateId <$> merchantId <*> state

type MerchantState = MerchantStateT Identity

$(enableKVPG ''MerchantStateT ['merchantId, 'state] [])

$(mkTableInstances ''MerchantStateT "merchant_state")
