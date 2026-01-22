{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Merchant where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.AccessMatrix
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Tools.Beam.UtilsTH

data MerchantT f = MerchantT
  { authTokenEncrypted :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    authTokenHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    defaultOperatingCity :: (B.C f Kernel.Types.Beckn.Context.City),
    domain :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    enabled :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    hasFleetMemberHierarchy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    id :: (B.C f Data.Text.Text),
    is2faMandatory :: (B.C f Kernel.Prelude.Bool),
    isStrongNameCheckRequired :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    requireAdminApprovalForFleetOnboarding :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    serverNames :: (B.C f [Domain.Types.AccessMatrix.ServerName]),
    shortId :: (B.C f Data.Text.Text),
    singleActiveSessionOnly :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    supportedOperatingCities :: (B.C f [Kernel.Types.Beckn.Context.City]),
    verifyFleetWhileLogin :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    website :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantT where
  data PrimaryKey MerchantT f = MerchantId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantId . id

type Merchant = MerchantT Identity

$(enableKVPG (''MerchantT) [('id)] [[('shortId)]])

$(mkTableInstances (''MerchantT) "merchant")
