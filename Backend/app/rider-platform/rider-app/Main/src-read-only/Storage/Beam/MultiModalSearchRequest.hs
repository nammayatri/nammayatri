{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MultiModalSearchRequest where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data MultiModalSearchRequestT f = MultiModalSearchRequestT
  { backendAppVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    backendConfigVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientBundleVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientConfigVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientManufacturer :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientModelName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientOsType :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType)),
    clientOsVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    clientReactNativeVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientSdkVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    configInExperimentVersions :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    isDashboardRequest :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    recentLocationId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    riderId :: (B.C f Kernel.Prelude.Text),
    startTime :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MultiModalSearchRequestT where
  data PrimaryKey MultiModalSearchRequestT f = MultiModalSearchRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MultiModalSearchRequestId . id

type MultiModalSearchRequest = MultiModalSearchRequestT Identity

$(enableKVPG (''MultiModalSearchRequestT) [('id)] [[('riderId)]])

$(mkTableInstances (''MultiModalSearchRequestT) "multimodal_search_request")
