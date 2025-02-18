{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Overlay where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.UtilsTH
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data OverlayT f = OverlayT
  { actions :: B.C f [Kernel.Prelude.Text],
    actions2 :: B.C f Data.Aeson.Value,
    cancelButtonText :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    contactSupportNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    delay :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    endPoint :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    imageUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    language :: B.C f Kernel.External.Types.Language,
    link :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    method :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    okButtonText :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    overlayKey :: B.C f Kernel.Prelude.Text,
    reqBody :: B.C f Data.Aeson.Value,
    secondaryActions :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    secondaryActions2 :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    showPushNotification :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    socialMediaLinks :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    title :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toastMessage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    udf1 :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory)
  }
  deriving (Generic, B.Beamable)

instance B.Table OverlayT where
  data PrimaryKey OverlayT f = OverlayId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OverlayId . id

type Overlay = OverlayT Identity

$(enableKVPG ''OverlayT ['id] [['overlayKey]])

$(mkTableInstances ''OverlayT "merchant_overlay")

$(Domain.Types.UtilsTH.mkCacParseInstance ''OverlayT)
