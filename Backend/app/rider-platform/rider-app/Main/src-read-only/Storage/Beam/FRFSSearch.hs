{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSSearch where

import qualified BecknV2.FRFS.Enums
import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSSearchT f = FRFSSearchT
  { busLocationData :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    fromStationAddress :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStationId :: B.C f Kernel.Prelude.Text,
    fromStationName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    fromStationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    id :: B.C f Kernel.Prelude.Text,
    integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    isOnSearchReceived :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    multimodalSearchRequestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    onSearchFailed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    partnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    partnerOrgTransactionId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    quantity :: B.C f Kernel.Prelude.Int,
    recentLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    riderId :: B.C f Kernel.Prelude.Text,
    routeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    searchAsParentStops :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    toStationAddress :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStationId :: B.C f Kernel.Prelude.Text,
    toStationName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    toStationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    validTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    vehicleNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSSearchT where
  data PrimaryKey FRFSSearchT f = FRFSSearchId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSSearchId . id

type FRFSSearch = FRFSSearchT Identity

$(enableKVPG ''FRFSSearchT ['id] [['riderId]])

$(mkTableInstances ''FRFSSearchT "frfs_search")
