{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PickedServices where

import qualified Database.Beam as B
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PickedServices
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data PickedServicesT f = PickedServicesT
  { autoComplete :: B.C f Kernel.External.Maps.Types.MapsService,
    getDistances :: B.C f Kernel.External.Maps.Types.MapsService,
    getDistancesForCancelRide :: B.C f Kernel.External.Maps.Types.MapsService,
    getPickupRoutes :: B.C f Kernel.External.Maps.Types.MapsService,
    getPlaceDetails :: B.C f Kernel.External.Maps.Types.MapsService,
    getPlaceName :: B.C f Kernel.External.Maps.Types.MapsService,
    getRoutes :: B.C f Kernel.External.Maps.Types.MapsService,
    getTripRoutes :: B.C f Kernel.External.Maps.Types.MapsService,
    searchRequestId :: B.C f Kernel.Prelude.Text,
    snapToRoad :: B.C f Kernel.External.Maps.Types.MapsService,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PickedServicesT where
  data PrimaryKey PickedServicesT f = PickedServicesId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = PickedServicesId . searchRequestId

type PickedServices = PickedServicesT Identity

$(enableKVPG ''PickedServicesT ['searchRequestId] [])

$(mkTableInstances ''PickedServicesT "picked_services")
