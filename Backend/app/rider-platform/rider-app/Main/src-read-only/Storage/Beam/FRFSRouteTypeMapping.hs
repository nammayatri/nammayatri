{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSRouteTypeMapping where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSRouteTypeMappingT f = FRFSRouteTypeMappingT
  { integratedBppConfigId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    routeCode :: (B.C f Kernel.Prelude.Text),
    routeType :: (B.C f Kernel.Prelude.Text),
    vehicleServiceTierId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSRouteTypeMappingT where
  data PrimaryKey FRFSRouteTypeMappingT f = FRFSRouteTypeMappingId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSRouteTypeMappingId <$> integratedBppConfigId <*> routeCode <*> vehicleServiceTierId

type FRFSRouteTypeMapping = FRFSRouteTypeMappingT Identity

$(enableKVPG (''FRFSRouteTypeMappingT) [('integratedBppConfigId), ('routeCode), ('vehicleServiceTierId)] [[('integratedBppConfigId), ('routeCode)]])

$(mkTableInstances (''FRFSRouteTypeMappingT) "frfs_route_type_mapping")
