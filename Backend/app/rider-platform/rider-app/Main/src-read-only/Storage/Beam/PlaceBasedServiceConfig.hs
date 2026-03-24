{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PlaceBasedServiceConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.MerchantServiceConfig
import qualified Kernel.Prelude
import qualified Data.Aeson
import qualified Domain.Types.UtilsTH
import qualified Database.Beam as B



data PlaceBasedServiceConfigT f
    = PlaceBasedServiceConfigT {merchantId :: (B.C f Kernel.Prelude.Text),
                                merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                placeId :: (B.C f Kernel.Prelude.Text),
                                configValue :: (B.C f Data.Aeson.Value),
                                serviceName :: (B.C f Domain.Types.MerchantServiceConfig.ServiceName),
                                createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PlaceBasedServiceConfigT
    where data PrimaryKey PlaceBasedServiceConfigT f = PlaceBasedServiceConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PlaceBasedServiceConfigId . placeId
type PlaceBasedServiceConfig = PlaceBasedServiceConfigT Identity

$(enableKVPG (''PlaceBasedServiceConfigT) [('placeId)] [])

$(mkTableInstances (''PlaceBasedServiceConfigT) "place_based_service_config")

$(Domain.Types.UtilsTH.mkCacParseInstance (''PlaceBasedServiceConfigT))

