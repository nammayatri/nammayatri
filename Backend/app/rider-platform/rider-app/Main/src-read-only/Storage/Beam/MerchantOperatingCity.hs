{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MerchantOperatingCity where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Database.Beam as B



data MerchantOperatingCityT f
    = MerchantOperatingCityT {city :: (B.C f Kernel.Types.Beckn.Context.City),
                              country :: (B.C f Kernel.Types.Beckn.Context.Country),
                              distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
                              driverOfferMerchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              id :: (B.C f Kernel.Prelude.Text),
                              lat :: (B.C f Kernel.Prelude.Double),
                              long :: (B.C f Kernel.Prelude.Double),
                              merchantId :: (B.C f Kernel.Prelude.Text),
                              merchantShortId :: (B.C f Kernel.Prelude.Text),
                              state :: (B.C f Kernel.Types.Beckn.Context.IndianState),
                              stdCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table MerchantOperatingCityT
    where data PrimaryKey MerchantOperatingCityT f = MerchantOperatingCityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = MerchantOperatingCityId . id
type MerchantOperatingCity = MerchantOperatingCityT Identity

$(enableKVPG (''MerchantOperatingCityT) [('id)] [])

$(mkTableInstances (''MerchantOperatingCityT) "merchant_operating_city")

