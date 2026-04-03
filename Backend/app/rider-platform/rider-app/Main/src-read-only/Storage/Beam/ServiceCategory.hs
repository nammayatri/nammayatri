{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.ServiceCategory where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Data.Aeson
import qualified Database.Beam as B



data ServiceCategoryT f
    = ServiceCategoryT {allowedSeats :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                        availableSeats :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                        description :: (B.C f Kernel.Prelude.Text),
                        id :: (B.C f Kernel.Prelude.Text),
                        inclusionPoints :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                        isClosed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                        maxSelection :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                        name :: (B.C f Kernel.Prelude.Text),
                        peopleCategory :: (B.C f [Kernel.Prelude.Text]),
                        placeId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                        rules :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                        merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table ServiceCategoryT
    where data PrimaryKey ServiceCategoryT f = ServiceCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = ServiceCategoryId . id
type ServiceCategory = ServiceCategoryT Identity

$(enableKVPG (''ServiceCategoryT) [('id)] [])

$(mkTableInstances (''ServiceCategoryT) "service_category")

