{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PlaceNameCache where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.Extra.PlaceNameCache
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PlaceNameCacheT f
    = PlaceNameCacheT {addressComponents :: (B.C f [Domain.Types.Extra.PlaceNameCache.AddressResp]),
                       formattedAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       geoHash :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       id :: (B.C f Kernel.Prelude.Text),
                       lat :: (B.C f Kernel.Prelude.Double),
                       lon :: (B.C f Kernel.Prelude.Double),
                       placeId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       plusCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       createdAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PlaceNameCacheT
    where data PrimaryKey PlaceNameCacheT f = PlaceNameCacheId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PlaceNameCacheId . id
type PlaceNameCache = PlaceNameCacheT Identity

$(enableKVPG (''PlaceNameCacheT) [('id)] [[('geoHash)], [('placeId)]])

$(mkTableInstances (''PlaceNameCacheT) "place_name_cache")

