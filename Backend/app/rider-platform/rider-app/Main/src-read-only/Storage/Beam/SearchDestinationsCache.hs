{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchDestinationsCache where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SearchDestinationsCacheT f = SearchDestinationsCacheT
  { geoHash :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    lat :: (B.C f Kernel.Prelude.Double),
    lon :: (B.C f Kernel.Prelude.Double),
    response :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchDestinationsCacheT where
  data PrimaryKey SearchDestinationsCacheT f = SearchDestinationsCacheId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchDestinationsCacheId . id

type SearchDestinationsCache = SearchDestinationsCacheT Identity

$(enableKVPG (''SearchDestinationsCacheT) [('id)] [[('geoHash)]])

$(mkTableInstances (''SearchDestinationsCacheT) "search_destinations_cache")
