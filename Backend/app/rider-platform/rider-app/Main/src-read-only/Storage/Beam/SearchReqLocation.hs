{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchReqLocation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SearchReqLocationT f = SearchReqLocationT
  { area :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    areaCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    building :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    city :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    country :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    door :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    extras :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    instructions :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    placeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    state :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    street :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    title :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ward :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f Kernel.Prelude.Double,
    lon :: B.C f Kernel.Prelude.Double,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchReqLocationT where
  data PrimaryKey SearchReqLocationT f = SearchReqLocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchReqLocationId . id

type SearchReqLocation = SearchReqLocationT Identity

$(enableKVPG ''SearchReqLocationT ['id] [])

$(mkTableInstances ''SearchReqLocationT "search_request_location")
