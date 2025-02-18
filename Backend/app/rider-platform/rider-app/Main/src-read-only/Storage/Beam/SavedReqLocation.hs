{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SavedReqLocation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SavedReqLocationT f = SavedReqLocationT
  { area :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    areaCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    building :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    city :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    country :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    door :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    isMoved :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    lat :: (B.C f Kernel.Prelude.Double),
    locationName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    lon :: (B.C f Kernel.Prelude.Double),
    placeId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    riderId :: (B.C f Kernel.Prelude.Text),
    state :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    street :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    tag :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    ward :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table SavedReqLocationT where
  data PrimaryKey SavedReqLocationT f = SavedReqLocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SavedReqLocationId . id

type SavedReqLocation = SavedReqLocationT Identity

$(enableKVPG (''SavedReqLocationT) [('id)] [[('riderId)]])

$(mkTableInstances (''SavedReqLocationT) "saved_location")
