{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchRequestPartiesLink where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Trip
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SearchRequestPartiesLinkT f = SearchRequestPartiesLinkT
  { id :: B.C f Kernel.Prelude.Text,
    partyId :: B.C f Kernel.Prelude.Text,
    partyName :: B.C f Kernel.Prelude.Text,
    partyType :: B.C f Domain.Types.Trip.TripParty,
    searchRequestId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestPartiesLinkT where
  data PrimaryKey SearchRequestPartiesLinkT f = SearchRequestPartiesLinkId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchRequestPartiesLinkId . id

type SearchRequestPartiesLink = SearchRequestPartiesLinkT Identity

$(enableKVPG ''SearchRequestPartiesLinkT ['id] [['searchRequestId]])

$(mkTableInstances ''SearchRequestPartiesLinkT "search_request_parties_link")
