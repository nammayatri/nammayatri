{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.SearchRequestPartiesLink where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.Trip
import qualified Database.Beam as B



data SearchRequestPartiesLinkT f
    = SearchRequestPartiesLinkT {id :: (B.C f Kernel.Prelude.Text),
                                 partyId :: (B.C f Kernel.Prelude.Text),
                                 partyName :: (B.C f Kernel.Prelude.Text),
                                 partyType :: (B.C f Domain.Types.Trip.TripParty),
                                 searchRequestId :: (B.C f Kernel.Prelude.Text),
                                 createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                 updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table SearchRequestPartiesLinkT
    where data PrimaryKey SearchRequestPartiesLinkT f = SearchRequestPartiesLinkId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = SearchRequestPartiesLinkId . id
type SearchRequestPartiesLink = SearchRequestPartiesLinkT Identity

$(enableKVPG (''SearchRequestPartiesLinkT) [('id)] [[('searchRequestId)]])

$(mkTableInstances (''SearchRequestPartiesLinkT) "search_request_parties_link")

