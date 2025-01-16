{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequestPartiesLink where

import Data.Aeson
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.Trip
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SearchRequestPartiesLink = SearchRequestPartiesLink
  { id :: Kernel.Types.Id.Id Domain.Types.SearchRequestPartiesLink.SearchRequestPartiesLink,
    partyId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    partyName :: Kernel.Prelude.Text,
    partyType :: Domain.Types.Trip.TripParty,
    searchRequestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
