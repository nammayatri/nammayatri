{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.SearchRequestPartiesLink where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Trip
import qualified Domain.Types.SearchRequest
import qualified Tools.Beam.UtilsTH



data SearchRequestPartiesLink
    = SearchRequestPartiesLink {id :: Kernel.Types.Id.Id Domain.Types.SearchRequestPartiesLink.SearchRequestPartiesLink,
                                partyId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                                partyName :: Kernel.Prelude.Text,
                                partyType :: Domain.Types.Trip.TripParty,
                                searchRequestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
                                createdAt :: Kernel.Prelude.UTCTime,
                                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



