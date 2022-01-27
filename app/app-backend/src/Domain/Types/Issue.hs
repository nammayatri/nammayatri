module Domain.Types.Issue where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote

data Issue = Issue
  { id :: Id Issue,
    customerId :: Id DPerson.Person,
    rideBookingId :: Maybe (Id DQuote.Quote),
    contactEmail :: Maybe Text,
    reason :: Text,
    description :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)
