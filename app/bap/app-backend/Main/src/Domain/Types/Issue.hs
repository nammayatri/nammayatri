module Domain.Types.Issue where

import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import Kernel.Prelude
import Kernel.Types.Id

data Issue = Issue
  { id :: Id Issue,
    customerId :: Id DPerson.Person,
    bookingId :: Maybe (Id DQuote.Quote),
    contactEmail :: Maybe Text,
    reason :: Text,
    description :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)
