module Domain.Types.Sos where

import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Domain.Types.Person (Person)
import Domain.Types.Ride (Ride)
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

data Sos = Sos
  { id :: Id Sos,
    personId :: Id Person,
    rideId :: Id Ride,
    status :: SosStatus,
    flow :: SosType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance FromField SosType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be SosType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be SosType

instance FromBackendRow Postgres SosType

instance IsString SosType where
  fromString = show

newtype EmergencyContactId = EmergencyContactId Text
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosType = Police | CustomerCare | EmergencyContact EmergencyContactId
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosStatus
  = Resolved
  | NotResolved
  | Pending
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromField SosStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be SosStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be SosStatus

instance FromBackendRow Postgres SosStatus

instance IsString SosStatus where
  fromString = show
