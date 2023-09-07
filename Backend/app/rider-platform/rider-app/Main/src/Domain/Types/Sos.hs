{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Sos where

import Domain.Types.Person (Person)
import Domain.Types.Ride (Ride)
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

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

newtype EmergencyContactId = EmergencyContactId Text
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosType = Police | CustomerCare | EmergencyContact EmergencyContactId
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SosStatus
  = Resolved
  | NotResolved
  | Pending
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''SosType)

$(mkBeamInstancesForEnum ''SosStatus)
