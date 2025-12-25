{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Person.Type where

import Data.Aeson
import qualified Domain.Types.Role as DRole
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.TH

data PersonE e = Person
  { id :: Id Person,
    firstName :: Text,
    lastName :: Text,
    roleId :: Id DRole.Role,
    email :: Maybe (EncryptedHashedField e Text),
    mobileNumber :: EncryptedHashedField e Text,
    mobileCountryCode :: Text,
    passwordHash :: Maybe DbHash,
    dashboardAccessType :: Maybe DRole.DashboardAccessType,
    dashboardType :: DashboardType, -- Using enum for type safety
    createdAt :: UTCTime,
    receiveNotification :: Maybe Bool,
    updatedAt :: UTCTime,
    verified :: Maybe Bool,
    rejectionReason :: Maybe Text,
    rejectedAt :: Maybe UTCTime,
    passwordUpdatedAt :: Maybe UTCTime,
    approvedBy :: Maybe (Id Person),
    rejectedBy :: Maybe (Id Person)
  }
  deriving (Generic)

type Person = PersonE 'AsEncrypted

type DecryptedPerson = PersonE 'AsUnencrypted

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (Person {..}, salt) = do
    mobileNumber_ <- encryptItem (mobileNumber, salt)
    email_ <- encryptItem $ (,salt) <$> email
    return Person {mobileNumber = mobileNumber_, email = email_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fst <$> decryptItem mobileNumber
    email_ <- fmap fst <$> decryptItem email
    return (Person {mobileNumber = mobileNumber_, email = email_, ..}, "")

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data DashboardType = DEFAULT_DASHBOARD | TICKET_DASHBOARD
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data DashboardTypeTag = DefaultDashboard | TicketDashboard

data SingDashboardType (t :: DashboardTypeTag) where
  SingDefaultDashboard :: SingDashboardType 'DefaultDashboard
  SingTicketDashboard :: SingDashboardType 'TicketDashboard

class KnownDashboardType (t :: DashboardTypeTag) where
  dashboardTypeVal :: proxy t -> DashboardType

instance KnownDashboardType 'DefaultDashboard where
  dashboardTypeVal _ = DEFAULT_DASHBOARD

instance KnownDashboardType 'TicketDashboard where
  dashboardTypeVal _ = TICKET_DASHBOARD

withDashboardType :: forall m a. Monad m => Maybe DashboardType -> (forall (t :: DashboardTypeTag). KnownDashboardType t => Proxy t -> m a) -> m a
withDashboardType (Just TICKET_DASHBOARD) f = f (Proxy @'TicketDashboard)
withDashboardType _ f = f (Proxy @'DefaultDashboard)

isFleetOwner :: Person -> Bool
isFleetOwner person = person.dashboardAccessType `elem` [Just DRole.FLEET_OWNER, Just DRole.RENTAL_FLEET_OWNER]

isAdmin :: Person -> Bool
isAdmin person = person.dashboardAccessType == Just DRole.DASHBOARD_ADMIN

$(mkBeamInstancesForEnum ''DashboardType)

$(mkHttpInstancesForEnum ''DashboardType)
