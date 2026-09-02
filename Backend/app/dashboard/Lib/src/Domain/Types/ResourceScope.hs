{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.ResourceScope where

import qualified Data.Aeson as A
import qualified Data.List as L
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (show)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Text.Show (Show (..))

-- Layer C — resource-scoped access, under a merchant + operating city.
--
-- `resourceType` is a closed enum: adding a new resource kind is a code change (a
-- new constructor + redeploy), not just data. It is stored in the DB as its
-- constructor name (ROUTE, SPECIAL_LOCATION, …) via mkBeamInstancesForEnum, so the
-- existing varchar columns need no migration. `resourceId` stays open Text — it
-- holds whatever the data keys on: an id (`route_123`) or a name ("Airport Zone").
-- Semantics per (person, merchant, city, resourceType):
--   * a wildcardResourceId ("*") row → all resources of that type in the MOC
--   * specific ids                   → restricted to those
--   * no rows                        → deny-all (ops gate) / unscoped (analytics)

-- | The kinds of resource Layer C can scope. Serialized (DB + JSON) as the
-- constructor name. Adding a kind = a new constructor here.
data ResourceType
  = ROUTE
  | SPECIAL_ZONE
  | SPECIAL_LOCATION
  | TICKET_PLACE
  deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''ResourceType)

-- | The Layer C ops-gate binding on capability_endpoint.resource_id_param — WHERE
-- (and whether) a scoped endpoint carries its resource id. A closed type (not raw
-- Text); serialized to/from its text form via Show/Read (same pattern as
-- Lib.Types.SpecialLocation.Area), so mkBeamInstancesForEnum stores/reads it and
-- the existing varchar column needs no migration:
--   BindParam "specialLocation" ↔ "param:specialLocation"  (path-capture segment)
--   BindSkip                    ↔ "__SKIP__"                (endpoint not gated)
--   BindHandler                 ↔ "__HANDLER__"             (the handler enforces)
-- A NULL column (no binding) = the gate has nothing to resolve → warn-and-pass.
data ResourceBinding
  = BindSkip
  | BindHandler
  | BindParam Text
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToSchema)

instance Show ResourceBinding where
  show BindSkip = "__SKIP__"
  show BindHandler = "__HANDLER__"
  show (BindParam name) = "param:" <> T.unpack name

instance Read ResourceBinding where
  readsPrec _ s
    | Just r <- L.stripPrefix "__SKIP__" s = [(BindSkip, r)]
    | Just r <- L.stripPrefix "__HANDLER__" s = [(BindHandler, r)]
    | Just r <- L.stripPrefix "param:" s = [(BindParam (T.pack r), "")]
    | otherwise = []

instance ToJSON ResourceBinding where
  toJSON = toJSON . T.pack . show

instance FromJSON ResourceBinding where
  parseJSON = A.withText "ResourceBinding" $ \t ->
    maybe (fail ("Invalid ResourceBinding: " <> T.unpack t)) pure (readMaybe (T.unpack t))

$(mkBeamInstancesForEnum ''ResourceBinding)

-- | Sentinel resourceId meaning "every resource of this type in the MOC".
wildcardResourceId :: Text
wildcardResourceId = "*"

data PersonResourceAccess = PersonResourceAccess
  { id :: Id PersonResourceAccess,
    personId :: Id DPerson.Person,
    merchantId :: Id DMerchant.Merchant,
    operatingCity :: City.City,
    resourceType :: ResourceType,
    resourceId :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
