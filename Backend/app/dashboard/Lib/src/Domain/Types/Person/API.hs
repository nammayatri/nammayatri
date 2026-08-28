{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Person.API where

import qualified Data.Text as T
import qualified Domain.Types.Entity as DEntity
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person.Type
import qualified Domain.Types.Role as DRole
import qualified Kernel.External.Types as KET
import Kernel.Prelude
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Text,
    lastName :: Text,
    role :: DRole.RoleAPIEntity,
    email :: Maybe Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    availableMerchants :: [ShortId DMerchant.Merchant],
    availableCitiesForMerchant :: Maybe [AvailableCitiesForMerchant],
    registeredAt :: UTCTime,
    verified :: Maybe Bool,
    receiveNotification :: Maybe Bool,
    language :: Maybe KET.Language,
    -- | Deprecated, retained so the wire shape stays additive: the first entity's id and name.
    -- A person may now hold several entities; read 'entityShortIds' instead.
    entityId :: Maybe (Id DEntity.Entity),
    entityName :: Maybe Text,
    entityShortIds :: [ShortId DEntity.Entity],
    tokenNo :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data AvailableCitiesForMerchant = AvailableCitiesForMerchant
  { merchantShortId :: ShortId DMerchant.Merchant,
    operatingCity :: [City.City]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- tokenNo authenticates a PT login, so callers state whether this response is allowed to carry
-- it. Only the person's own profile does; the admin list does not (ptList is the place for that).
data TokenNoVisibility = ShowTokenNo | HideTokenNo
  deriving (Eq, Show)

-- Legacy hash-only rows decrypt to an empty placeholder; surface those as "no token" rather than
-- as a blank credential. See isLegacyTokenNoPlaceholder.
presentableTokenNo :: Maybe Text -> Maybe Text
presentableTokenNo mbTokenNo = mbTokenNo >>= \t -> if T.null t then Nothing else Just t

-- Takes the resolved entities rather than a pre-picked name so the deprecated scalar and the
-- new list can never disagree about which entity comes first.
makePersonAPIEntity :: DecryptedPerson -> DRole.Role -> [ShortId DMerchant.Merchant] -> Maybe [AvailableCitiesForMerchant] -> [DEntity.Entity] -> TokenNoVisibility -> PersonAPIEntity
makePersonAPIEntity Person {..} personRole availableMerchants availableCitiesForMerchant personEntities tokenNoVisibility =
  PersonAPIEntity
    { registeredAt = createdAt,
      role = DRole.mkRoleAPIEntity personRole,
      language = language,
      entityId = listToMaybe personEntities <&> (.id),
      entityName = listToMaybe personEntities <&> (.entityName),
      entityShortIds = personEntities <&> (.entityShortId),
      tokenNo = case tokenNoVisibility of
        ShowTokenNo -> presentableTokenNo tokenNo
        HideTokenNo -> Nothing,
      ..
    }

-- Flat, PT-shaped row: what a depot-operations screen needs about one conductor or depot
-- manager, without the merchant/city access machinery the generic entity carries.
data PTEmployeeAPIEntity = PTEmployeeAPIEntity
  { id :: Id Person,
    firstName :: Text,
    lastName :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    email :: Maybe Text,
    roleName :: Text,
    tokenNo :: Maybe Text,
    vpa :: Maybe Text,
    entityShortIds :: [ShortId DEntity.Entity],
    verified :: Maybe Bool,
    registeredAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

makePTEmployeeAPIEntity :: DecryptedPerson -> DRole.Role -> [DEntity.Entity] -> PTEmployeeAPIEntity
makePTEmployeeAPIEntity Person {..} personRole personEntities =
  PTEmployeeAPIEntity
    { roleName = personRole.name,
      tokenNo = presentableTokenNo tokenNo,
      vpa = vpa,
      entityShortIds = personEntities <&> (.entityShortId),
      registeredAt = createdAt,
      ..
    }
