{-# LANGUAGE DataKinds #-}

module Types.API.Person where

import Beckn.External.FCM.Types as FCM
import Beckn.Types.Id
import Beckn.Types.Predicate
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id, state)
import Servant.API
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as SP

data EntityType = VEHICLE | PASS | TICKET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance FromHttpApiData EntityType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data UpdatePersonReq = UpdatePersonReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    role :: Maybe SP.Role,
    gender :: Maybe SP.Gender,
    email :: Maybe Text,
    identifier :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    description :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON)

validateUpdatePersonReq :: Validate UpdatePersonReq
validateUpdatePersonReq UpdatePersonReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ InMaybe $ MinLength 3 `And` P.name,
      validateField "middleName" middleName $ InMaybe $ NotEmpty `And` P.name,
      validateField "lastName" lastName $ InMaybe $ NotEmpty `And` P.name,
      validateField "fullName" fullName $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description . InMaybe $ LengthInRange 2 255 `And` P.name
    ]

modifyPerson :: UpdatePersonReq -> SP.Person -> SP.Person
modifyPerson req person =
  -- only these below will be updated in the person table. if you want to add something extra please add in queries also
  person{firstName = ifJust (req.firstName) (person.firstName),
         middleName = ifJust (req.middleName) (person.middleName),
         lastName = ifJust (req.lastName) (person.lastName),
         fullName = ifJust (req.fullName) (person.fullName),
         role = ifJustExtract (req.role) (person.role),
         gender = ifJustExtract (req.gender) (person.gender),
         email = ifJust (req.email) (person.email),
         identifier = ifJust (req.identifier) (person.identifier),
         deviceToken = ifJust (req.deviceToken) (person.deviceToken),
         udf1 = person.udf1,
         udf2 = person.udf2,
         organizationId = person.organizationId,
         description = ifJust (req.description) (person.description)
        }

ifJust :: Maybe a -> Maybe a -> Maybe a
ifJust a b = if isJust a then a else b

ifJustExtract :: Maybe a -> a -> a
ifJustExtract a b = fromMaybe b a

newtype UpdatePersonRes = UpdatePersonRes
  {user :: SP.PersonAPIEntity}
  deriving (Generic, ToJSON, FromJSON)

newtype PersonRes = PersonRes
  {user :: SP.PersonAPIEntity}
  deriving (Generic, ToJSON, FromJSON)

newtype DeletePersonRes = DeletePersonRes
  {personId :: Text}
  deriving (Generic, ToJSON, FromJSON)

data LinkReq = LinkReq
  { entityId :: Text,
    entityType :: EntityType
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data PersonEntityRes = PersonEntityRes
  { id :: Id SP.Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    role :: SP.Role,
    gender :: SP.Gender,
    identifierType :: SP.IdentifierType,
    email :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    identifier :: Maybe Text,
    rating :: Maybe Int,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    organizationId :: Maybe (Id Org.Organization),
    deviceToken :: Maybe FCM.FCMRecipientToken,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data GetPersonDetailsRes = GetPersonDetailsRes
  { id :: Id SP.Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    role :: SP.Role,
    gender :: SP.Gender,
    email :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON)
