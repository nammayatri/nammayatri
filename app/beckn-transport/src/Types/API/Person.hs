{-# LANGUAGE DataKinds #-}

module Types.API.Person where

import App.Types
import Beckn.External.Encryption (encrypt)
import Beckn.External.FCM.Types as FCM
import Beckn.TypeClass.Transform
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Predicate
import qualified Beckn.Types.Storage.Location as SL
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Utils.JSON
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id, state)
import Servant.API
import qualified Storage.Queries.Location as QL
import Types.Error
import Utils.Common

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
    rating :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    description :: Maybe Text,
    locationType :: Maybe SL.LocationType,
    lat :: Maybe Double,
    long :: Maybe Double,
    ward :: Maybe Text,
    district :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    pincode :: Maybe Text,
    address :: Maybe Text,
    bound :: Maybe Text
  }
  deriving (Generic, ToJSON)

instance FromJSON UpdatePersonReq where
  parseJSON = genericParseJsonWithValidation "UpdatePersonReq" validateUpdatePersonReq

validateUpdatePersonReq :: Validate UpdatePersonReq
validateUpdatePersonReq UpdatePersonReq {..} =
  validateMaybe "firstName" firstName $ MinLength 3 `And` P.name

instance ModifyTransform UpdatePersonReq SP.Person Flow where
  modifyTransform req person = do
    location <- updateOrCreateLocation req $ SP.locationId person
    return
      person
        { -- only these below will be updated in the person table. if you want to add something extra please add in queries also
          SP.firstName = ifJust (req.firstName) (person.firstName),
          SP.middleName = ifJust (req.middleName) (person.middleName),
          SP.lastName = ifJust (req.lastName) (person.lastName),
          SP.fullName = ifJust (req.fullName) (person.fullName),
          SP.role = ifJustExtract (req.role) (person.role),
          SP.gender = ifJustExtract (req.gender) (person.gender),
          SP.email = ifJust (req.email) (person.email),
          SP.identifier = ifJust (req.identifier) (person.identifier),
          SP.rating = ifJust (req.rating) (person.rating),
          SP.deviceToken = ifJust (req.deviceToken) (person.deviceToken),
          SP.udf1 = ifJust (req.udf1) (person.udf1),
          SP.udf2 = ifJust (req.udf2) (person.udf2),
          SP.organizationId = person.organizationId,
          SP.description = ifJust (req.description) (person.description),
          SP.locationId = Just (SL.id location)
        }

updateOrCreateLocation :: UpdatePersonReq -> Maybe (Id SL.Location) -> Flow SL.Location
updateOrCreateLocation req Nothing = do
  location <- createLocation req
  QL.createFlow location
  return location
updateOrCreateLocation req (Just locId) = do
  location <-
    QL.findLocationById locId
      >>= fromMaybeM LocationDoesNotExist
  QL.updateLocationRec locId $ transformToLocation req location
  return location

transformToLocation :: UpdatePersonReq -> SL.Location -> SL.Location
transformToLocation req location =
  location
    { SL.locationType = fromMaybe SL.PINCODE $ req.locationType,
      SL.lat = req.lat,
      SL.long = req.long,
      SL.ward = req.ward,
      SL.district = req.district,
      SL.city = req.city,
      SL.state = req.state,
      SL.country = req.country,
      SL.pincode = req.pincode,
      SL.address = req.address,
      SL.bound = req.bound
    }

createLocation :: UpdatePersonReq -> Flow SL.Location
createLocation UpdatePersonReq {..} = do
  id <- generateGUID
  createdAt <- getCurrentTime
  pure
    SL.Location
      { locationType = fromMaybe SL.PINCODE locationType,
        updatedAt = createdAt,
        point = SL.Point,
        ..
      }

ifJust :: Maybe a -> Maybe a -> Maybe a
ifJust a b = if isJust a then a else b

ifJustExtract :: Maybe a -> a -> a
ifJustExtract a b = fromMaybe b a

newtype UpdatePersonRes = UpdatePersonRes
  {user :: SP.Person}
  deriving (Generic, ToJSON, FromJSON)

-- Create Person request and response
data CreatePersonReq = CreatePersonReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    role :: Maybe SP.Role,
    gender :: Maybe SP.Gender,
    email :: Maybe Text,
    identifier :: Maybe Text,
    identifierType :: Maybe SP.IdentifierType,
    rating :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    description :: Maybe Text,
    locationType :: Maybe SL.LocationType,
    lat :: Maybe Double,
    long :: Maybe Double,
    ward :: Maybe Text,
    district :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    pincode :: Maybe Text,
    address :: Maybe Text,
    bound :: Maybe Text
  }
  deriving (Generic, ToJSON)

validateCreatePersonReq :: Validate CreatePersonReq
validateCreatePersonReq CreatePersonReq {..} =
  sequenceA_
    [ validateMaybe "firstName" firstName $ NotEmpty `And` P.name,
      validateMaybe "mobileNumber" mobileNumber P.mobileNumber,
      validateMaybe "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

instance FromJSON CreatePersonReq where
  parseJSON = genericParseJsonWithValidation "CreatePersonReq" validateCreatePersonReq

instance CreateTransform CreatePersonReq SP.Person Flow where
  createTransform req = do
    pid <- generateGUID
    now <- getCurrentTime
    location <- createLocationT req
    mobileNumber <- encrypt req.mobileNumber
    return
      SP.Person
        { -- only these below will be updated in the person table. if you want to add something extra please add in queries also
          SP.id = pid,
          SP.firstName = req.firstName,
          SP.middleName = req.middleName,
          SP.lastName = req.lastName,
          SP.fullName = req.fullName,
          SP.role = ifJustExtract (req.role) SP.USER,
          SP.gender = ifJustExtract (req.gender) SP.UNKNOWN,
          SP.email = req.email,
          SP.passwordHash = Nothing,
          SP.identifier = req.identifier,
          SP.identifierType = fromMaybe SP.MOBILENUMBER $ req.identifierType,
          SP.mobileNumber = mobileNumber,
          SP.mobileCountryCode = req.mobileCountryCode,
          SP.verified = False,
          SP.rating = req.rating,
          SP.status = SP.INACTIVE,
          SP.deviceToken = req.deviceToken,
          SP.udf1 = req.udf1,
          SP.udf2 = req.udf2,
          SP.organizationId = Nothing,
          SP.description = req.description,
          SP.locationId = Just location.id,
          SP.createdAt = now,
          SP.updatedAt = now
        }

createLocationT :: CreatePersonReq -> Flow SL.Location
createLocationT req = do
  location <- createLocationRec req
  QL.createFlow location
  return location

-- FIXME? This is to silence hlint reusing as much code from `createLocation`
--   as possible, still we need fake organizationId here ...
-- Better solution in he long run is to factor out common data reducing this
--   enormous amount of duplication ...
createLocationRec :: CreatePersonReq -> Flow SL.Location
createLocationRec CreatePersonReq {..} = createLocation UpdatePersonReq {..}

newtype ListPersonRes = ListPersonRes
  {users :: [PersonEntityRes]}
  deriving (Generic, ToJSON, FromJSON)

newtype PersonRes = PersonRes
  {user :: SP.Person}
  deriving (Generic, ToJSON, FromJSON)

newtype DeletePersonRes = DeletePersonRes
  {personId :: Text}
  deriving (Generic, ToJSON, FromJSON)

data LinkReq = LinkReq
  { entityId :: Text,
    entityType :: EntityType
  }
  deriving (Show, Generic)

instance FromJSON LinkReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LinkReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data LinkedEntity = LinkedEntity
  { entityType :: EntityType,
    entityValue :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON LinkedEntity where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LinkedEntity where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

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
    rating :: Maybe Text,
    verified :: Bool,
    udf1 :: Maybe Text,
    udf2 :: Maybe Text,
    status :: SP.Status,
    organizationId :: Maybe (Id Org.Organization),
    locationId :: Maybe (Id SL.Location),
    deviceToken :: Maybe FCM.FCMRecipientToken,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    linkedEntity :: Maybe LinkedEntity
  }
  deriving (Show, Generic)

instance FromJSON PersonEntityRes where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PersonEntityRes where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

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
