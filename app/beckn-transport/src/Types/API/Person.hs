{-# LANGUAGE OverloadedLabels #-}

module Types.API.Person where

import App.Types
import Beckn.External.FCM.Types as FCM
import Beckn.TypeClass.Transform
import Beckn.Types.Common as BC
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Location as SL
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Utils.Common
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import EulerHS.Prelude
import Servant.API
import qualified Storage.Queries.Location as QL

data EntityType = VEHICLE | PASS | TICKET
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance FromHttpApiData EntityType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data UpdatePersonReq = UpdatePersonReq
  { _firstName :: Maybe Text,
    _middleName :: Maybe Text,
    _lastName :: Maybe Text,
    _fullName :: Maybe Text,
    _role :: Maybe SP.Role,
    _gender :: Maybe SP.Gender,
    _email :: Maybe Text,
    _identifier :: Maybe Text,
    _rating :: Maybe Text,
    _deviceToken :: Maybe FCM.FCMRecipientToken,
    _udf1 :: Maybe Text,
    _udf2 :: Maybe Text,
    _organizationId :: Maybe Text,
    _description :: Maybe Text,
    _locationType :: Maybe SL.LocationType,
    _lat :: Maybe Double,
    _long :: Maybe Double,
    _ward :: Maybe Text,
    _district :: Maybe Text,
    _city :: Maybe Text,
    _state :: Maybe Text,
    _country :: Maybe Text,
    _pincode :: Maybe Text,
    _address :: Maybe Text,
    _bound :: Maybe Text
  }
  deriving (Generic)

instance FromJSON UpdatePersonReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON UpdatePersonReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance ModifyTransform UpdatePersonReq SP.Person Flow where
  modifyTransform req person = do
    location <- updateOrCreateLocation req $ SP._locationId person
    return
      person
        { -- only these below will be updated in the person table. if you want to add something extra please add in queries also
          SP._firstName = ifJust (req ^. #_firstName) (person ^. #_firstName),
          SP._middleName = ifJust (req ^. #_middleName) (person ^. #_middleName),
          SP._lastName = ifJust (req ^. #_lastName) (person ^. #_lastName),
          SP._fullName = ifJust (req ^. #_fullName) (person ^. #_fullName),
          SP._role = ifJustExtract (req ^. #_role) (person ^. #_role),
          SP._gender = ifJustExtract (req ^. #_gender) (person ^. #_gender),
          SP._email = ifJust (req ^. #_email) (person ^. #_email),
          SP._identifier = ifJust (req ^. #_identifier) (person ^. #_identifier),
          SP._rating = ifJust (req ^. #_rating) (person ^. #_rating),
          SP._deviceToken = ifJust (req ^. #_deviceToken) (person ^. #_deviceToken),
          SP._udf1 = ifJust (req ^. #_udf1) (person ^. #_udf1),
          SP._udf2 = ifJust (req ^. #_udf2) (person ^. #_udf2),
          SP._organizationId = ifJust (req ^. #_organizationId) (person ^. #_organizationId),
          SP._description = ifJust (req ^. #_description) (person ^. #_description),
          SP._locationId = Just (getId $ SL._id location)
        }

updateOrCreateLocation :: UpdatePersonReq -> Maybe Text -> Flow SL.Location
updateOrCreateLocation req Nothing = do
  location <- createLocation req
  QL.create location
  return location
updateOrCreateLocation req (Just locId) = do
  location <-
    QL.findLocationById (Id locId)
      >>= fromMaybeM LocationDoesNotExist
  QL.updateLocationRec (Id locId) $ transformToLocation req location
  return location

transformToLocation :: UpdatePersonReq -> SL.Location -> SL.Location
transformToLocation req location =
  location
    { SL._locationType = fromMaybe SL.PINCODE $ req ^. #_locationType,
      SL._lat = req ^. #_lat,
      SL._long = req ^. #_long,
      SL._ward = req ^. #_ward,
      SL._district = req ^. #_district,
      SL._city = req ^. #_city,
      SL._state = req ^. #_state,
      SL._country = req ^. #_country,
      SL._pincode = req ^. #_pincode,
      SL._address = req ^. #_address,
      SL._bound = req ^. #_bound
    }

createLocation :: UpdatePersonReq -> Flow SL.Location
createLocation UpdatePersonReq {..} = do
  _id <- BC.generateGUID
  _createdAt <- getCurrTime
  pure
    SL.Location
      { _locationType = fromMaybe SL.PINCODE _locationType,
        _updatedAt = _createdAt,
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
  { _firstName :: Maybe Text,
    _middleName :: Maybe Text,
    _lastName :: Maybe Text,
    _fullName :: Maybe Text,
    _role :: Maybe SP.Role,
    _gender :: Maybe SP.Gender,
    _email :: Maybe Text,
    _identifier :: Maybe Text,
    _identifierType :: Maybe SP.IdentifierType,
    _rating :: Maybe Text,
    _deviceToken :: Maybe FCM.FCMRecipientToken,
    _mobileNumber :: Maybe Text,
    _mobileCountryCode :: Maybe Text,
    _udf1 :: Maybe Text,
    _udf2 :: Maybe Text,
    _description :: Maybe Text,
    _locationType :: Maybe SL.LocationType,
    _lat :: Maybe Double,
    _long :: Maybe Double,
    _ward :: Maybe Text,
    _district :: Maybe Text,
    _city :: Maybe Text,
    _state :: Maybe Text,
    _country :: Maybe Text,
    _pincode :: Maybe Text,
    _address :: Maybe Text,
    _bound :: Maybe Text
  }
  deriving (Generic)

instance FromJSON CreatePersonReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CreatePersonReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance CreateTransform CreatePersonReq SP.Person Flow where
  createTransform req = do
    pid <- BC.generateGUID
    now <- getCurrTime
    location <- createLocationT req
    return
      SP.Person
        { -- only these below will be updated in the person table. if you want to add something extra please add in queries also
          SP._id = pid,
          SP._firstName = req ^. #_firstName,
          SP._middleName = req ^. #_middleName,
          SP._lastName = req ^. #_lastName,
          SP._fullName = req ^. #_fullName,
          SP._role = ifJustExtract (req ^. #_role) SP.USER,
          SP._gender = ifJustExtract (req ^. #_gender) SP.UNKNOWN,
          SP._email = req ^. #_email,
          SP._passwordHash = Nothing,
          SP._identifier = req ^. #_identifier,
          SP._identifierType = fromMaybe SP.MOBILENUMBER $ req ^. #_identifierType,
          SP._mobileNumber = req ^. #_mobileNumber,
          SP._mobileCountryCode = req ^. #_mobileCountryCode,
          SP._verified = False,
          SP._rating = req ^. #_rating,
          SP._status = SP.INACTIVE,
          SP._deviceToken = req ^. #_deviceToken,
          SP._udf1 = req ^. #_udf1,
          SP._udf2 = req ^. #_udf2,
          SP._organizationId = Nothing,
          SP._description = req ^. #_description,
          SP._locationId = Just (getId $ SL._id location),
          SP._createdAt = now,
          SP._updatedAt = now
        }

createLocationT :: CreatePersonReq -> Flow SL.Location
createLocationT req = do
  location <- createLocationRec req
  QL.create location
  return location

-- FIXME? This is to silence hlint reusing as much code from `createLocation`
--   as possible, still we need fake _organizationId here ...
-- Better solution in he long run is to factor out common data reducing this
--   enormous amount of duplication ...
createLocationRec :: CreatePersonReq -> Flow SL.Location
createLocationRec CreatePersonReq {..} = createLocation UpdatePersonReq {_organizationId = Nothing, ..}

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
  { _entityId :: Text,
    _entityType :: EntityType
  }
  deriving (Show, Generic)

instance FromJSON LinkReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON LinkReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data LinkedEntity = LinkedEntity
  { _entityType :: EntityType,
    _entityValue :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON LinkedEntity where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON LinkedEntity where
  toJSON = genericToJSON stripAllLensPrefixOptions

data PersonEntityRes = PersonEntityRes
  { _id :: Id SP.Person,
    _firstName :: Maybe Text,
    _middleName :: Maybe Text,
    _lastName :: Maybe Text,
    _fullName :: Maybe Text,
    _role :: SP.Role,
    _gender :: SP.Gender,
    _identifierType :: SP.IdentifierType,
    _email :: Maybe Text,
    _mobileNumber :: Maybe Text,
    _mobileCountryCode :: Maybe Text,
    _identifier :: Maybe Text,
    _rating :: Maybe Text,
    _verified :: Bool,
    _udf1 :: Maybe Text,
    _udf2 :: Maybe Text,
    _status :: SP.Status,
    _organizationId :: Maybe Text,
    _locationId :: Maybe Text,
    _deviceToken :: Maybe FCM.FCMRecipientToken,
    _description :: Maybe Text,
    _createdAt :: UTCTime,
    _updatedAt :: UTCTime,
    _linkedEntity :: Maybe LinkedEntity
  }
  deriving (Show, Generic)

instance FromJSON PersonEntityRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON PersonEntityRes where
  toJSON = genericToJSON stripAllLensPrefixOptions
