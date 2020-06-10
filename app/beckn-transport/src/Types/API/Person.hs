{-# LANGUAGE OverloadedLabels #-}

module Types.API.Person where

import Beckn.External.FCM.Types as FCM
import Beckn.TypeClass.Transform
import Beckn.Types.App
import Beckn.Types.Common as BC
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Location as SL
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Utils.Extra
import Data.Generics.Labels
import Data.Swagger
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Swagger
import qualified Storage.Queries.Location as QL

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
  deriving (Generic, ToSchema)

instance FromJSON UpdatePersonReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance Transform UpdatePersonReq SP.Person where
  transformFlow2 req person = do
    location <- updateOrCreateLocation req $ SP._locationId person
    return $
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
          SP._locationId = Just (_getLocationId $ SL._id location)
        }

updateOrCreateLocation :: UpdatePersonReq -> Maybe Text -> L.Flow SL.Location
updateOrCreateLocation req Nothing = do
  location <- createLocation req
  QL.create location
  return location
updateOrCreateLocation req (Just id) = do
  location <- QL.findLocationById (LocationId id)
  QL.updateLocationRec (LocationId id) $ transformToLocation req location
  return location

transformToLocation :: UpdatePersonReq -> SL.Location -> SL.Location
transformToLocation req location = do
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

createLocation :: UpdatePersonReq -> L.Flow SL.Location
createLocation req = do
  id <- BC.generateGUID
  now <- getCurrentTimeUTC
  return $
    SL.Location
      { SL._id = id,
        SL._locationType = fromMaybe SL.PINCODE $ req ^. #_locationType,
        SL._lat = req ^. #_lat,
        SL._long = req ^. #_long,
        SL._ward = req ^. #_ward,
        SL._district = req ^. #_district,
        SL._city = req ^. #_city,
        SL._state = req ^. #_state,
        SL._country = req ^. #_country,
        SL._pincode = req ^. #_pincode,
        SL._address = req ^. #_address,
        SL._bound = req ^. #_bound,
        SL._createdAt = now,
        SL._updatedAt = now
      }

ifJust a b = if isJust a then a else b

ifJustExtract a b = fromMaybe b a

(^^.) f g = g f

data UpdatePersonRes = UpdatePersonRes
  {user :: SP.Person}
  deriving (Generic, ToJSON, ToSchema)

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
  deriving (Generic, ToSchema)

instance FromJSON CreatePersonReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance Transform2 CreatePersonReq SP.Person where
  transformFlow req = do
    id <- BC.generateGUID
    now <- getCurrentTimeUTC
    location <- createLocationT req
    return $
      SP.Person
        { -- only these below will be updated in the person table. if you want to add something extra please add in queries also
          SP._id = id,
          SP._firstName = req ^. #_firstName,
          SP._middleName = req ^. #_middleName,
          SP._lastName = req ^. #_lastName,
          SP._fullName = req ^. #_fullName,
          SP._role = ifJustExtract (req ^. #_role) SP.USER,
          SP._gender = ifJustExtract (req ^. #_gender) SP.UNKNOWN,
          SP._email = req ^. #_email,
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
          SP._locationId = Just (_getLocationId $ SL._id location),
          SP._createdAt = now,
          SP._updatedAt = now
        }

createLocationT :: CreatePersonReq -> L.Flow SL.Location
createLocationT req = do
  location <- createLocationRec req
  QL.create location
  return location

createLocationRec :: CreatePersonReq -> L.Flow SL.Location
createLocationRec req = do
  id <- BC.generateGUID
  now <- getCurrentTimeUTC
  return $
    SL.Location
      { SL._id = id,
        SL._locationType = fromMaybe SL.PINCODE $ req ^. #_locationType,
        SL._lat = req ^. #_lat,
        SL._long = req ^. #_long,
        SL._ward = req ^. #_ward,
        SL._district = req ^. #_district,
        SL._city = req ^. #_city,
        SL._state = req ^. #_state,
        SL._country = req ^. #_country,
        SL._pincode = req ^. #_pincode,
        SL._address = req ^. #_address,
        SL._bound = req ^. #_bound,
        SL._createdAt = now,
        SL._updatedAt = now
      }

data ListPersonRes = ListPersonRes
  {users :: [SP.Person]}
  deriving (Generic, ToJSON, ToSchema)
