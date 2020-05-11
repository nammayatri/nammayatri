{-# LANGUAGE OverloadedLabels      #-}
module Types.API.Person where

import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.Location as SL
import qualified Storage.Queries.Location as QL
import qualified EulerHS.Language as L
import Beckn.Types.Common as BC
import Data.Swagger
import Beckn.Utils.Extra
import Beckn.Types.App
import Beckn.Types.Common
import EulerHS.Prelude
import Data.Generics.Labels
import Servant.Swagger
import Beckn.TypeClass.Transform

data UpdatePersonReq = UpdatePersonReq
  { _firstName          :: Maybe Text
  , _middleName         :: Maybe Text
  , _lastName           :: Maybe Text
  , _fullName           :: Maybe Text
  , _role               :: Maybe SP.Role
  , _gender             :: Maybe SP.Gender
  , _email              :: Maybe Text
  , _identifier         :: Maybe Text
  , _rating             :: Maybe Text
  , _deviceToken        :: Maybe Text
  , _udf1               :: Maybe Text
  , _udf2               :: Maybe Text
  , _organizationId     :: Maybe Text
  , _description        :: Maybe Text
  , _locationType       :: Maybe SL.LocationType
  , _lat                :: Maybe Text
  , _long               :: Maybe Text
  , _ward               :: Maybe Text
  , _district           :: Maybe Text
  , _city               :: Maybe Text
  , _state              :: Maybe Text
  , _country            :: Maybe Text
  , _pincode            :: Maybe Text
  , _address            :: Maybe Text
  , _bound              :: Maybe Text

  }
  deriving (Generic, ToSchema)

instance FromJSON UpdatePersonReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance Transform UpdatePersonReq SP.Person where
  transformFlow2 req person = do
    location        <- updateOrCreateLocation req $ SP._locationId person
    return $ person {
      -- only these below will be updated in the person table. if you want to add something extra please add in queries also
      SP._firstName = ifJust (req ^^. _firstName) (person ^^. SP._firstName)
      , SP._middleName = ifJust (req ^^. _middleName) (person ^^. SP._middleName)
      , SP._lastName           = ifJust (req ^^.  _lastName) (person ^^. SP._lastName)
      , SP._fullName           = ifJust (req ^^.  _fullName) (person ^^. SP._fullName)
      , SP._role               = ifJustExtract (req ^^.  _role) (person ^^. SP._role)
      , SP._gender             = ifJustExtract (req ^^.  _gender) (person ^^. SP._gender)
      , SP._email              = ifJust (req ^^.  _email) (person ^^. SP._email)
      , SP._identifier         = ifJust (req ^^.  _identifier) (person ^^. SP._identifier)
      , SP._rating             = ifJust (req ^^.  _rating) (person ^^. SP._rating)
      , SP._deviceToken        = ifJust (req ^^.  _deviceToken) (person ^^. SP._deviceToken)
      , SP._udf1               = ifJust (req ^^.  _udf1) (person ^^. SP._udf1)
      , SP._udf2               = ifJust (req ^^.  _udf2) (person ^^. SP._udf2)
      , SP._organizationId     = ifJust (req ^^.  _organizationId) (person ^^. SP._organizationId)
      , SP._description        = ifJust (req ^^.  _description) (person ^^. SP._description)
      , SP._locationId         = Just (_getLocationId $ SL._id location)
    }

updateOrCreateLocation :: UpdatePersonReq ->  Maybe Text -> L.Flow SL.Location
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
  location {
    SL._locationType = fromMaybe SL.PINCODE $ req ^. #_locationType
    , SL._lat = req ^. #_lat
    , SL._long = req ^. #_long
    , SL._ward = req ^. #_ward
    , SL._district = req ^. #_district
    , SL._city = req ^. #_city
    , SL._state = req ^. #_state
    , SL._country = req ^. #_country
    , SL._pincode = req ^. #_pincode
    , SL._address = req ^. #_address
    , SL._bound = req ^. #_bound
  }

createLocation :: UpdatePersonReq -> L.Flow SL.Location
createLocation req = do
  id <- BC.generateGUID
  now <- getCurrentTimeUTC
  return $ SL.Location {
    SL._id = id
    , SL._locationType = fromMaybe SL.PINCODE $ req ^. #_locationType
    , SL._lat = req ^. #_lat
    , SL._long = req ^. #_long
    , SL._ward = req ^. #_ward
    , SL._district = req ^. #_district
    , SL._city = req ^. #_city
    , SL._state = req ^. #_state
    , SL._country = req ^. #_country
    , SL._pincode = req ^. #_pincode
    , SL._address = req ^. #_address
    , SL._bound = req ^. #_bound
    , SL._createdAt = now
    , SL._updatedAt = now
  }

ifJust a b = if isJust a then a else b
ifJustExtract a b = fromMaybe b a

(^^.) f g = g f

data UpdatePersonRes = UpdatePersonRes
  {  user :: SP.Person }
  deriving (Generic, ToJSON, ToSchema)