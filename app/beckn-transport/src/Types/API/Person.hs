module Types.API.Person where

import qualified Beckn.Types.Storage.Person as SP
import Data.Swagger
import Beckn.Types.Common
import EulerHS.Prelude
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
  , _mobileNumber       :: Maybe Text
  , _mobileCountryCode  :: Maybe Text
  , _identifier         :: Maybe Text
  , _rating             :: Maybe Text
  , _deviceToken        :: Maybe Text
  , _udf1               :: Maybe Text
  , _udf2               :: Maybe Text
  , _organizationId     :: Maybe Text
  , _description        :: Maybe Text
  }
  deriving (Generic, ToSchema)

instance FromJSON UpdatePersonReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance Transform UpdatePersonReq SP.Person where
  transform req person = person {
    SP._firstName = ifJust (req ^^. _firstName) (person ^^. SP._firstName)
    , SP._middleName = ifJust (req ^^. _middleName) (person ^^. SP._middleName)
    , SP._lastName           = ifJust (req ^^.  _lastName) (person ^^. SP._lastName)
    , SP._fullName           = ifJust (req ^^.  _fullName) (person ^^. SP._fullName)
    , SP._role               = ifJustExtract (req ^^.  _role) (person ^^. SP._role)
    , SP._gender             = ifJustExtract (req ^^.  _gender) (person ^^. SP._gender)
    , SP._email              = ifJust (req ^^.  _email) (person ^^. SP._email)
    , SP._mobileNumber       = ifJust (req ^^.  _mobileNumber) (person ^^. SP._mobileNumber)
    , SP._mobileCountryCode  = ifJust (req ^^.  _mobileCountryCode) (person ^^. SP._mobileCountryCode)
    , SP._identifier         = ifJust (req ^^.  _identifier) (person ^^. SP._identifier)
    , SP._rating             = ifJust (req ^^.  _rating) (person ^^. SP._rating)
    , SP._deviceToken        = ifJust (req ^^.  _deviceToken) (person ^^. SP._deviceToken)
    , SP._udf1               = ifJust (req ^^.  _udf1) (person ^^. SP._udf1)
    , SP._udf2               = ifJust (req ^^.  _udf2) (person ^^. SP._udf2)
    , SP._organizationId     = ifJust (req ^^.  _organizationId) (person ^^. SP._organizationId)
    , SP._description        = ifJust (req ^^.  _description) (person ^^. SP._description)
  }

ifJust a b = if isJust a then a else b
ifJustExtract a b = fromMaybe b a

(^^.) f g = g f

data UpdatePersonRes = UpdatePersonRes
  {  user :: SP.Person }
  deriving (Generic, ToJSON, ToSchema)