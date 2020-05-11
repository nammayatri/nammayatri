{-# LANGUAGE OverloadedLabels      #-}

module Types.API.Transporter where

import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.Location as SL
import qualified Storage.Queries.Location as QL
import Beckn.Types.Common as BC
import Data.Swagger
import Data.Time.LocalTime
import EulerHS.Prelude
import Servant.Swagger
import Beckn.Utils.Extra
import Beckn.Utils.Common
import Data.Generics.Labels
import Beckn.Types.App
import Beckn.TypeClass.Transform
import qualified EulerHS.Language as L

data TransporterReq = TransporterReq
  { _name :: Text
  , _description :: Maybe Text
  , _mobileNumber :: Maybe Text
  , _gstin :: Maybe Text
  , _orgType :: SO.OrganizationType
  , _fromTime :: Maybe LocalTime
  , _toTime :: Maybe LocalTime
  , _headCount :: Maybe Int
  , _locationType :: Maybe SL.LocationType
  , _lat :: Maybe Double
  , _long :: Maybe Double
  , _ward :: Maybe Text
  , _district :: Maybe Text
  , _city :: Maybe Text
  , _state :: Maybe Text
  , _country :: Maybe Text
  , _pincode :: Maybe Text
  , _address :: Maybe Text
  , _bound :: Maybe Text
  }
  deriving (Generic, ToSchema)

instance FromJSON TransporterReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance Transform TransporterReq SO.Organization where
  transformFlow req = do
    id <- BC.generateGUID
    now <- getCurrentTimeUTC
    location <- transformToLocation req
    QL.create location
    return $ SO.Organization {
    SO._id = id
    , SO._name = req ^. #_name
    , SO._description = req ^. #_description
    , SO._mobileNumber = req ^. #_mobileNumber
    , SO._gstin = req ^. #_gstin
    , SO._locationId = Just (_getLocationId $ SL._id location)
    , SO._type = req ^. #_orgType
    , SO._fromTime = req ^. #_fromTime
    , SO._toTime = req ^. #_toTime
    , SO._headCount = req ^. #_headCount
    , SO._apiKey = Nothing
    , SO._callbackUrl = Nothing
    , SO._status = SO.PENDING_VERIFICATION
    , SO._verified = False
    , SO._createdAt = now
    , SO._updatedAt = now
    }

transformToLocation :: TransporterReq -> L.Flow SL.Location
transformToLocation req = do
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

data TransporterRes = TransporterRes
  { user :: SP.Person
  , organization :: SO.Organization
  }
  deriving (Generic, ToJSON, ToSchema)

data GatewayRes = GatewayRes
  { organization :: SO.Organization
  }
  deriving (Generic, ToJSON, ToSchema)