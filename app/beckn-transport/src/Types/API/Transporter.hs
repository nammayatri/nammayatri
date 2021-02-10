{-# LANGUAGE OverloadedLabels #-}

module Types.API.Transporter where

import App.Types
import Beckn.TypeClass.Transform
import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Location as SL
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude
import qualified Storage.Queries.Location as QL

data TransporterReq = TransporterReq
  { _name :: Text,
    _description :: Maybe Text,
    _mobileNumber :: Maybe Text,
    _mobileCountryCode :: Maybe Text,
    _gstin :: Maybe Text,
    _orgType :: SO.OrganizationType,
    _orgDomain :: Maybe SO.OrganizationDomain,
    _fromTime :: Maybe UTCTime,
    _toTime :: Maybe UTCTime,
    _headCount :: Maybe Int,
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

instance FromJSON TransporterReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance CreateTransform TransporterReq SO.Organization Flow where
  createTransform req = do
    oid <- BC.generateGUID
    let shortId = ShortOrganizationId $ _getOrganizationId oid
    now <- getCurrTime
    location <- transformToLocation req
    QL.create location
    return $
      SO.Organization
        { SO._id = oid,
          SO._name = req ^. #_name,
          SO._shortId = shortId,
          SO._description = req ^. #_description,
          SO._mobileNumber = req ^. #_mobileNumber,
          SO._mobileCountryCode = req ^. #_mobileCountryCode,
          SO._gstin = req ^. #_gstin,
          SO._locationId = Just (_getLocationId $ SL._id location),
          SO._type = req ^. #_orgType,
          SO._domain = req ^. #_orgDomain,
          SO._fromTime = req ^. #_fromTime,
          SO._toTime = req ^. #_toTime,
          SO._headCount = req ^. #_headCount,
          SO._apiKey = Nothing,
          SO._callbackUrl = Nothing,
          SO._status = SO.PENDING_VERIFICATION,
          SO._verified = False,
          SO._enabled = True,
          SO._createdAt = now,
          SO._updatedAt = now,
          SO._callbackApiKey = Nothing,
          SO._info = Nothing
        }

transformToLocation :: TransporterReq -> Flow SL.Location
transformToLocation req = do
  locId <- BC.generateGUID
  now <- getCurrTime
  return $
    SL.Location
      { SL._id = locId,
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

data TransporterRes = TransporterRes
  { user :: SP.Person,
    organization :: SO.Organization
  }
  deriving (Generic, ToJSON)

newtype TransporterRec = TransporterRec
  { organization :: SO.Organization
  }
  deriving (Generic, ToJSON)

data UpdateTransporterReq = UpdateTransporterReq
  { name :: Maybe Text,
    description :: Maybe Text,
    headCount :: Maybe Int,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON)

instance ModifyTransform UpdateTransporterReq SO.Organization Flow where
  modifyTransform req org = do
    now <- getCurrTime
    return $
      org
        { SO._name = fromMaybe (org ^. #_name) (req ^. #name),
          SO._description = (req ^. #description) <|> (org ^. #_description),
          SO._headCount = (req ^. #headCount) <|> (org ^. #_headCount),
          SO._enabled = fromMaybe (org ^. #_enabled) (req ^. #enabled),
          SO._updatedAt = now
        }
