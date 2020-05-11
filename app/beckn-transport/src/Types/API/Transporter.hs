{-# LANGUAGE OverloadedLabels      #-}

module Types.API.Transporter where

import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Types.Common as BC
import Data.Swagger
import Data.Time.LocalTime
import EulerHS.Prelude
import Servant.Swagger
import Beckn.Types.Common
import Beckn.Utils.Extra
import Beckn.Utils.Common
import Data.Generics.Labels
import Beckn.TypeClass.Transform
import qualified EulerHS.Language as L

data TransporterReq = TransporterReq
  { _name :: Text
  , _description :: Maybe Text
  , _mobileNumber :: Maybe Text
  , _gstin :: Maybe Text
  , _orgType :: SO.OrganizationType
  , _fromTime :: Maybe Text
  , _toTime :: Maybe Text
  , _headCount :: Maybe Int
  , _locationType :: Maybe Text
  , _lat :: Maybe Text
  , _long :: Maybe Text
  , _ward :: Maybe Text
  , _district :: Maybe Text
  , _city :: Maybe Text
  , _state :: Maybe Text
  , _country :: Maybe Text
  , _pincode :: Maybe Text
  , _address :: Maybe Text
  }
  deriving (Generic, ToSchema)

instance FromJSON TransporterReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance Transform TransporterReq SO.Organization where
  transformFlow req = do
    id <- BC.generateGUID
    now <- getCurrentTimeUTC
    return $ SO.Organization {
    SO._id = id
    , SO._name = req ^. #_name
    , SO._description = req ^. #_description
    , SO._mobileNumber = req ^. #_mobileNumber
    , SO._gstin = req ^. #_gstin
    , SO._locationId = Nothing
    , SO._type = req ^. #_orgType
    , SO._fromTime = textToMaybeLocalTime =<< req ^. #_fromTime
    , SO._toTime = textToMaybeLocalTime =<< req ^. #_toTime
    , SO._headCount = req ^. #_headCount
    , SO._apiKey = Nothing
    , SO._callbackUrl = Nothing
    , SO._status = SO.PENDING_VERIFICATION
    , SO._verified = False
    , SO._createdAt = now
    , SO._updatedAt = now
    }

-- instance Transform TransporterReq SP.Person where
--   transformFlow req = do
--     id <- BC.generateGUID
--     now <- getCurrentTimeUTC
--     return $ SP.Person {
--       SP._id = id
--     }

data TransporterRes = TransporterRes
  { user :: SP.Person
  , organization :: SO.Organization
  }
  deriving (Generic, ToJSON, ToSchema)