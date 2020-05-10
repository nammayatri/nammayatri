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
import Beckn.TypeClass.Transform

data TransporterReq = TransporterReq
  { _name :: Text
  , _description :: Maybe Text
  , _mobileNumber :: Maybe Text
  , _gstin :: Maybe Text
  , _orgType :: SO.OrganizationType
  , _fromTime :: Maybe LocalTime
  , _toTime :: Maybe LocalTime
  , _headCount :: Maybe Int
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
    , SO._name = req ^^. _name
    , SO._description = req ^^. _description
    , SO._mobileNumber = req ^^. _mobileNumber
    , SO._gstin = req ^^. _gstin
    , SO._locationId = Nothing
    , SO._type = req ^^. _orgType
    , SO._fromTime = req ^^. _fromTime
    , SO._toTime = req ^^. _toTime
    , SO._headCount = req ^^. _headCount
    , SO._apiKey = Nothing
    , SO._callbackUrl = Nothing
    , SO._status = SO.PENDING_VERIFICATION
    , SO._verified = False
    , SO._createdAt = now
    , SO._updatedAt = now
    }

(^^.) f g = g f

data TransporterRes = TransporterRes
  { user :: SP.Person
  , organization :: SO.Organization
  }
  deriving (Generic, ToJSON, ToSchema)