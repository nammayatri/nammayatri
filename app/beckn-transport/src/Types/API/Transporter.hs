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
import Data.Generics.Labels
import Beckn.TypeClass.Transform

data TransporterReq = TransporterReq
  { name :: Text
  , description :: Maybe Text
  , mobileNumber :: Maybe Text
  , gstin :: Maybe Text
  , orgType :: SO.OrganizationType
  , fromTime :: Maybe LocalTime
  , toTime :: Maybe LocalTime
  , headCount :: Maybe Int
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
    , SO._name = req ^. #name
    , SO._description = req ^. #description
    , SO._mobileNumber = req ^. #mobileNumber
    , SO._gstin = req ^. #gstin
    , SO._locationId = Nothing
    , SO._type = req ^. #orgType
    , SO._fromTime = req ^. #fromTime
    , SO._toTime = req ^. #toTime
    , SO._headCount = req ^. #headCount
    , SO._apiKey = Nothing
    , SO._callbackUrl = Nothing
    , SO._status = SO.PENDING_VERIFICATION
    , SO._verified = False
    , SO._createdAt = now
    , SO._updatedAt = now
    }

data TransporterRes = TransporterRes
  { user :: SP.Person
  , organization :: SO.Organization
  }
  deriving (Generic, ToJSON, ToSchema)