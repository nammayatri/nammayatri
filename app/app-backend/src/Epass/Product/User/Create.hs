{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Epass.Product.User.Create where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common (withFlowHandler)
import Data.Aeson
import Data.Time
import Epass.Product.Common
import qualified Epass.Storage.Queries.Location as Location
import Epass.Types.API.Common
import Epass.Types.API.User
import qualified Epass.Types.Storage.Location as Location
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as Person
import Utils.Common

create :: Maybe Text -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} = withFlowHandler $ do
  verifyToken regToken
  loc <- Location.findLocationWithErr _locationId
  user <- userInfo id
  Person.create user
  eres <- Person.findById (user ^. #_id)
  case eres of
    Nothing -> L.throwException $ err500 {errBody = "Couldnt find user"}
    Just user -> do
      locInfo <- getLocationInfo _locationId
      return $ mkUInfo user locInfo
  where
    userInfo id = do
      id <- L.generateGUID -- TODO: use GUID typeclass instance
      now <- getCurrTime
      return $
        Person.Person
          { _id = PersonId id,
            _firstName = Nothing,
            _middleName = Nothing,
            _lastName = Nothing,
            _fullName = Just $ _name,
            _role = _role,
            _gender = Person.UNKNOWN,
            _identifierType = Person.MOBILENUMBER,
            _email = Nothing,
            _mobileNumber = Just $ _mobileNumber,
            _mobileCountryCode = _mobileCountryCode,
            _identifier = Just $ _mobileNumber,
            _rating = Nothing,
            _verified = False,
            _udf1 = Nothing,
            _udf2 = Nothing,
            _status = Person.INACTIVE,
            _organizationId = Just $ _getOrganizationId _organizationId,
            _locationId = Just $ _locationId,
            _deviceToken = Nothing,
            _description = Nothing,
            _createdAt = now,
            _updatedAt = now
          }
