{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Epass.Product.User.Update where

import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Data.Aeson
import qualified Data.List as List
import Data.Time
import Epass.Product.Common
import qualified Epass.Storage.Queries.Location as Location
import qualified Epass.Storage.Queries.Organization as Org
import Epass.Types.API.Common
import Epass.Types.API.User
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Location as Location
import qualified Epass.Types.Storage.Organization as Org
import Epass.Utils.Common
import Epass.Utils.Extra
import Epass.Utils.Routes
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as Person

update :: Maybe Text -> PersonId -> UpdateReq -> FlowHandler UpdateRes
update regToken userId UpdateReq {..} = withFlowHandler $ do
  verifyToken regToken
  Person.update userId _status _name _email _role
  Person.findById userId
    >>= fromMaybeM500 "Couldnot find user"
    >>= return . UpdateRes

delete :: Maybe RegistrationTokenText -> PersonId -> FlowHandler Ack
delete regToken userId = withFlowHandler $ do
  verifyToken regToken
  Person.deleteById userId
  sendAck
