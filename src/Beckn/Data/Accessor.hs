{-# Language TemplateHaskell #-}
module Beckn.Data.Accessor where

import EulerHS.Prelude
import Beckn.Types.API.PassApplication
import Beckn.Types.API.Registration
import Beckn.Types.API.Organization

makeFieldsNoPrefix 'LoginReq
makeFieldsNoPrefix 'InitiateLoginReq
makeFieldsNoPrefix 'CreateOrganizationReq
