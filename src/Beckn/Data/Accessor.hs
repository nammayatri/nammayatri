{-# LANGUAGE TemplateHaskell #-}
module Beckn.Data.Accessor where

import           Beckn.Types.API.Organization
import           Beckn.Types.API.PassApplication
import           Beckn.Types.API.Registration
import           EulerHS.Prelude

makeFieldsNoPrefix 'LoginReq
makeFieldsNoPrefix 'InitiateLoginReq
makeFieldsNoPrefix 'ReInitiateLoginReq
makeFieldsNoPrefix 'CreateOrganizationReq
