{-# LANGUAGE TemplateHaskell #-}
module Epass.Data.Accessor where

import           Epass.Types.API.Organization
import           Epass.Types.API.PassApplication
import           Epass.Types.API.Registration
import           EulerHS.Prelude

makeFieldsNoPrefix 'LoginReq
makeFieldsNoPrefix 'InitiateLoginReq
makeFieldsNoPrefix 'ReInitiateLoginReq
makeFieldsNoPrefix 'CreateOrganizationReq
