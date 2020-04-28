{-# Language TemplateHaskell #-}
module Beckn.Data.Accessor where

import EulerHS.Prelude
import Beckn.Types.API.PassApplication
import Beckn.Types.API.Registration

makeFieldsNoPrefix 'LoginReq
makeFieldsNoPrefix 'InitiateLoginReq
