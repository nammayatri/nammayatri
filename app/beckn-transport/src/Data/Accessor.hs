{-# LANGUAGE TemplateHaskell #-}
module Data.Accessor where

import           Types.API.Registration
import           EulerHS.Prelude

makeFieldsNoPrefix 'LoginReq
makeFieldsNoPrefix 'InitiateLoginReq
makeFieldsNoPrefix 'ReInitiateLoginReq