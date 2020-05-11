{-# LANGUAGE TemplateHaskell #-}

module Data.Accessor where

import EulerHS.Prelude
import Types.API.Registration
import Types.API.Person

makeFieldsNoPrefix 'LoginReq
makeFieldsNoPrefix 'InitiateLoginReq
makeFieldsNoPrefix 'ReInitiateLoginReq
makeFieldsNoPrefix 'UpdatePersonReq
