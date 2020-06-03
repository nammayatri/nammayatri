{-# LANGUAGE TemplateHaskell #-}

module Data.Accessor where

import EulerHS.Prelude
import Types.API.Registration

makeFieldsNoPrefix 'LoginReq

makeFieldsNoPrefix 'InitiateLoginReq

makeFieldsNoPrefix 'ReInitiateLoginReq
