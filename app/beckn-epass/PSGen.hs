{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module PSGen where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Data.Text.IO                       as T
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Servant.API
import           Servant.PureScript
import           Servant.Subscriber.Subscribable

import           Beckn.App.Routes
import           Beckn.Types.API.Registration
import qualified Beckn.Types.API.Blacklist as Blacklist
import qualified Beckn.Types.API.Comment as Comment
import qualified Beckn.Types.API.Customer as Customer
import qualified Beckn.Types.API.Document as Document
import qualified Beckn.Types.API.Organization as Organization
import qualified Beckn.Types.API.Pass as Pass
import qualified Beckn.Types.API.PassApplication as PA
import qualified Beckn.Types.API.Quota as Quota
import qualified Beckn.Types.API.Tag as Tag
import qualified Beckn.Types.API.User as User
--import           EulerHS.Prelude

-- | We have been lazy and defined our types in the WebAPI module,
--   we use this opportunity to show how to create a custom bridge moving those
--   types to Counter.ServerTypes.
fixTypesModule :: BridgePart
fixTypesModule = do
  typeModule ^== "Beckn.App.Routes"
  t <- view haskType
  TypeInfo (_typePackage t) "Beckn.ServerTypes" (_typeName t) <$> psTypeParameters

myBridge :: BridgePart
myBridge = defaultBridge <|> fixTypesModule

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge


myTypes :: [SumType 'Haskell]
myTypes =  [
            mkSumType (Proxy :: Proxy LoginReq)
          , mkSumType (Proxy :: Proxy LoginRes)
          , mkSumType (Proxy :: Proxy InitiateLoginReq)
          , mkSumType (Proxy :: Proxy InitiateLoginRes)
          , mkSumType (Proxy :: Proxy ReInitiateLoginReq)
          , mkSumType (Proxy :: Proxy Blacklist.CreateReq)
          , mkSumType (Proxy :: Proxy Blacklist.CreateRes)
          , mkSumType (Proxy :: Proxy Blacklist.UpdateReq)
          , mkSumType (Proxy :: Proxy Blacklist.UpdateRes)
          , mkSumType (Proxy :: Proxy Blacklist.ListRes)
          , mkSumType (Proxy :: Proxy Blacklist.GetRes)
          , mkSumType (Proxy :: Proxy Comment.CreateReq)
          , mkSumType (Proxy :: Proxy Comment.CreateRes)
          , mkSumType (Proxy :: Proxy Comment.ListRes)
          , mkSumType (Proxy :: Proxy Customer.GetCustomerRes)
          , mkSumType (Proxy :: Proxy Document.DocumentRes)
          , mkSumType (Proxy :: Proxy Document.ListDocumentRes)
          , mkSumType (Proxy :: Proxy Organization.CreateOrganizationReq)
          , mkSumType (Proxy :: Proxy Organization.OrganizationRes)
          , mkSumType (Proxy :: Proxy Organization.ListOrganizationReq)
          , mkSumType (Proxy :: Proxy Organization.ListOrganizationRes)
          , mkSumType (Proxy :: Proxy Organization.UpdateOrganizationReq)
          , mkSumType (Proxy :: Proxy Pass.PassRes)
          , mkSumType (Proxy :: Proxy Pass.UpdatePassReq)
          , mkSumType (Proxy :: Proxy Pass.ListPassReq)
          , mkSumType (Proxy :: Proxy Pass.ListPassRes)
          , mkSumType (Proxy :: Proxy PA.CreatePassApplicationReq)
          , mkSumType (Proxy :: Proxy PA.PassApplicationRes)
          , mkSumType (Proxy :: Proxy PA.ListPassApplicationRes)
          , mkSumType (Proxy :: Proxy PA.UpdatePassApplicationReq)
          , mkSumType (Proxy :: Proxy Quota.UpdateReq)
          , mkSumType (Proxy :: Proxy Quota.UpdateRes)
          , mkSumType (Proxy :: Proxy Quota.ListRes)
          , mkSumType (Proxy :: Proxy Quota.CreateRes)
          , mkSumType (Proxy :: Proxy Quota.CreateReq)
          , mkSumType (Proxy :: Proxy Tag.ListRes)
          , mkSumType (Proxy :: Proxy Tag.CreateRes)
          , mkSumType (Proxy :: Proxy Tag.CreateReq)
          , mkSumType (Proxy :: Proxy Tag.TagEntityReq)
          , mkSumType (Proxy :: Proxy Tag.TagEntityRes)
          , mkSumType (Proxy :: Proxy User.UpdateReq)
          , mkSumType (Proxy :: Proxy User.UpdateRes)
          , mkSumType (Proxy :: Proxy User.ListRes)
          , mkSumType (Proxy :: Proxy User.GetRes)
          , mkSumType (Proxy :: Proxy User.CreateRes)
          , mkSumType (Proxy :: Proxy User.CreateReq)
          ]

mySettings :: Settings
mySettings = defaultSettings
--mySettings = (addReaderParam "AuthToken" defaultSettings & apiModuleName .~ ".WebAPI") {
  --_generateSubscriberAPI = True
  --}


main = do
  let frontEndRoot = "frontend/src"
  writeAPIModuleWithSettings mySettings frontEndRoot myBridgeProxy epassAPIs
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes
