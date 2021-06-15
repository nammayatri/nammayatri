{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ExternalAPI.Types where

import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import Servant
import Types.API.Location

type LocationAPI =
  "location"
    :> Capture "caseId" Text
    :> Get '[JSON] GetLocationRes

locationAPI :: Proxy LocationAPI
locationAPI = Proxy

location :: Text -> EulerClient GetLocationRes
location = client locationAPI
