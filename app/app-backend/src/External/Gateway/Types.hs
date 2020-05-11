{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module External.Gateway.Types where

import qualified Beckn.Types.API.Confirm as Confirm
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] Confirm.ConfirmReq :> Post '[JSON] Confirm.ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

confirm req =
  void $ ET.client confirmAPI req
