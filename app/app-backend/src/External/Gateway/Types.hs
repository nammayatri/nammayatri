{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module External.Gateway.Types where

import EulerHS.Prelude
import qualified Beckn.Types.API.Confirm as Confirm
import Servant

type ConfirmAPIs =
  "confirm" :> ReqBody '[JSON] Confirm.ConfirmReq :> Post '[JSON] Confirm.ConfirmRes
