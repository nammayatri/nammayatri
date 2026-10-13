{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.TicketVerify where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.MultimodalTicketVerify
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data IntegratedQRReq = IntegratedQRReq {integratedQR :: Domain.Types.MultimodalTicketVerify.UnifiedTicketQRV2, provider :: Domain.Types.MultimodalTicketVerify.Provider}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultimodalTicketVerifyReq
  = IntegratedQR IntegratedQRReq
  | SingleQR SingleQRReq
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SingleQRReq = SingleQRReq {provider :: Domain.Types.MultimodalTicketVerify.Provider, tickets :: [Data.Text.Text]}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
