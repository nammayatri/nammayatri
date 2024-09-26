{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.TicketService where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Ticket.Interface.Types
import qualified Kernel.External.Ticket.Kapture.Types
import Kernel.Types.Common
import Servant
import Servant.Client

type API = ("ticketService" :> PostTicketServiceCreateKaptureTicket)

type PostTicketServiceCreateKaptureTicket =
  ( "createKaptureTicket" :> ReqBody ('[JSON]) Kernel.External.Ticket.Interface.Types.CreateTicketReq
      :> Post
           ('[JSON])
           Kernel.External.Ticket.Kapture.Types.CreateTicketResp
  )

newtype TicketServiceAPIs = TicketServiceAPIs {postTicketServiceCreateKaptureTicket :: (Kernel.External.Ticket.Interface.Types.CreateTicketReq -> EulerHS.Types.EulerClient Kernel.External.Ticket.Kapture.Types.CreateTicketResp)}

mkTicketServiceAPIs :: (Client EulerHS.Types.EulerClient API -> TicketServiceAPIs)
mkTicketServiceAPIs ticketServiceClient = (TicketServiceAPIs {..})
  where
    postTicketServiceCreateKaptureTicket = ticketServiceClient

data TicketServiceEndpointDSL
  = PostTicketServiceCreateKaptureTicketEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON TicketServiceEndpointDSL where
  toJSON (PostTicketServiceCreateKaptureTicketEndpoint) = Data.Aeson.String "PostTicketServiceCreateKaptureTicketEndpoint"

instance FromJSON TicketServiceEndpointDSL where
  parseJSON (Data.Aeson.String "PostTicketServiceCreateKaptureTicketEndpoint") = pure PostTicketServiceCreateKaptureTicketEndpoint
  parseJSON _ = fail "PostTicketServiceCreateKaptureTicketEndpoint expected"
