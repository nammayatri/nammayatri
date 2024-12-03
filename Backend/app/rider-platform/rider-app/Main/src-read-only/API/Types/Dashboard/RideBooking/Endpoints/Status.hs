{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Status where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.FRFSTicketBooking
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("status" :> GetStatusGetFRFSTicketStatus)

type GetStatusGetFRFSTicketStatus =
  ( "getFRFSTicketStatus" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> Get
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

newtype StatusAPIs = StatusAPIs {getStatusGetFRFSTicketStatus :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess}

mkStatusAPIs :: (Client EulerHS.Types.EulerClient API -> StatusAPIs)
mkStatusAPIs statusClient = (StatusAPIs {..})
  where
    getStatusGetFRFSTicketStatus = statusClient

data StatusUserActionType
  = GET_STATUS_GET_FRFS_TICKET_STATUS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON StatusUserActionType where
  toJSON GET_STATUS_GET_FRFS_TICKET_STATUS = Data.Aeson.String "GET_STATUS_GET_FRFS_TICKET_STATUS"

instance FromJSON StatusUserActionType where
  parseJSON (Data.Aeson.String "GET_STATUS_GET_FRFS_TICKET_STATUS") = pure GET_STATUS_GET_FRFS_TICKET_STATUS
  parseJSON _ = fail "GET_STATUS_GET_FRFS_TICKET_STATUS expected"

$(Data.Singletons.TH.genSingletons [''StatusUserActionType])
