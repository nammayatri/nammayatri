{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCab where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.SharedCabSession
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data AffectedRider = AffectedRider {bookingId :: Kernel.Prelude.Text, dropStop :: Kernel.Prelude.Text, firstName :: Kernel.Prelude.Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelectRouteMode
  = AfterLastDrop
  | Force
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

data SelectRouteResp = SelectRouteResp {affectedRiders :: Kernel.Prelude.Maybe [AffectedRider], session :: Kernel.Prelude.Maybe Domain.Types.SharedCabSession.SharedCabSession}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabBookingOperation
  = ACCEPT
  | DECLINE
  | CANCEL
  | BOARDED_WITHOUT_CODE
  | DROPPED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabBookingOperationReq = SharedCabBookingOperationReq {operation :: SharedCabBookingOperation, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabDemandReq = SharedCabDemandReq {routeCodes :: [Kernel.Prelude.Text]}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabMessageReq = SharedCabMessageReq {message :: Kernel.Prelude.Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRouteOperation
  = SELECT_ROUTE
  | RETURN_TRIP
  | CHANGE_ROUTE
  | END_ROUTE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRouteOperationReq = SharedCabRouteOperationReq
  { atLastStop :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    forceEnd :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    mode :: Kernel.Prelude.Maybe SelectRouteMode,
    operation :: SharedCabRouteOperation,
    routeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    walkupCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

data SharedCabSessionOperation
  = SET_SEATS
  | RESUME
  | CAB_FULL
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabSessionOperationReq = SharedCabSessionOperationReq {operation :: SharedCabSessionOperation, version :: Kernel.Prelude.Maybe Kernel.Prelude.Int, walkupCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
