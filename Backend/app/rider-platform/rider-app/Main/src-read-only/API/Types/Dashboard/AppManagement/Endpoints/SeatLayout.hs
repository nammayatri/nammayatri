{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.SeatLayout where

import qualified Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified "this" Domain.Types.Seat
import qualified "this" Domain.Types.SeatLayout
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data SeatDef = SeatDef
  { col :: Kernel.Prelude.Int,
    directionDegrees :: Data.Maybe.Maybe Kernel.Prelude.Int,
    isBookable :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    isLadiesOnly :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    label :: Data.Text.Text,
    minStopsRequired :: Data.Maybe.Maybe Kernel.Prelude.Int,
    row :: Kernel.Prelude.Int,
    seatType :: Data.Maybe.Maybe Domain.Types.Seat.SeatType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SeatItem = SeatItem
  { col :: Kernel.Prelude.Int,
    directionDegrees :: Data.Maybe.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.Seat.Seat,
    isBookable :: Kernel.Prelude.Bool,
    isLadiesOnly :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    label :: Data.Text.Text,
    minStopsRequired :: Data.Maybe.Maybe Kernel.Prelude.Int,
    row :: Kernel.Prelude.Int,
    seatType :: Data.Maybe.Maybe Domain.Types.Seat.SeatType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SeatLayoutDetailResp = SeatLayoutDetailResp {columns :: Kernel.Prelude.Int, id :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout, name :: Data.Text.Text, rows :: Kernel.Prelude.Int, seats :: [SeatItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SeatLayoutListItem = SeatLayoutListItem {columns :: Kernel.Prelude.Int, id :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout, name :: Data.Text.Text, rows :: Kernel.Prelude.Int, seatCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SeatLayoutUpsertReq = SeatLayoutUpsertReq {columns :: Kernel.Prelude.Int, name :: Data.Text.Text, rows :: Kernel.Prelude.Int, seatLayoutId :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout, seats :: [SeatDef]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SeatLayoutUpsertReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("seatLayout" :> (UpsertSeatLayout :<|> ListSeatLayout :<|> GetSeatLayout :<|> DeleteSeatLayout))

type UpsertSeatLayout = ("upsert" :> ReqBody '[JSON] SeatLayoutUpsertReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type ListSeatLayout = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get '[JSON] [SeatLayoutListItem])

type GetSeatLayout = (Capture "seatLayoutId" (Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout) :> Get '[JSON] SeatLayoutDetailResp)

type DeleteSeatLayout = (Capture "seatLayoutId" (Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout) :> "delete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

data SeatLayoutAPIs = SeatLayoutAPIs
  { upsertSeatLayout :: SeatLayoutUpsertReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    listSeatLayout :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient [SeatLayoutListItem],
    getSeatLayout :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> EulerHS.Types.EulerClient SeatLayoutDetailResp,
    deleteSeatLayout :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkSeatLayoutAPIs :: (Client EulerHS.Types.EulerClient API -> SeatLayoutAPIs)
mkSeatLayoutAPIs seatLayoutClient = (SeatLayoutAPIs {..})
  where
    upsertSeatLayout :<|> listSeatLayout :<|> getSeatLayout :<|> deleteSeatLayout = seatLayoutClient

data SeatLayoutUserActionType
  = UPSERT_SEAT_LAYOUT
  | LIST_SEAT_LAYOUT
  | GET_SEAT_LAYOUT
  | DELETE_SEAT_LAYOUT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''SeatLayoutUserActionType])
