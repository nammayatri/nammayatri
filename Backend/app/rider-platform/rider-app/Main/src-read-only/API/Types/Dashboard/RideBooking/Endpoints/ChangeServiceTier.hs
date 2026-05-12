{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.ChangeServiceTier where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Quote
import qualified "this" Domain.Types.Booking
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Quote
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ChangeServiceTierConfirmReq = ChangeServiceTierConfirmReq {quoteId :: Kernel.Types.Id.Id Domain.Types.Quote.Quote}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("rideBooking" :> (GetChangeServiceTierQuotes :<|> PostChangeServiceTierConfirm))

type GetChangeServiceTierQuotes =
  ( "changeServiceTier" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
      :> Capture
           "customerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> "quotes"
      :> Get ('[JSON]) Domain.Action.UI.Quote.GetQuotesRes
  )

type PostChangeServiceTierConfirm =
  ( "changeServiceTier" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
      :> Capture
           "customerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> "confirm"
      :> ReqBody ('[JSON]) ChangeServiceTierConfirmReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

data ChangeServiceTierAPIs = ChangeServiceTierAPIs
  { getChangeServiceTierQuotes :: (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient Domain.Action.UI.Quote.GetQuotesRes),
    postChangeServiceTierConfirm :: (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> ChangeServiceTierConfirmReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkChangeServiceTierAPIs :: (Client EulerHS.Types.EulerClient API -> ChangeServiceTierAPIs)
mkChangeServiceTierAPIs changeServiceTierClient = (ChangeServiceTierAPIs {..})
  where
    getChangeServiceTierQuotes :<|> postChangeServiceTierConfirm = changeServiceTierClient

data ChangeServiceTierUserActionType
  = GET_CHANGE_SERVICE_TIER_QUOTES
  | POST_CHANGE_SERVICE_TIER_CONFIRM
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''ChangeServiceTierUserActionType)])
