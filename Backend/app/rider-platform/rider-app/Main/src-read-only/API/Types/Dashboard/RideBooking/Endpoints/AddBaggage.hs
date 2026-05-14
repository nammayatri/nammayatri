{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.AddBaggage where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.Booking
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AddBaggageConfirmReq = AddBaggageConfirmReq {numberOfLuggages :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("rideBooking" :> PostAddBaggageConfirm)

type PostAddBaggageConfirm =
  ( "addBaggage" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
      :> Capture
           "customerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> "confirm"
      :> ReqBody ('[JSON]) AddBaggageConfirmReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

newtype AddBaggageAPIs = AddBaggageAPIs {postAddBaggageConfirm :: (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> AddBaggageConfirmReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}

mkAddBaggageAPIs :: (Client EulerHS.Types.EulerClient API -> AddBaggageAPIs)
mkAddBaggageAPIs addBaggageClient = (AddBaggageAPIs {..})
  where
    postAddBaggageConfirm = addBaggageClient

data AddBaggageUserActionType
  = POST_ADD_BAGGAGE_CONFIRM
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON AddBaggageUserActionType where
  toJSON (POST_ADD_BAGGAGE_CONFIRM) = Data.Aeson.String "POST_ADD_BAGGAGE_CONFIRM"

instance FromJSON AddBaggageUserActionType where
  parseJSON (Data.Aeson.String "POST_ADD_BAGGAGE_CONFIRM") = pure POST_ADD_BAGGAGE_CONFIRM
  parseJSON _ = fail "POST_ADD_BAGGAGE_CONFIRM expected"

$(Data.Singletons.TH.genSingletons [(''AddBaggageUserActionType)])
