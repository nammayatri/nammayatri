{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Cancel where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Cancel
import qualified "this" Domain.Types.Booking
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("rideBooking" :> PostCancelBooking)

type PostCancelBooking =
  ( "cancel" :> Capture "rideBookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
      :> Capture
           "customerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> ReqBody '[JSON] Domain.Action.UI.Cancel.CancelReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

newtype CancelAPIs = CancelAPIs {postCancelBooking :: Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Cancel.CancelReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess}

mkCancelAPIs :: (Client EulerHS.Types.EulerClient API -> CancelAPIs)
mkCancelAPIs cancelClient = (CancelAPIs {..})
  where
    postCancelBooking = cancelClient

data CancelUserActionType
  = POST_CANCEL_BOOKING
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON CancelUserActionType where
  toJSON POST_CANCEL_BOOKING = Data.Aeson.String "POST_CANCEL_BOOKING"

instance FromJSON CancelUserActionType where
  parseJSON (Data.Aeson.String "POST_CANCEL_BOOKING") = pure POST_CANCEL_BOOKING
  parseJSON _ = fail "POST_CANCEL_BOOKING expected"

$(Data.Singletons.TH.genSingletons [''CancelUserActionType])
