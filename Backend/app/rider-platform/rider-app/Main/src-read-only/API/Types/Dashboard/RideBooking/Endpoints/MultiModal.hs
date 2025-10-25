{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.MultiModal where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Booking
import qualified "this" Domain.Types.Booking.API
import qualified "beckn-spec" Domain.Types.BookingStatus
import qualified "this" Domain.Types.Journey
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client

type API = ("multiModal" :> GetMultiModalList)

type GetMultiModalList =
  ( "list" :> QueryParam "limit" EulerHS.Prelude.Integer :> QueryParam "offset" EulerHS.Prelude.Integer
      :> QueryParam
           "bookingOffset"
           EulerHS.Prelude.Integer
      :> QueryParam "journeyOffset" EulerHS.Prelude.Integer
      :> QueryParam "customerPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "countryCode"
           Kernel.Prelude.Text
      :> QueryParam
           "email"
           Kernel.Prelude.Text
      :> QueryParam
           "fromDate"
           EulerHS.Prelude.Integer
      :> QueryParam
           "toDate"
           EulerHS.Prelude.Integer
      :> QueryParam
           "rideStatus"
           [Domain.Types.BookingStatus.BookingStatus]
      :> QueryParam
           "journeyStatus"
           [Domain.Types.Journey.JourneyStatus]
      :> QueryParam
           "isPaymentSuccess"
           Kernel.Prelude.Bool
      :> QueryParam
           "bookingRequestType"
           Domain.Types.Booking.API.BookingRequestType
      :> QueryParam
           "customerId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           Domain.Action.UI.Booking.BookingListResV2
  )

newtype MultiModalAPIs = MultiModalAPIs {getMultiModalList :: Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe [Domain.Types.BookingStatus.BookingStatus] -> Kernel.Prelude.Maybe [Domain.Types.Journey.JourneyStatus] -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Booking.API.BookingRequestType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Domain.Action.UI.Booking.BookingListResV2}

mkMultiModalAPIs :: (Client EulerHS.Types.EulerClient API -> MultiModalAPIs)
mkMultiModalAPIs multiModalClient = (MultiModalAPIs {..})
  where
    getMultiModalList = multiModalClient

data MultiModalUserActionType
  = GET_MULTI_MODAL_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON MultiModalUserActionType where
  toJSON GET_MULTI_MODAL_LIST = Data.Aeson.String "GET_MULTI_MODAL_LIST"

instance FromJSON MultiModalUserActionType where
  parseJSON (Data.Aeson.String "GET_MULTI_MODAL_LIST") = pure GET_MULTI_MODAL_LIST
  parseJSON _ = fail "GET_MULTI_MODAL_LIST expected"

$(Data.Singletons.TH.genSingletons [''MultiModalUserActionType])
