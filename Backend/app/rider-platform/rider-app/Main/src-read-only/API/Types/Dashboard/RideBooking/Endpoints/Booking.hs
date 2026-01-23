{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Booking where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Booking
import qualified "this" Domain.Types.Booking
import qualified "this" Domain.Types.Booking.API
import qualified Domain.Types.BookingStatus
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("booking" :> (PostBookingStatus :<|> GetBookingBooking :<|> GetBookingList :<|> GetBookingAgentL1ListHelper :<|> GetBookingAgentL2List))

type PostBookingStatus =
  ( "ridebooking" :> Capture "rideBookingId" (Kernel.Types.Id.Id Domain.Types.Booking.Booking)
      :> Capture
           "customerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> Post '[JSON] Domain.Types.Booking.API.BookingAPIEntity
  )

type GetBookingBooking = (Capture "bookingCode" Kernel.Prelude.Text :> "booking" :> Get '[JSON] Domain.Types.Booking.API.BookingAPIEntity)

type GetBookingList =
  ( "list" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> QueryParam "limit" EulerHS.Prelude.Integer
      :> QueryParam
           "offset"
           EulerHS.Prelude.Integer
      :> QueryParam "onlyActive" Kernel.Prelude.Bool
      :> QueryParam
           "status"
           Domain.Types.BookingStatus.BookingStatus
      :> Get
           '[JSON]
           Domain.Action.UI.Booking.BookingListRes
  )

type GetBookingAgentL1List =
  ( "agentL1" :> "list" :> QueryParam "limit" EulerHS.Prelude.Integer :> QueryParam "offset" EulerHS.Prelude.Integer
      :> QueryParam
           "status"
           Domain.Types.BookingStatus.BookingStatus
      :> QueryParam "customerPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "fromDate"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "toDate"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           Domain.Action.UI.Booking.BookingListRes
  )

type GetBookingAgentL1ListHelper =
  ( "agentL1" :> "list" :> QueryParam "agentId" Kernel.Prelude.Text :> QueryParam "limit" EulerHS.Prelude.Integer
      :> QueryParam
           "offset"
           EulerHS.Prelude.Integer
      :> QueryParam "status" Domain.Types.BookingStatus.BookingStatus
      :> QueryParam
           "customerPhoneNo"
           Kernel.Prelude.Text
      :> QueryParam
           "fromDate"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "toDate"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           Domain.Action.UI.Booking.BookingListRes
  )

type GetBookingAgentL2List =
  ( "agentL2" :> "list" :> QueryParam "limit" EulerHS.Prelude.Integer :> QueryParam "offset" EulerHS.Prelude.Integer
      :> QueryParam
           "status"
           Domain.Types.BookingStatus.BookingStatus
      :> QueryParam "customerPhoneNo" Kernel.Prelude.Text
      :> QueryParam
           "fromDate"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "toDate"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           Domain.Action.UI.Booking.BookingListRes
  )

data BookingAPIs = BookingAPIs
  { postBookingStatus :: Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient Domain.Types.Booking.API.BookingAPIEntity,
    getBookingBooking :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient Domain.Types.Booking.API.BookingAPIEntity,
    getBookingList :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.BookingStatus.BookingStatus -> EulerHS.Types.EulerClient Domain.Action.UI.Booking.BookingListRes,
    getBookingAgentL1List :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Domain.Types.BookingStatus.BookingStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient Domain.Action.UI.Booking.BookingListRes,
    getBookingAgentL2List :: Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Domain.Types.BookingStatus.BookingStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient Domain.Action.UI.Booking.BookingListRes
  }

mkBookingAPIs :: (Client EulerHS.Types.EulerClient API -> BookingAPIs)
mkBookingAPIs bookingClient = (BookingAPIs {..})
  where
    postBookingStatus :<|> getBookingBooking :<|> getBookingList :<|> getBookingAgentL1List :<|> getBookingAgentL2List = bookingClient

data BookingUserActionType
  = POST_BOOKING_STATUS
  | GET_BOOKING_BOOKING
  | GET_BOOKING_LIST
  | GET_BOOKING_AGENT_L1_LIST
  | GET_BOOKING_AGENT_L2_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''BookingUserActionType])
