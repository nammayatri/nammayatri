{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.MultiModal (getMultiModalList) where

import Data.Aeson
import Data.Aeson (eitherDecode)
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Action.UI.Booking as DBooking
import qualified "this" Domain.Action.UI.Booking
import qualified "this" Domain.Types.Booking.API
import qualified Domain.Types.BookingStatus
import qualified "this" Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import SharedLogic.Merchant (findMerchantByShortId)
import Tools.Auth

getMultiModalList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe [Domain.Types.BookingStatus.BookingStatus] -> Kernel.Prelude.Maybe [Domain.Types.Journey.JourneyStatus] -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Booking.API.BookingRequestType -> Environment.Flow Domain.Action.UI.Booking.BookingListResV2)
getMultiModalList merchantShortId _opCity customerId limit offset bookingOffset journeyOffset fromDate toDate rideStatus journeyStatus isPaymentSuccess bookingRequestType = do
  m <- findMerchantByShortId merchantShortId
  DBooking.bookingListV2 (customerId, m.id) limit offset bookingOffset journeyOffset fromDate toDate (fromMaybe [] rideStatus) (fromMaybe [] journeyStatus) isPaymentSuccess bookingRequestType

instance FromHttpApiData [Domain.Types.Journey.JourneyStatus] where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader bs = BF.first T.pack . eitherDecode . BSL.fromStrict $ bs

instance ToHttpApiData [Domain.Types.Journey.JourneyStatus] where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode
