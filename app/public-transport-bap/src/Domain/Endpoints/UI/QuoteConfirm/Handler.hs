module Domain.Endpoints.UI.QuoteConfirm.Handler where

import API.UI.QuoteConfirm.Types
import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as D
import qualified Domain.Types.Quote as D
import qualified Storage.Queries.Booking as EsqBk
import Storage.Queries.Quote
import Tools.Auth


--import PublicTransportTestData

--handler _ _ _ = withFlowHandlerAPI (insertTestData >> pure (QConfirmRes "1"))

data ConfirmMessageD = ConfirmMessageD
  { txnId :: Text,
    quantity :: Int,
    requestorName :: Text,
    booking :: D.Booking,
    quote :: D.Quote
  }

validateConfirmReq :: QConfirmReq -> Flow ()
validateConfirmReq confirmReq = do
  when (confirmReq.quantity <= 0) $ throwError $ InvalidRequest "invalid quantity value"

quoteConfirm :: PersonId -> Id D.Quote -> QConfirmReq -> Flow (QConfirmRes, ConfirmMessageD)
quoteConfirm personId quoteId confirmReq = do
  validateConfirmReq confirmReq
  quote <- findById quoteId >>= fromMaybeM QuoteNotFound
  bookingId <- generateGUID
  let txnId = bookingId
  now <- getCurrentTime
  let booking = buildBooking now bookingId personId confirmReq quote
  _ <- Esq.runTransaction $ EsqBk.create booking
  --  callConfirm $ makeConfirmMessageD txnId confirmReq quote booking
  pure (QConfirmRes bookingId, makeConfirmMessageD txnId confirmReq quote booking)

makeConfirmMessageD :: Text -> QConfirmReq -> D.Quote -> D.Booking -> ConfirmMessageD
makeConfirmMessageD txnId qConfirmReq quote booking = do
  let quantity = qConfirmReq.quantity
      requestorName = qConfirmReq.requestorName
  ConfirmMessageD {..}

buildBooking :: UTCTime -> Text -> PersonId -> QConfirmReq -> D.Quote -> D.Booking
buildBooking now bookingId personId confirmReq quote = do
  let id = Id bookingId
      searchId = quote.searchId
      quoteId = quote.id
      bknTxnId = bookingId
      requestorId = personId
      quantity = confirmReq.quantity
      bppId = quote.bppId
      bppUrl = quote.bppUrl
      publicTransportSupportNumber = "support number" -- FIXME
      description = quote.description
      fare = quote.fare -- fare of one ticket
      departureTime = quote.departureTime
      arrivalTime = quote.arrivalTime
      departureStationId = quote.departureStationId
      arrivalStationId = quote.arrivalStationId
      status = D.NEW
      createdAt = now
      updatedAt = now
      ticketId = Nothing
      ticketCreatedAt = Nothing
  D.Booking {..}
