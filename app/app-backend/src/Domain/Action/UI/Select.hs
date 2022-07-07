{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Domain.Action.UI.Select where

import App.Types
import Beckn.Prelude
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchReqLocation as SRLoc
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Types.Error
import Utils.Common

data DSelectReq = DSelectReq
  { searchRequest :: DSearchReq.SearchRequest,
    quoteId :: Id DQuote.Quote,
    fromLocation :: SRLoc.SearchReqLocation,
    toLocation :: Maybe SRLoc.SearchReqLocation,
    providerId :: Text,
    providerUrl :: BaseUrl
  }

select :: Id DPerson.Person -> Id DQuote.Quote -> Flow DSelectReq
select personId quoteId = do
  now <- getCurrentTime
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  let searchRequestId = quote.requestId
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist personId.getId)
  when ((searchRequest.validTill) < now) $
    throwError SearchRequestExpired
  fromLocation <- QLoc.findById searchRequest.fromLocationId >>= fromMaybeM (InternalError "fromLocation not found") --FIXME
  toLocation <-
    case searchRequest.toLocationId of
      Nothing -> pure Nothing
      Just toLocId -> (QLoc.findById toLocId >>= fromMaybeM (InternalError "toLocation not found")) <&> Just --FIXME
  case quote.quoteDetails of
    DQuote.AutoDetails -> pure ()
    _ -> throwError $ InvalidRequest "Static quotes cannot be selected"
  pure
    DSelectReq
      { providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        ..
      }
