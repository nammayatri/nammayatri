{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnSearch.Rental where

import Data.Traversable
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverPool (HasDriverPoolConfig)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as QRentalFarePolicy
import qualified Storage.Queries.Quote as QQuote
import Tools.Maps
import Tools.Metrics (CoreMetrics, HasBPPMetrics)

data QuoteInfo = QuoteInfo
  { quoteId :: Id DQuote.Quote,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    baseDistance :: Kilometers,
    baseDuration :: Hours,
    descriptions :: [Text],
    fromLocation :: LatLong,
    startTime :: UTCTime
  }

onSearchCallback ::
  forall m r.
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasDriverPoolConfig r,
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  DSearchRequest.SearchRequest ->
  Id DM.Merchant ->
  DLoc.SearchReqLocation ->
  UTCTime ->
  m [QuoteInfo]
onSearchCallback searchRequest transporterId fromLocation now = do
  rentalFarePolicies <- QRentalFarePolicy.findAllByMerchantId transporterId

  let fromLoc = getCoordinates fromLocation
  (listOfQuotes, quoteInfos) <- fmap unzip $
    forM rentalFarePolicies $ \fp -> do
      quote <- buildRentalQuote searchRequest.id now fp
      let quoteInfo = mkQuoteInfo quote fp fromLoc searchRequest.startTime
      return (quote, quoteInfo)

  Esq.runTransaction $
    for_ listOfQuotes (QQuote.create @m)

  pure quoteInfos

buildRentalQuote ::
  EsqDBFlow m r =>
  Id DSearchRequest.SearchRequest ->
  UTCTime ->
  DRentalFP.RentalFarePolicy ->
  m DQuote.Quote
buildRentalQuote searchRequestId now rentalFarePolicy@DRentalFP.RentalFarePolicy {..} = do
  quoteId <- Id <$> generateGUID
  let estimatedFare = baseFare
      discount = Nothing -- FIXME we don't have discount in RentalFarePolicy now
      estimatedTotalFare = estimatedFare
  -- FIXME this request is duplicating
  pure $
    DQuote.Quote
      { id = quoteId,
        requestId = searchRequestId,
        providerId = merchantId,
        createdAt = now,
        quoteDetails = DQuote.RentalDetails rentalFarePolicy,
        ..
      }

mkQuoteInfo :: DQuote.Quote -> DRentalFP.RentalFarePolicy -> LatLong -> UTCTime -> QuoteInfo
mkQuoteInfo quote DRentalFP.RentalFarePolicy {..} fromLocation startTime = do
  QuoteInfo
    { quoteId = quote.id,
      vehicleVariant = quote.vehicleVariant,
      estimatedFare = quote.estimatedFare,
      discount = quote.discount,
      estimatedTotalFare = quote.estimatedTotalFare,
      ..
    }
