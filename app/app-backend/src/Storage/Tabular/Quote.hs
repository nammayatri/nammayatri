{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Quote.QuoteTerms as Domain
import qualified Storage.Queries.Quote.QuoteTerms as QQuoteTerms
import qualified Storage.Queries.Quote.RentalQuote as QRentalQuote
import qualified Storage.Tabular.SearchRequest as SSearchRequest
import Types.Error
import Utils.Common hiding (id)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      bppQuoteId Text
      requestId SSearchRequest.SearchRequestTId
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      providerCompletedRidesCount Int
      distanceToNearestDriver Double Maybe
      vehicleVariant Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT where
  type DomainKey QuoteT = Id Domain.Quote
  fromKey (QuoteTKey _id) = Id _id
  toKey (Id id) = QuoteTKey id

instance TEntity QuoteT Domain.Quote where
  fromTEntity entity = do
    let QuoteT {..} = entityVal entity
    pUrl <- parseBaseUrl providerUrl
    quoteTermsEntities <- QQuoteTerms.findAllByQuoteId (Id id)
    let quoteTerms = Domain.mkQuoteTerms <$> quoteTermsEntities
    quoteDetails <- case distanceToNearestDriver of
      Just distanceToNearestDriver' ->
        pure . Domain.OneWayDetails $
          Domain.OneWayQuoteDetails
            { distanceToNearestDriver = distanceToNearestDriver'
            }
      Nothing -> do
        rentalQuote <- QRentalQuote.findByQuoteId (Id id) >>= fromMaybeM (QuoteDoesNotExist id)
        pure . Domain.RentalDetails $
          Domain.RentalQuoteDetails
            { baseDistance = rentalQuote.baseDistance,
              baseDurationHr = rentalQuote.baseDurationHr
            }

    return $
      Domain.Quote
        { id = Id id,
          bppQuoteId = Id bppQuoteId,
          requestId = fromKey requestId,
          providerUrl = pUrl,
          quoteTerms = quoteTerms,
          ..
        }
  toTType Domain.Quote {..} = do
    let distanceToNearestDriver = case quoteDetails of
          Domain.OneWayDetails details -> Just details.distanceToNearestDriver
          Domain.RentalDetails _ -> Nothing
    QuoteT
      { id = getId id,
        bppQuoteId = getId bppQuoteId,
        requestId = toKey requestId,
        providerUrl = showBaseUrl providerUrl,
        ..
      }

  toTEntity a =
    Entity (toKey a.id) $ toTType a
