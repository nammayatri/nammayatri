{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.Instances where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import Storage.Tabular.Quote.QuoteTerms
import Storage.Tabular.Quote.RentalQuote
import Storage.Tabular.Quote.Table
import Types.Error
import Utils.Common hiding (id)

data QuoteDetailsT = OneWayDetailsT | RentalDetailsT RentalQuoteT

type FullQuoteT = (QuoteT, [QuoteTermsT], QuoteDetailsT)

instance TType FullQuoteT Domain.Quote where
  fromTType (QuoteT {..}, quoteTermsT, quoteDetailsT) = do
    pUrl <- parseBaseUrl providerUrl
    let quoteTerms = fromQuoteTermsTType <$> quoteTermsT
    quoteDetails <- case quoteDetailsT of
      OneWayDetailsT -> do
        distanceToNearestDriver' <- distanceToNearestDriver & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
        pure . Domain.OneWayDetails $
          Domain.OneWayQuoteDetails
            { distanceToNearestDriver = distanceToNearestDriver'
            }
      RentalDetailsT RentalQuoteT {..} -> do
        pure . Domain.RentalDetails $
          Domain.RentalQuoteDetails
            { baseDistance = Kilometers baseDistance,
              baseDuration = Hours baseDuration
            }

    return $
      Domain.Quote
        { id = Id id,
          requestId = fromKey requestId,
          providerUrl = pUrl,
          quoteTerms = quoteTerms,
          ..
        }
  toTType Domain.Quote {..} = do
    let (quoteDetailsT, distanceToNearestDriver) = case quoteDetails of
          Domain.OneWayDetails details -> (OneWayDetailsT, Just details.distanceToNearestDriver)
          Domain.RentalDetails details -> do
            let rentalQuoteT = toRentalQuoteTType id details.baseDistance details.baseDuration
            (RentalDetailsT rentalQuoteT, Nothing)
        quoteTermsT = toQuoteTermsTType id <$> quoteTerms
        quoteT =
          QuoteT
            { id = getId id,
              requestId = toKey requestId,
              providerUrl = showBaseUrl providerUrl,
              ..
            }
    (quoteT, quoteTermsT, quoteDetailsT)

fromQuoteTermsTType :: QuoteTermsT -> Domain.QuoteTerms
fromQuoteTermsTType QuoteTermsT {..} = do
  Domain.QuoteTerms
    { id = Id id,
      ..
    }

toQuoteTermsTType :: Id Domain.Quote -> Domain.QuoteTerms -> QuoteTermsT
toQuoteTermsTType quoteId Domain.QuoteTerms {..} =
  QuoteTermsT
    { id = getId id,
      quoteId = toKey quoteId,
      ..
    }

toRentalQuoteTType :: Id Domain.Quote -> Kilometers -> Hours -> RentalQuoteT
toRentalQuoteTType quoteId (Kilometers baseDistance) (Hours baseDuration) =
  RentalQuoteT
    { quoteId = toKey quoteId,
      ..
    }
