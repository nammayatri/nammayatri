{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.Instances where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import Storage.Tabular.Quote.Table
import Storage.Tabular.RentalSlab
import Storage.Tabular.TripTerms
import Types.Error
import Utils.Common hiding (id)

-- we can move it back to Storage.Tabular.Quote
data QuoteDetailsT = OneWayDetailsT | RentalDetailsT RentalSlabT

type FullQuoteT = (QuoteT, Maybe TripTermsT, QuoteDetailsT)

instance TType FullQuoteT Domain.Quote where
  fromTType (QuoteT {..}, mbTripTermsT, quoteDetailsT) = do
    pUrl <- parseBaseUrl providerUrl
    tripTerms <- forM mbTripTermsT fromTType
    quoteDetails <- case quoteDetailsT of
      OneWayDetailsT -> do
        distanceToNearestDriver' <- distanceToNearestDriver & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
        pure . Domain.OneWayDetails $
          Domain.OneWayQuoteDetails
            { distanceToNearestDriver = HighPrecMeters distanceToNearestDriver'
            }
      RentalDetailsT rentalSlabT ->
        Domain.RentalDetails <$> fromTType rentalSlabT
    return $
      Domain.Quote
        { id = Id id,
          requestId = fromKey requestId,
          providerUrl = pUrl,
          ..
        }
  toTType Domain.Quote {..} = do
    let (fareProductType, quoteDetailsT, distanceToNearestDriver, rentalSlabId) = case quoteDetails of
          Domain.OneWayDetails details -> (Domain.ONE_WAY, OneWayDetailsT, Just $ getHighPrecMeters details.distanceToNearestDriver, Nothing)
          Domain.RentalDetails rentalSlab -> do
            let rentalSlabT = toTType rentalSlab
            (Domain.RENTAL, RentalDetailsT rentalSlabT, Nothing, Just $ toKey rentalSlab.id)

        quoteT =
          QuoteT
            { id = getId id,
              requestId = toKey requestId,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)), -- mbTripTermsT <&> (.id) not working
              ..
            }
    let mbTripTermsT = toTType <$> tripTerms
    (quoteT, mbTripTermsT, quoteDetailsT)
