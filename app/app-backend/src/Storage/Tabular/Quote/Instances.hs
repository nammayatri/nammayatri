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
        pure . Domain.RentalDetails $ fromRentalSlabTType rentalSlabT
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
          Domain.RentalDetails details -> do
            let rentalSlabT = toRentalSlabTType details
            (Domain.RENTAL, RentalDetailsT rentalSlabT, Nothing, Just $ toKey details.slabId)

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

-- use instance instead
fromRentalSlabTType :: RentalSlabT -> Domain.RentalQuoteDetails
fromRentalSlabTType RentalSlabT {..} =
  Domain.RentalQuoteDetails
    { slabId = Id id,
      baseDistance = Kilometers baseDistance,
      baseDuration = Hours baseDuration
    }

toRentalSlabTType :: Domain.RentalQuoteDetails -> RentalSlabT
toRentalSlabTType Domain.RentalQuoteDetails {..} =
  RentalSlabT
    { id = getId slabId,
      baseDistance = getKilometers baseDistance,
      baseDuration = getHours baseDuration
    }
