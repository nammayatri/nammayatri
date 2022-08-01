{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.Instances where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.Error
import qualified Domain.Types.Quote as Domain
import qualified Storage.Tabular.DriverOffer as SDriverOffer
import Storage.Tabular.Quote
import qualified Storage.Tabular.RentalSlab as SRentalSlab
import qualified Storage.Tabular.TripTerms as STripTerms
import Types.Error

data QuoteDetailsT
  = OneWayDetailsT
  | RentalDetailsT SRentalSlab.RentalSlabT
  | DriverOfferDetailsT SDriverOffer.DriverOfferT

type FullQuoteT = (QuoteT, Maybe STripTerms.TripTermsT, QuoteDetailsT)

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
      DriverOfferDetailsT driverOfferT ->
        Domain.DriverOfferDetails <$> fromTType driverOfferT
    return $
      Domain.Quote
        { id = Id id,
          requestId = fromKey requestId,
          providerUrl = pUrl,
          ..
        }
  toTType Domain.Quote {..} = do
    let (fareProductType, quoteDetailsT, distanceToNearestDriver, rentalSlabId, driverOfferId) =
          case quoteDetails of
            Domain.OneWayDetails details ->
              (Domain.ONE_WAY, OneWayDetailsT, Just $ getHighPrecMeters details.distanceToNearestDriver, Nothing, Nothing)
            Domain.RentalDetails rentalSlab -> do
              let rentalSlabT = toTType rentalSlab
              (Domain.RENTAL, RentalDetailsT rentalSlabT, Nothing, Just $ toKey rentalSlab.id, Nothing)
            Domain.DriverOfferDetails driverOffer -> do
              let driverOfferT = toTType driverOffer
              (Domain.AUTO, DriverOfferDetailsT driverOfferT, Nothing, Nothing, Just $ toKey driverOffer.id)
        quoteT =
          QuoteT
            { id = getId id,
              requestId = toKey requestId,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              ..
            }
    let mbTripTermsT = toTType <$> tripTerms
    (quoteT, mbTripTermsT, quoteDetailsT)
