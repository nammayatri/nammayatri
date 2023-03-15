{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.Instances where

import qualified Domain.Types.FarePolicy.FareProductType as Domain
import qualified Domain.Types.Quote as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Storage.Tabular.DriverOffer as SDriverOffer
import Storage.Tabular.Quote
import qualified Storage.Tabular.RentalSlab as SRentalSlab
import qualified Storage.Tabular.SpecialZoneQuote as SSpecialZoneQuote
import qualified Storage.Tabular.TripTerms as STripTerms
import Tools.Error

data QuoteDetailsT
  = OneWayDetailsT
  | RentalDetailsT SRentalSlab.RentalSlabT
  | DriverOfferDetailsT SDriverOffer.DriverOfferT
  | OneWaySpecialZoneDetailsT SSpecialZoneQuote.SpecialZoneQuoteT

type FullQuoteT = (QuoteT, Maybe STripTerms.TripTermsT, QuoteDetailsT)

instance FromTType FullQuoteT Domain.Quote where
  fromTType (QuoteT {..}, mbTripTermsT, quoteDetailsT) = do
    pUrl <- parseBaseUrl providerUrl
    tripTerms <- forM mbTripTermsT fromTType
    quoteDetails <- case quoteDetailsT of
      OneWayDetailsT -> do
        distanceToNearestDriver' <- distanceToNearestDriver & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
        pure . Domain.OneWayDetails $
          Domain.OneWayQuoteDetails
            { distanceToNearestDriver = distanceToNearestDriver'
            }
      RentalDetailsT rentalSlabT ->
        Domain.RentalDetails <$> fromTType rentalSlabT
      DriverOfferDetailsT driverOfferT ->
        Domain.DriverOfferDetails <$> fromTType driverOfferT
      OneWaySpecialZoneDetailsT specialZoneQuoteT ->
        Domain.OneWaySpecialZoneDetails <$> fromTType specialZoneQuoteT
    return $
      Domain.Quote
        { id = Id id,
          requestId = fromKey requestId,
          providerUrl = pUrl,
          estimatedFare = roundToIntegral estimatedFare,
          discount = roundToIntegral <$> discount,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType FullQuoteT Domain.Quote where
  toTType Domain.Quote {..} = do
    let (fareProductType, quoteDetailsT, distanceToNearestDriver, rentalSlabId, driverOfferId, specialZoneQuoteId) =
          case quoteDetails of
            Domain.OneWayDetails details ->
              (Domain.ONE_WAY, OneWayDetailsT, Just $ details.distanceToNearestDriver, Nothing, Nothing, Nothing)
            Domain.RentalDetails rentalSlab -> do
              let rentalSlabT = toTType rentalSlab
              (Domain.RENTAL, RentalDetailsT rentalSlabT, Nothing, Just $ toKey rentalSlab.id, Nothing, Nothing)
            Domain.DriverOfferDetails driverOffer -> do
              let driverOfferT = toTType driverOffer
              (Domain.DRIVER_OFFER, DriverOfferDetailsT driverOfferT, Nothing, Nothing, Just $ toKey driverOffer.id, Nothing)
            Domain.OneWaySpecialZoneDetails specialZoneQuote -> do
              let specialZoneQuoteT = toTType specialZoneQuote
              (Domain.ONE_WAY_SPECIAL_ZONE, OneWaySpecialZoneDetailsT specialZoneQuoteT, Nothing, Nothing, Nothing, Just $ toKey specialZoneQuote.id)
        quoteT =
          QuoteT
            { id = getId id,
              requestId = toKey requestId,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              estimatedFare = realToFrac estimatedFare,
              discount = realToFrac <$> discount,
              estimatedTotalFare = realToFrac estimatedTotalFare,
              merchantId = toKey merchantId,
              ..
            }
    let mbTripTermsT = toTType <$> tripTerms
    (quoteT, mbTripTermsT, quoteDetailsT)
