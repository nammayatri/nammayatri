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
import qualified Storage.Tabular.TripTerms as STripTerms
import Tools.Error

data QuoteDetailsT
  = OneWayDetailsT
  | RentalDetailsT SRentalSlab.RentalSlabT
  | DriverOfferDetailsT SDriverOffer.DriverOfferT

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
    return $
      Domain.Quote
        { id = Id id,
          requestId = fromKey requestId,
          providerUrl = pUrl,
          estimatedFare = roundToIntegral estimatedFare,
          discount = roundToIntegral <$> discount,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          ..
        }

instance ToTType FullQuoteT Domain.Quote where
  toTType Domain.Quote {..} = do
    let (fareProductType, quoteDetailsT, distanceToNearestDriver, rentalSlabId, driverOfferId) =
          case quoteDetails of
            Domain.OneWayDetails details ->
              (Domain.ONE_WAY, OneWayDetailsT, Just $ details.distanceToNearestDriver, Nothing, Nothing)
            Domain.RentalDetails rentalSlab -> do
              let rentalSlabT = toTType rentalSlab
              (Domain.RENTAL, RentalDetailsT rentalSlabT, Nothing, Just $ toKey rentalSlab.id, Nothing)
            Domain.DriverOfferDetails driverOffer -> do
              let driverOfferT = toTType driverOffer
              (Domain.DRIVER_OFFER, DriverOfferDetailsT driverOfferT, Nothing, Nothing, Just $ toKey driverOffer.id)
        quoteT =
          QuoteT
            { id = getId id,
              requestId = toKey requestId,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              estimatedFare = realToFrac estimatedFare,
              discount = realToFrac <$> discount,
              estimatedTotalFare = realToFrac estimatedTotalFare,
              ..
            }
    let mbTripTermsT = toTType <$> tripTerms
    (quoteT, mbTripTermsT, quoteDetailsT)
