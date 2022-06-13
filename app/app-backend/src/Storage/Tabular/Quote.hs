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
import qualified Domain.Types.VehicleVariant as VehVar
import qualified Storage.Tabular.RentalSlab as SRentalSlab
import qualified Storage.Tabular.SearchRequest as SSearchRequest
import qualified Storage.Tabular.TripTerms as STripTerms
import Types.Error
import Utils.Common hiding (id)

derivePersistField "Domain.FareProductType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      fareProductType Domain.FareProductType
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
      vehicleVariant VehVar.VehicleVariant
      tripTermsId STripTerms.TripTermsTId Maybe
      rentalSlabId SRentalSlab.RentalSlabTId Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT where
  type DomainKey QuoteT = Id Domain.Quote
  fromKey (QuoteTKey _id) = Id _id
  toKey (Id id) = QuoteTKey id

data QuoteDetailsT = OneWayDetailsT | RentalDetailsT SRentalSlab.RentalSlabT

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
