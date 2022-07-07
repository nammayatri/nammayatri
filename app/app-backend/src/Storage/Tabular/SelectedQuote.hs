{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SelectedQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.SelectedQuote as Domain
import qualified Domain.Types.VehicleVariant as VehVar
import qualified Storage.Tabular.Quote as TQuote
import qualified Storage.Tabular.TripTerms as STripTerms
import Types.Common hiding (id)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SelectedQuoteT sql=selected_quote
      id Text
      fareProductType FareProductType
      quoteId TQuote.QuoteTId
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      providerCompletedRidesCount Int
      vehicleVariant VehVar.VehicleVariant
      tripTermsId STripTerms.TripTermsTId Maybe
      createdAt UTCTime
      driverName Text
      durationToPickup Int
      distanceToPickup Double
      validTill UTCTime
      bppQuoteId Text
      rating Double Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey SelectedQuoteT where
  type DomainKey SelectedQuoteT = Id Domain.SelectedQuote
  fromKey (SelectedQuoteTKey _id) = Id _id
  toKey (Id id) = SelectedQuoteTKey id

type FullSelectedQuoteT = (SelectedQuoteT, Maybe STripTerms.TripTermsT)

instance TType FullSelectedQuoteT Domain.SelectedQuote where
  fromTType (SelectedQuoteT {..}, mbTripTermsT) = do
    pUrl <- parseBaseUrl providerUrl
    tripTerms <- forM mbTripTermsT fromTType

    return $
      Domain.SelectedQuote
        { id = Id id,
          providerUrl = pUrl,
          bppQuoteId = Id bppQuoteId,
          quoteId = fromKey quoteId,
          ..
        }
  toTType Domain.SelectedQuote {..} = do
    let quoteT =
          SelectedQuoteT
            { id = getId id,
              fareProductType = AUTO,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              bppQuoteId = bppQuoteId.getId,
              quoteId = toKey quoteId,
              ..
            }
    let mbTripTermsT = toTType <$> tripTerms
    (quoteT, mbTripTermsT)
