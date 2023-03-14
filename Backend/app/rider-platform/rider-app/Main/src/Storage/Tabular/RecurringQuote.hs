{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RecurringQuote where

import qualified Domain.Types.RecurringQuote as Domain
import qualified Domain.Types.VehicleVariant as VehVar
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMeters, HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Storage.Tabular.SearchRequest as SSearchRequest
import Tools.Error

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RecurringQuoteT sql=recurring_quote
      id Text
      requestId SSearchRequest.SearchRequestTId
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      providerId Text
      providerUrl Text
      distanceToNearestDriver HighPrecMeters Maybe
      vehicleVariant VehVar.VehicleVariant
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RecurringQuoteT where
  type DomainKey RecurringQuoteT = Id Domain.RecurringQuote
  fromKey (RecurringQuoteTKey _id) = Id _id
  toKey (Id id) = RecurringQuoteTKey id

instance FromTType RecurringQuoteT Domain.RecurringQuote where
  fromTType (RecurringQuoteT {..}) = do
    pUrl <- parseBaseUrl providerUrl
    distToNearest <- distanceToNearestDriver & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
    pure $
      Domain.RecurringQuote
        { id = Id id,
          requestId = fromKey requestId,
          estimatedFare = roundToIntegral estimatedFare,
          discount = roundToIntegral <$> discount,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          providerId = providerId,
          providerUrl = pUrl,
          tripTerms = Nothing,
          quoteDetails = Domain.RecurringQuoteDetails distToNearest,
          createdAt = createdAt
        }

instance ToTType RecurringQuoteT Domain.RecurringQuote where
  toTType recurringQuote =
    RecurringQuoteT
      { id = getId recurringQuote.id,
        requestId = toKey recurringQuote.requestId,
        estimatedFare = realToFrac recurringQuote.estimatedFare,
        discount = realToFrac <$> recurringQuote.discount,
        estimatedTotalFare = realToFrac recurringQuote.estimatedTotalFare,
        providerId = recurringQuote.providerId,
        providerUrl = showBaseUrl recurringQuote.providerUrl,
        distanceToNearestDriver = Just recurringQuote.quoteDetails.distanceToNearestDriver,
        vehicleVariant = VehVar.AUTO_RICKSHAW,
        createdAt = recurringQuote.createdAt
      }
