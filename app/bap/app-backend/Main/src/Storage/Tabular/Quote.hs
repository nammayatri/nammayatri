{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote where

import qualified Domain.Types.FarePolicy.FareProductType as Domain
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.VehicleVariant as VehVar
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMeters, HighPrecMoney)
import Kernel.Types.Id
import qualified Storage.Tabular.DriverOffer as SDriverOffer
import qualified Storage.Tabular.RentalSlab as SRentalSlab
import qualified Storage.Tabular.SearchRequest as SSearchRequest
import qualified Storage.Tabular.TripTerms as STripTerms

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      fareProductType Domain.FareProductType
      requestId SSearchRequest.SearchRequestTId
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      providerCompletedRidesCount Int
      distanceToNearestDriver HighPrecMeters Maybe
      vehicleVariant VehVar.VehicleVariant
      tripTermsId STripTerms.TripTermsTId Maybe
      rentalSlabId SRentalSlab.RentalSlabTId Maybe
      driverOfferId SDriverOffer.DriverOfferTId Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT where
  type DomainKey QuoteT = Id Domain.Quote
  fromKey (QuoteTKey _id) = Id _id
  toKey (Id id) = QuoteTKey id
