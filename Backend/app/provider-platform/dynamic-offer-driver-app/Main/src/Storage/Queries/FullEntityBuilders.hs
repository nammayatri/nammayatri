{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders
  ( buildFullBooking,
    buildFullFareParameters,
    buildFullFarePolicy,
    buildFullDriverQuote,
    buildFullQuoteSpecialZone,
  )
where

import Domain.Types.Booking
import qualified Domain.Types.DriverQuote as DriverQuote
import qualified Domain.Types.FareParameters as FareParams
import qualified Domain.Types.FarePolicy as FarePolicy
import qualified Domain.Types.QuoteSpecialZone as QuoteSpecialZone
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (findById, isNothing)
import Kernel.Types.Id
import qualified Storage.Queries.FarePolicy.DriverExtraFeeBounds as FarePolicyDriverExtraFeeBounds
import qualified Storage.Queries.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as QFarePolicyProgressiveDetailsPerExtraKmRateSection
import qualified Storage.Queries.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab as FarePolicySlabsDetailsSlab
import Storage.Tabular.Booking
import Storage.Tabular.Booking.BookingLocation
import qualified Storage.Tabular.DriverQuote as DriverQuote
import Storage.Tabular.FareParameters
import qualified Storage.Tabular.FareParameters as FareParams
import qualified Storage.Tabular.FareParameters.FareParametersProgressiveDetails as FareParametersProgressiveDetails
import qualified Storage.Tabular.FareParameters.FareParametersSlabDetails as FareParametersSlabDetails
import qualified Storage.Tabular.FareParameters.Instances as FareParams
import Storage.Tabular.FarePolicy
import qualified Storage.Tabular.FarePolicy as FarePolicy
import qualified Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails as FarePolicyProgressiveDetails
import qualified Storage.Tabular.FarePolicy.Instances as FarePolicy
import qualified Storage.Tabular.QuoteSpecialZone as QuoteSpecialZone

buildFullBooking ::
  Transactionable m =>
  BookingT ->
  DTypeBuilder m (Maybe (SolidType FullBookingT))
buildFullBooking bookingT@BookingT {..} = runMaybeT $ do
  fromLocationT <- Esq.findByIdM @BookingLocationT fromLocationId
  toLocationT <- Esq.findByIdM @BookingLocationT toLocationId
  fareParamsT <- Esq.findByIdM @FareParams.FareParametersT fareParametersId
  fullFareParamsData <- MaybeT $ getFullFareParamsData fareParamsT
  return $ extractSolidType @Booking (bookingT, fromLocationT, toLocationT, fullFareParamsData)

getFullFareParamsData ::
  Transactionable m =>
  FareParams.FareParametersT ->
  DTypeBuilder m (Maybe FareParams.FullFareParametersT)
getFullFareParamsData fareParamsT@FareParams.FareParametersT {..} = do
  runMaybeT $ do
    fareParamsDet <- case fareParametersType of
      FareParams.Progressive ->
        MaybeT $
          fmap FareParams.ProgressiveDetailsT
            <$> Esq.findById' @FareParametersProgressiveDetails.FareParametersProgressiveDetailsT (toKey $ Id id)
      FareParams.Slab ->
        MaybeT $
          fmap FareParams.SlabDetailsT
            <$> Esq.findById' @FareParametersSlabDetails.FareParametersSlabDetailsT (toKey $ Id id)
    return (fareParamsT, fareParamsDet)

buildFullFareParameters ::
  Transactionable m =>
  FareParams.FareParametersT ->
  DTypeBuilder m (Maybe (SolidType FareParams.FullFareParametersT))
buildFullFareParameters fareParamsT = do
  fmap (extractSolidType @FareParams.FareParameters) <$> getFullFareParamsData fareParamsT

buildFullFarePolicy ::
  Transactionable m =>
  FarePolicy.FarePolicyT ->
  DTypeBuilder m (Maybe (SolidType FarePolicy.FullFarePolicyT))
buildFullFarePolicy farePolicyT@FarePolicy.FarePolicyT {..} = do
  driverExtraFeeBoundsT <- FarePolicyDriverExtraFeeBounds.findAll' (Id id)
  runMaybeT $ do
    farePolicyDet <- case farePolicyType of
      FarePolicy.Progressive -> do
        fpDetT <- MaybeT $ Esq.findById' @FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsT (toKey $ Id id)
        fpDetPerExtraKmRateT <- MaybeT $ Just <$> QFarePolicyProgressiveDetailsPerExtraKmRateSection.findAll' (Id id)
        return $ FarePolicy.ProgressiveDetailsT (fpDetT, fpDetPerExtraKmRateT)
      FarePolicy.Slabs -> MaybeT $ Just . FarePolicy.SlabsDetailsT <$> FarePolicySlabsDetailsSlab.findAll' (Id id)
    return $ extractSolidType @FarePolicy.FarePolicy (farePolicyT, driverExtraFeeBoundsT, farePolicyDet)

buildFullDriverQuote ::
  Transactionable m =>
  (DriverQuote.DriverQuoteT, FareParams.FareParametersT) ->
  DTypeBuilder m (Maybe (SolidType DriverQuote.FullDriverQuoteT))
buildFullDriverQuote (driverQuoteT, fareParamsT) = runMaybeT $ do
  fullFareParamsData <- MaybeT $ getFullFareParamsData fareParamsT
  return $ extractSolidType @DriverQuote.DriverQuote (driverQuoteT, fullFareParamsData)

buildFullQuoteSpecialZone ::
  Transactionable m =>
  (QuoteSpecialZone.QuoteSpecialZoneT, FareParams.FareParametersT) ->
  DTypeBuilder m (Maybe (SolidType QuoteSpecialZone.FullQuoteSpecialZoneT))
buildFullQuoteSpecialZone (quoteSpecialZoneT, fareParamsT) = runMaybeT $ do
  fullFareParamsData <- MaybeT $ getFullFareParamsData fareParamsT
  return $ extractSolidType @QuoteSpecialZone.QuoteSpecialZone (quoteSpecialZoneT, fullFareParamsData)
