module Storage.Queries.Transformers.FareParameters where

import qualified Data.Aeson as Aeson
import Domain.Types.ConditionalCharges (ConditionalCharges)
import Domain.Types.FareParameters
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FareParametersAmbulanceDetails as BeamFPAD
import qualified Storage.Queries.FareParametersInterCityDetails as QFPICD
import qualified Storage.Queries.FareParametersProgressiveDetails as QFPPD
import qualified Storage.Queries.FareParametersRentalDetails as QFPRD
import qualified Storage.Queries.FareParametersSlabDetails as QFPSD

mkCardCharge :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe CardCharge
mkCardCharge onFare fixed = Just $ CardCharge {onFare = onFare, fixed = fixed}

decodeConditionalCharges :: Maybe Aeson.Value -> [ConditionalCharges]
decodeConditionalCharges mVal =
  fromMaybe [] $
    mVal >>= \val -> case Aeson.fromJSON val of
      Aeson.Success x -> Just x
      Aeson.Error _ -> Nothing

decodeCustomerGateFeeItems :: Maybe Aeson.Value -> [CustomerGateFeeItem]
decodeCustomerGateFeeItems mVal =
  fromMaybe [] $
    mVal >>= \val -> case Aeson.fromJSON val of
      Aeson.Success x -> Just x
      Aeson.Error _ -> Nothing

mkFareParametersType :: FareParametersDetails -> FareParametersType
mkFareParametersType = \case
  ProgressiveDetails _ -> Progressive
  SlabDetails _ -> Slab
  RentalDetails _ -> Rental
  InterCityDetails _ -> InterCity
  AmbulanceDetails _ -> Ambulance

fromMaybeFareParametersDetails :: Maybe FareParametersDetails -> FareParametersDetails
fromMaybeFareParametersDetails = fromMaybe (error "fareParametersDetails not found")

fetchFareParametersDetails :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FareParametersType -> Text -> m (Maybe FareParametersDetails)
fetchFareParametersDetails fareParamsType fpId =
  case fareParamsType of
    Progressive -> do
      mFullFPPD <- QFPPD.findById' (Id fpId)
      case mFullFPPD of
        Just (_, fPPD) -> return (Just $ ProgressiveDetails fPPD)
        Nothing -> return Nothing
    Slab -> do
      mFullFPSD <- QFPSD.findById' (Id fpId)
      case mFullFPSD of
        Just (_, fPSD) -> return (Just $ SlabDetails fPSD)
        Nothing -> return Nothing
    Rental -> do
      mFullFPRD <- QFPRD.findById' (Id fpId)
      case mFullFPRD of
        Just (_, fPRD) -> return (Just $ RentalDetails fPRD)
        Nothing -> return Nothing
    InterCity -> do
      mFullFPICD <- QFPICD.findById' (Id fpId)
      case mFullFPICD of
        Just (_, fPICD) -> return (Just $ InterCityDetails fPICD)
        Nothing -> return Nothing
    Ambulance -> do
      mFullFPAD <- BeamFPAD.findById' (Id fpId)
      case mFullFPAD of
        Just (_, fPAD) -> return (Just $ AmbulanceDetails fPAD)
        Nothing -> return Nothing
