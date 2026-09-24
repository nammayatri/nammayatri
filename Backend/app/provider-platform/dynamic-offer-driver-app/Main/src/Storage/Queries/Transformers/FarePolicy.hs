module Storage.Queries.Transformers.FarePolicy where

import qualified "this" API.Types.ProviderPlatform.Management.Merchant as DPM
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.ConditionalCharges as DTAC
import Domain.Types.FarePolicy
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.ConditionalCharges as QueriesAdditionalCharges
import qualified Storage.Queries.FarePolicyAmbulanceDetailsSlab as QueriesFPAD
import qualified Storage.Queries.FarePolicyDriverExtraFeeBounds as QueriesDEFB
import qualified Storage.Queries.FarePolicyInterCityDetails as QueriesFPICD
import qualified Storage.Queries.FarePolicyProgressiveDetails as QueriesFPPD
import qualified Storage.Queries.FarePolicyRentalDetails as QueriesFPRD
import qualified Storage.Queries.FarePolicySlabsDetailsSlab as QueriesFPSDS

mkNightShiftBounds :: Maybe TimeOfDay -> Maybe TimeOfDay -> Maybe DPM.NightShiftBounds
mkNightShiftBounds nightShiftStart nightShiftEnd = DPM.NightShiftBounds <$> nightShiftStart <*> nightShiftEnd

mkAllowedTripDistanceBoundsFromBeam :: Maybe Meters -> Maybe Meters -> Maybe DistanceUnit -> Maybe AllowedTripDistanceBounds
mkAllowedTripDistanceBoundsFromBeam minDist maxDist mDistUnit =
  ((,) <$> minDist <*> maxDist) <&> \(minD, maxD) ->
    AllowedTripDistanceBounds
      { minAllowedTripDistance = minD,
        maxAllowedTripDistance = maxD,
        distanceUnit = fromMaybe Meter mDistUnit
      }

mkCardCharge :: Maybe Double -> Maybe HighPrecMoney -> Maybe CardCharge
mkCardCharge perDistanceUnitMultiplier fixed =
  Just $ CardCharge {perDistanceUnitMultiplier = perDistanceUnitMultiplier, fixed = fixed}

mkFarePolicyType :: FarePolicyDetails -> FarePolicyType
mkFarePolicyType = \case
  ProgressiveDetails _ -> Progressive
  SlabsDetails _ -> Slabs
  RentalDetails _ -> Rental
  InterCityDetails _ -> InterCity
  AmbulanceDetails _ -> Ambulance

fromMaybeFarePolicyDetails :: Maybe FarePolicyDetails -> FarePolicyDetails
fromMaybeFarePolicyDetails = fromMaybe (error "farePolicyDetails not found")

encodeChargeConfig :: FareChargeConfig -> Text
encodeChargeConfig = TE.decodeUtf8 . BL.toStrict . Aeson.encode

decodeChargeConfig :: Maybe Text -> Maybe FareChargeConfig
decodeChargeConfig = (>>= Aeson.decode . BL.fromStrict . TE.encodeUtf8)

fetchFarePolicyDetails :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FarePolicyType -> Text -> m (Maybe FarePolicyDetails)
fetchFarePolicyDetails fareParamsType fpId =
  case fareParamsType of
    Progressive -> do
      mFPPD <- QueriesFPPD.findById' (Id fpId)
      case mFPPD of
        Just (_, fPPD) -> return $ Just (ProgressiveDetails fPPD)
        Nothing -> return Nothing
    Slabs -> do
      fullSlabs <- QueriesFPSDS.findAll' (Id fpId)
      let slabs = snd <$> fullSlabs
      case nonEmpty slabs of
        Just nESlabs -> return $ Just (SlabsDetails (FPSlabsDetails nESlabs))
        Nothing -> return Nothing
    Rental -> do
      mFPRD <- QueriesFPRD.findById' (Id fpId)
      case mFPRD of
        Just (_, fPRD) -> return $ Just (RentalDetails fPRD)
        Nothing -> return Nothing
    InterCity -> do
      mFPICD <- QueriesFPICD.findById' (Id fpId)
      case mFPICD of
        Just (_, fPICD) -> return $ Just (InterCityDetails fPICD)
        Nothing -> return Nothing
    Ambulance -> do
      fullAmbSlabs <- QueriesFPAD.findById' (Id fpId)
      let slabs = snd <$> fullAmbSlabs
      case nonEmpty slabs of
        Just nESlabs -> return $ Just (AmbulanceDetails (FPAmbulanceDetails nESlabs))
        Nothing -> return Nothing

fetchDriverExtraFeeBounds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [DriverExtraFeeBounds]
fetchDriverExtraFeeBounds fpId = do
  results <- QueriesDEFB.findAll' (Id fpId)
  pure $ snd <$> results

fetchConditionalCharges :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [DTAC.ConditionalCharges]
fetchConditionalCharges = QueriesAdditionalCharges.findAllByFp
