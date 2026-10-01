module SharedLogic.LocationFallback
  ( LocationRole (..),
    LenientLocationReadsKey (..),
    withLenientLocationReads,
    mkPlaceholderLocation,
    isPlaceholderLocation,
    initialPickupLookupId,
    resolveLocation,
  )
where

import Data.List (sortOn)
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationAddress as DLA
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Location as QL
import Tools.Error (LocationMappingError)

data LocationRole = Pickup | Drop | Stop
  deriving (Show, Eq)

emptyAddress :: DLA.LocationAddress
emptyAddress = DLA.LocationAddress Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkPlaceholderLocation :: Id DL.Location -> Maybe (Id DM.Merchant) -> Maybe (Id DMOC.MerchantOperatingCity) -> UTCTime -> DL.Location
mkPlaceholderLocation locId merchantId merchantOperatingCityId now =
  DL.Location
    { id = locId,
      lat = 0,
      lon = 0,
      address = emptyAddress,
      createdAt = now,
      updatedAt = now,
      ..
    }

isPlaceholderLocation :: DL.Location -> Bool
isPlaceholderLocation loc = loc.lat == 0 && loc.lon == 0 && loc.address == emptyAddress

data LenientLocationReadsKey = LenientLocationReadsKey
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity LenientLocationReadsKey Bool

withLenientLocationReads :: (L.MonadFlow m, MonadMask m) => m a -> m a
withLenientLocationReads action = do
  previous <- L.getOptionLocal LenientLocationReadsKey
  L.setOptionLocal LenientLocationReadsKey True
  action `finally` L.setOptionLocal LenientLocationReadsKey (fromMaybe False previous)

initialPickupLookupId :: [DLM.LocationMapping] -> DL.Location -> Maybe (Id DL.Location)
initialPickupLookupId mappings resolvedPickup =
  case listToMaybe (reverse (sortOn (.version) (filter (\m -> m.order == 0) mappings))) of
    Nothing -> Nothing
    Just initialMapping
      | initialMapping.locationId == resolvedPickup.id -> Nothing
      | otherwise -> Just initialMapping.locationId

resolveLocation ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  LocationRole ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Id DL.Location ->
  (Text -> LocationMappingError) ->
  m DL.Location
resolveLocation role entityId merchantId merchantOperatingCityId locId mkErr =
  QL.findById locId >>= \case
    Just loc -> pure loc
    Nothing -> do
      lenient <- fromMaybe False <$> L.getOptionLocal LenientLocationReadsKey
      logError $
        "LOCATION_MISSING role=" <> show role
          <> " entityId="
          <> entityId
          <> " locationId="
          <> locId.getId
          <> " lenient="
          <> show lenient
      if lenient
        then mkPlaceholderLocation locId (Id <$> merchantId) (Id <$> merchantOperatingCityId) <$> getCurrentTime
        else throwError (mkErr locId.getId)
