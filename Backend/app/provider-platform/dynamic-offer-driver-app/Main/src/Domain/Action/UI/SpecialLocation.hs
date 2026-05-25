module Domain.Action.UI.SpecialLocation where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (throwError)
import Lib.Queries.SpecialLocation
import Servant (Header, Headers, addHeader)
import Tools.Error (SpecialZoneErrors (SpecialLocationNotModified))

getSpecialLocationList ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Text ->
    Environment.Flow (Headers '[Header "ETag" Text] [Lib.Queries.SpecialLocation.SpecialLocationFull])
  )
getSpecialLocationList (_, _, merchantOperatingCityId) isOrigin mbIfNoneMatch = do
  let mocId = merchantOperatingCityId.getId
  mbCachedETag <- getFullSpecialLocationHash mocId
  case (mbIfNoneMatch, mbCachedETag) of
    (Just clientETag, Just cachedETag)
      | clientETag == cachedETag ->
        throwError SpecialLocationNotModified
    _ -> do
      specialLocationsWithAllGates <- notNullGateGeoJson <$> findFullSpecialLocationsByMerchantOperatingCityId mocId
      let isOrigin' = fromMaybe True isOrigin
          filteredLocs = mapMaybe (\loc -> filterGates (Just loc) isOrigin') specialLocationsWithAllGates
          eTag = computeSpecialLocationListETag filteredLocs
      setFullSpecialLocationHash mocId eTag
      pure $ addHeader eTag filteredLocs
  where
    notNullGateGeoJson = filter (any (isJust . (.geoJson)) . (.gatesInfo))
