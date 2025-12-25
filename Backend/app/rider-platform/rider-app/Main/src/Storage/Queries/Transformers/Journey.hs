module Storage.Queries.Transformers.Journey where

import qualified Domain.Types.Location
import qualified Domain.Types.LocationAddress as DLA
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Location as QL
import Tools.Error

getLocation :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Kernel.Prelude.Text -> Maybe Kernel.Prelude.Text -> m Domain.Types.Location.Location
getLocation locationId locationAddress = do
  case (locationId, locationAddress) of
    (Just id, _) -> QL.findById (Id id) >>= fromMaybeM (FromLocationNotFound id)
    -- For Backward compatibility, Address is required
    (_, Just address) -> do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.Types.Location.Location
          { address =
              DLA.LocationAddress
                { street = Nothing,
                  door = Nothing,
                  city = Nothing,
                  state = Nothing,
                  country = Nothing,
                  building = Nothing,
                  areaCode = Nothing,
                  area = Nothing,
                  ward = Nothing,
                  placeId = Nothing,
                  instructions = Nothing,
                  title = Just address,
                  extras = Nothing
                },
            createdAt = now,
            id,
            lat = 0.0,
            lon = 0.0,
            updatedAt = now,
            merchantId = Nothing,
            merchantOperatingCityId = Nothing
          }
    _ -> throwError $ InternalError "I was supposed to be backward compatible, sorry incase I'm not!"
