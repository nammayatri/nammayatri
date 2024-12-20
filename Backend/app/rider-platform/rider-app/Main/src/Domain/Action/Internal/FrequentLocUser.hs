module Domain.Action.Internal.FrequentLocUser where

import Data.Aeson
import Data.OpenApi
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id, length, mapM_)
import Kernel.Prelude
import Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.Queries.Person as QP

newtype UpdateFrLocGeohashReq = UpdateFrLocGeohashReq
  { reqList :: [UpdateFrLocGeohash]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data UpdateFrLocGeohash = UpdateFrLocGeohash
  { frequentLocGeohashes :: [Text],
    riderId :: Id DP.Person
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

updateFrequentLocGeohashHandler :: UpdateFrLocGeohashReq -> Flow APISuccess
updateFrequentLocGeohashHandler UpdateFrLocGeohashReq {..} = do
  mapM_ updateFrequentLocGeoHashPerson reqList
  return APISuccess.Success

updateFrequentLocGeoHashPerson :: UpdateFrLocGeohash -> Flow ()
updateFrequentLocGeoHashPerson UpdateFrLocGeohash {..} = do
  person <- QP.findById riderId >>= fromMaybeM (PersonDoesNotExist riderId.getId)
  let lengthFrequentLocGeohashes = length frequentLocGeohashes
      currentFreqLocGeohashes = fromMaybe [] person.frequentLocGeohashes
      updatedFrequentLocGeohashes =
        if lengthFrequentLocGeohashes >= 10
          then take 10 frequentLocGeohashes
          else frequentLocGeohashes ++ take (10 - lengthFrequentLocGeohashes) currentFreqLocGeohashes
  void $ QP.updateFreqLocGeohashes (Just updatedFrequentLocGeohashes) riderId
