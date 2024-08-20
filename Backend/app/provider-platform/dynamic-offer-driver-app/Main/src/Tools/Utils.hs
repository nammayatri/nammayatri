module Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Text as T
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Maps.HasCoordinates (getCoordinates)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Distance (metersToHighPrecMeters)
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Lib.Yudhishthira.Tools.Utils

convertTags :: [Text] -> A.Value
convertTags input = A.object $ map toObject pairs
  where
    pairs = map (T.splitOn "#") input
    toObject [name, value] = (A.fromText $ T.strip name :: A.Key) A..= fromMaybe A.Null (textToMaybeValue (T.strip value) :: Maybe A.Value)
    toObject [name] = (A.fromText $ T.strip name :: A.Key) A..= A.Null
    toObject xs = do
      let reconstructed = T.intercalate "#" xs
      (A.fromText $ T.strip reconstructed :: A.Key) A..= A.Null

isDropInsideThreshold :: DB.Booking -> DTConf.TransporterConfig -> LatLong -> Bool
isDropInsideThreshold booking thresholdConfig currLoation = do
  let dropLocThreshold = metersToHighPrecMeters thresholdConfig.dropLocThreshold
      locationDiff = maybe 0 (\toLocation -> abs $ distanceBetweenInMeters (getCoordinates toLocation) currLoation) booking.toLocation
   in locationDiff <= dropLocThreshold
