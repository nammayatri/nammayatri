module Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Ride as DR
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
    toObject [name, value] = do
      let valueArr = T.splitOn "&" value
      case valueArr of
        [element] -> (A.fromText $ T.strip name :: A.Key) A..= fromMaybe A.Null (textToMaybeValue (T.strip element) :: Maybe A.Value)
        elements -> do
          let jsonValues = map A.String elements
          (A.fromText $ T.strip name :: A.Key) A..= A.Array (Vector.fromList jsonValues)
    toObject [name] = (A.fromText $ T.strip name :: A.Key) A..= A.Null
    toObject xs = do
      let reconstructed = T.intercalate "#" xs
      (A.fromText $ T.strip reconstructed :: A.Key) A..= A.Null

isDropInsideThreshold :: DB.Booking -> DTConf.TransporterConfig -> LatLong -> Bool
isDropInsideThreshold booking thresholdConfig currLoation = do
  let dropLocThreshold = metersToHighPrecMeters thresholdConfig.dropLocThreshold
      locationDiff = maybe 0 (\toLocation -> abs $ distanceBetweenInMeters (getCoordinates toLocation) currLoation) booking.toLocation
   in locationDiff <= dropLocThreshold

isValidRide :: DR.Ride -> Bool
isValidRide ride = maybe True (elem "ValidRide#Yes") ride.rideTags -- TODO: How to remove hardcode string
