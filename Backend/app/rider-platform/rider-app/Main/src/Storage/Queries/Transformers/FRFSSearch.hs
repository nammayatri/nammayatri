module Storage.Queries.Transformers.FRFSSearch where

import Kernel.Prelude
import qualified Lib.JourneyLeg.Types

mkJourneyLegInfo :: (Maybe Text -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Lib.JourneyLeg.Types.JourneySearchData)
mkJourneyLegInfo agency (Just convenienceCost) isDeleted (Just journeyId) (Just journeyLegOrder) pricingId (Just skipBooking) = Just $ Lib.JourneyLeg.Types.JourneySearchData {..}
mkJourneyLegInfo _ _ _ _ _ _ _ = Nothing

mkCrisSearchData ::
  Maybe Text -> -- bookAuthCode
  Maybe Int -> -- crisAppSession
  Maybe Int -> -- crisRouteId
  Maybe Text -> -- crisSdkToken
  Maybe Text -> -- deviceId
  Maybe Int -> -- distance
  Maybe Text -> -- osBuildVersion
  Maybe Text -> -- osType
  Maybe Text -> -- trainType
  Maybe Text -> -- via
  Maybe Lib.JourneyLeg.Types.CrisSearchData
mkCrisSearchData bookAuthCode_ crisAppSession_ crisRouteId_ crisSdkToken_ deviceId_ distance_ osBuildVersion_ osType_ trainType_ via_ =
  Just $
    Lib.JourneyLeg.Types.CrisSearchData
      { crisSdkToken = crisSdkToken_,
        bookAuthCode = bookAuthCode_,
        deviceId = deviceId_,
        osBuildVersion = osBuildVersion_,
        osType = osType_,
        distance = distance_,
        trainType = trainType_,
        crisAppSession = crisAppSession_,
        via = via_,
        crisRouteId = crisRouteId_
      }
