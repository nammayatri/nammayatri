module Storage.Queries.Transformers.FRFSSearch where

import Kernel.Prelude
import qualified Lib.JourneyLeg.Types

mkJourneyLegInfo :: (Maybe Text -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe Lib.JourneyLeg.Types.JourneySearchData)
mkJourneyLegInfo agency (Just convenienceCost) isDeleted (Just journeyId) (Just journeyLegOrder) pricingId (Just skipBooking) = Just $ Lib.JourneyLeg.Types.JourneySearchData {..}
mkJourneyLegInfo _ _ _ _ _ _ _ = Nothing

mkCrisSearchData ::
  Maybe Text -> -- crisSdkToken
  Maybe Text -> -- crisEncryptedTicketData
  Maybe Text -> -- bookAuthCode
  Maybe Text -> -- deviceId
  Maybe Int -> -- osBuildVersion
  Maybe Text -> -- osType
  Maybe Lib.JourneyLeg.Types.CrisSearchData
mkCrisSearchData crisSdkToken crisEncryptedTicketData bookAuthCode deviceId osBuildVersion osType =
  Just $
    Lib.JourneyLeg.Types.CrisSearchData
      { crisSdkToken = crisSdkToken,
        crisEncryptedTicketData = crisEncryptedTicketData,
        bookAuthCode = bookAuthCode,
        deviceId = deviceId,
        osBuildVersion = osBuildVersion,
        osType = osType
      }
