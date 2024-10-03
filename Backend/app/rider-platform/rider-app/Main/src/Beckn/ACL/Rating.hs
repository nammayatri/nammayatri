{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Rating (buildRatingReqV2) where

import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Action.UI.Feedback as DFeedback
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

buildRatingReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DFeedback.FeedbackRes ->
  m Spec.RatingReq
buildRatingReqV2 res@DFeedback.FeedbackRes {..} = do
  msgId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  moc <- CQMOC.findByMerchantIdAndCity merchant.id city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show city)
  bapConfigs <- QBC.findByMerchantIdDomainandMerchantOperatingCityId res.merchant.id "MOBILITY" moc.id
  bapConfig <- listToMaybe bapConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show res.merchant.id.getId <> " merchantOperatingCityId " <> show moc.id.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
  ttl <- bapConfig.ratingTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.RATING Context.MOBILITY msgId (Just transactionId) merchant.bapId bapUrl (Just providerId) (Just providerUrl) merchant.defaultCity merchant.country (Just ttl)
  let message = tfMessage res
  pure $
    Spec.RatingReq
      { ratingReqContext = context,
        ratingReqMessage = message
      }

tfMessage :: DFeedback.FeedbackRes -> Spec.RatingReqMessage
tfMessage res = do
  Spec.RatingReqMessage
    { ratingReqMessageRatings = Just [tfRating res]
    }

tfRating :: DFeedback.FeedbackRes -> Spec.Rating
tfRating res@DFeedback.FeedbackRes {..} = do
  Spec.Rating
    { ratingId = Just $ bppBookingId.getId,
      ratingValue = Just $ show ratingValue,
      ratingRatingCategory = Nothing,
      ratingFeedbackForm = Just $ tfFeedbackForm res,
      ratingTag = guard isValueAddNP >> mkRatingTags res.shouldFavDriver res.riderPhoneNum res.filePath res.riderName
    }

tfFeedbackForm :: DFeedback.FeedbackRes -> [Spec.FeedbackForm]
tfFeedbackForm DFeedback.FeedbackRes {..} = do
  [ Spec.FeedbackForm
      { feedbackFormQuestion = "Evaluate your ride experience.",
        feedbackFormAnswer = feedbackDetails
      },
    Spec.FeedbackForm
      { feedbackFormQuestion = "Was Assistance Offered?",
        feedbackFormAnswer = case wasOfferedAssistance of
          Just True -> Just "True"
          Just False -> Just "False"
          _ -> Nothing
      },
    Spec.FeedbackForm
      { feedbackFormQuestion = "Get IssueId.",
        feedbackFormAnswer = issueId
      }
    ]

mkRatingTags :: Maybe Bool -> Maybe Text -> Maybe Text -> Text -> Maybe [Spec.TagGroup]
mkRatingTags mbShouldFavDriver mbPhoneNum mbFilePath riderName =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.RATING_TAGS,
                  descriptorName = Just "Rating Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList = mkPersonTag mbShouldFavDriver <> mkPersonNumberTag mbPhoneNum <> mkFilePathTag mbFilePath <> mkPersonName riderName
        }
    ]

mkPersonTag :: Maybe Bool -> Maybe [Spec.Tag]
mkPersonTag mbShouldFavDriver =
  mbShouldFavDriver <&> \shouldFavDriver ->
    [ Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.SHOULD_FAVOURITE_DRIVER,
                  descriptorName = Just "Should Favourite Driver",
                  descriptorShortDesc = Nothing
                },
          tagDisplay = Just False,
          tagValue = Just $ show shouldFavDriver
        }
    ]

mkPersonNumberTag :: Maybe Text -> Maybe [Spec.Tag]
mkPersonNumberTag mbPhoneNum =
  mbPhoneNum <&> \phoneNum ->
    [ Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.RIDER_PHONE_NUMBER,
                  descriptorName = Just "Rider Phone Number",
                  descriptorShortDesc = Nothing
                },
          tagDisplay = Just False,
          tagValue = Just phoneNum
        }
    ]

mkFilePathTag :: Maybe Text -> Maybe [Spec.Tag]
mkFilePathTag mbFilePath =
  mbFilePath <&> \filePath ->
    [ Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.MEDIA_FILE_PATH,
                  descriptorName = Just "Media File Path",
                  descriptorShortDesc = Nothing
                },
          tagDisplay = Just False,
          tagValue = Just filePath
        }
    ]

mkPersonName :: Text -> Maybe [Spec.Tag]
mkPersonName riderName = do
  Just
    [ Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.RIDER_NAME,
                  descriptorName = Just "Rider Name",
                  descriptorShortDesc = Nothing
                },
          tagDisplay = Just False,
          tagValue = Just riderName
        }
    ]
