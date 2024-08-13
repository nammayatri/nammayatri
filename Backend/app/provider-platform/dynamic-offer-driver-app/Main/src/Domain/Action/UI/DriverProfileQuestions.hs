{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.DriverProfileQuestions where

import qualified API.Types.UI.DriverProfileQuestions
import qualified AWS.S3 as S3
import Data.Maybe (fromJust)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time.Calendar (diffDays)
import Data.Time.Clock (utctDay)
import qualified Domain.Types.DriverProfileQuestions as DTDPQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common as KUC
import Servant
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.DriverProfileQuestions as DPQ
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.Error

data ImageType = JPG | PNG | UNKNOWN deriving (Generic, Show, Eq)

postDriverProfileQues ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq ->
    Flow APISuccess
  )
postDriverProfileQues (mbPersonId, _, merchantOpCityId) req@API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq {..} =
  do
    driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
    person <- QP.findById driverId >>= fromMaybeM (PersonNotFound ("No person found with id" <> show driverId))
    driverStats <- QDS.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound ("No person found with id" <> show driverId))
    now <- getCurrentTime
    DPQ.upsert
      ( DTDPQ.DriverProfileQuestions
          { updatedAt = now,
            createdAt = now,
            driverId = driverId,
            hometown = hometown,
            merchantOperatingCityId = merchantOpCityId,
            pledges = pledges,
            aspirations = toMaybe aspirations,
            drivingSince = drivingSince,
            imageIds = toMaybe imageIds,
            vehicleTags = toMaybe vehicleTags,
            aboutMe = generateAboutMe person driverStats now req
          }
      )
      >> pure Success
  where
    toMaybe xs = guard (not (null xs)) >> Just xs

    -- Generate with LLM or create a template text here
    generateAboutMe person driverStats now req' = Just (hometownDetails req'.hometown <> "I have been with Nammayatri for " <> (withNY now person.createdAt) <> "months. " <> writeDriverStats driverStats <> genAspirations req'.aspirations)

    hometownDetails mHometown = case mHometown of
      Just hometown' -> "Hailing from " <> hometown' <> ", "
      Nothing -> ""

    withNY now createdAt = T.pack $ show $ diffDays (utctDay now) (utctDay createdAt) `div` 30

    writeDriverStats driverStats = ratingStat driverStats <> cancellationStat driverStats

    ratingStat driverStats =
      let avgRating = divideMaybe driverStats.totalRatingScore driverStats.totalRatings
       in if avgRating > Just 4.82 && isJust avgRating
            then ("I have an average rating of " <> T.pack (show $ fromJust avgRating) <> " and is among the top 10 percentile. ")
            else ""

    cancellationStat driverStats =
      let cancRate = divideMaybe driverStats.ridesCancelled driverStats.totalRidesAssigned
       in if (cancRate < Just 0.04 && isJust cancRate)
            then (if ratingStat driverStats == "" then "" else "Also, ") <> "I have a very low cancellation rate of " <> (T.pack $ show $ fromJust cancRate) <> " that ranks among top 10 percentile. "
            else ""

    genAspirations aspirations' = "With the earnings from my trips, I aspire to " <> T.toLower (T.intercalate ", " aspirations')

    divideMaybe :: Maybe Int -> Maybe Int -> Maybe Double
    divideMaybe mNum mDenom = do
      num <- mNum
      denom <- mDenom
      if denom == 0
        then Nothing
        else Just (fromIntegral num / fromIntegral denom)

getDriverProfileQues ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Flow API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
  )
getDriverProfileQues (mbPersonId, _merchantId, _merchantOpCityId) =
  mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
    >>= DPQ.findByPersonId
    >>= \case
      Just res ->
        getImages (maybe [] (Id <$>) res.imageIds)
          >>= \images ->
            pure $
              API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
                { aspirations = fromMaybe [] res.aspirations,
                  hometown = res.hometown,
                  pledges = res.pledges,
                  drivingSince = res.drivingSince,
                  vehicleTags = fromMaybe [] res.vehicleTags,
                  otherImages = images, -- fromMaybe [] res.images
                  profileImage = Nothing,
                  otherImageIds = fromMaybe [] res.imageIds
                }
      Nothing ->
        pure $
          API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
            { aspirations = [],
              hometown = Nothing,
              pledges = [],
              drivingSince = Nothing,
              vehicleTags = [],
              otherImages = [],
              profileImage = Nothing,
              otherImageIds = []
            }
  where
    getImages imageIds = do
      mapM (QMF.findById) imageIds <&> catMaybes <&> ((.url) <$>)
        >>= mapM (S3.get . T.unpack . extractFilePath)

    extractFilePath url = case T.splitOn "filePath=" url of
      [_before, after] -> after
      _ -> T.empty
