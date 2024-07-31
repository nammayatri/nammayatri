{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.DriverProfileQuestions where

import qualified API.Types.UI.DriverProfileQuestions
import qualified AWS.S3 as S3
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.DriverProfileQuestions as DTDPQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
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
postDriverProfileQues (mbPersonId, _, merchantOpCityId) API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq {..} =
  do
    B.runInReplica (mbPersonId & fromMaybeM (PersonNotFound "No person found"))
    >>= \driverId ->
      getCurrentTime
        >>= \now ->
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
                  aboutMe = generateAboutMe
                }
            )
            >> pure Success
  where
    toMaybe xs = guard (not (null xs)) >> Just xs

    -- Generate with LLM or create a template text here
    generateAboutMe = Just "Sample text goes here"

getDriverProfileQues ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Flow API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
  )
getDriverProfileQues (mbPersonId, _merchantId, _merchantOpCityId) =
  mbPersonId & fromMaybeM (PersonNotFound "No person found")
    >>= DPQ.findByPersonId
    >>= \case
      Just res ->
        getImages (maybe [] (Id <$>) res.imageIds)
          >>= \images ->
            pure $
              API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
                { aspirations = fromMaybe [] res.vehicleTags,
                  hometown = res.hometown,
                  pledges = res.pledges,
                  drivingSince = res.drivingSince,
                  vehicleTags = fromMaybe [] res.vehicleTags,
                  otherImages = images, -- fromMaybe [] res.images
                  profileImage = Nothing
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
              profileImage = Nothing
            }
  where
    getImages imageIds =
      mapM (QMF.findById) imageIds
        >>= pure . catMaybes
        >>= pure . ((.url) <$>)
        >>= mapM (S3.get . T.unpack)
