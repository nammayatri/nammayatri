{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.CallFeedback (postDriverCallFeedback) where

import qualified API.Types.UI.CallFeedback
import qualified API.Types.UI.CallFeedback as APITCallFeedback
import Data.OpenApi (ToSchema)
import qualified Domain.Types.CallFeedback as TCallFeedback
-- import qualified Storage.Queries.CallFeedbackOptions as CallFeedbackOptions
import qualified Domain.Types.CallStatus as CallStatus
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.APISuccess
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Id as ID
import qualified Kernel.Utils.Common as UC
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging (logDebug)
import Servant
import qualified Storage.Queries.CallFeedback as QCallFeedback
import qualified Storage.Queries.CallFeedbackOptions as QCallFeedbackOptions
import qualified Storage.Queries.CallStatus as QCallStatus
import Tools.Auth

postDriverCallFeedback ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.CallFeedback.CallFeedbackReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDriverCallFeedback (_, merchantId, merchantOperatingCityId) APITCallFeedback.CallFeedbackReq {..} = do
  callStatus <- QCallStatus.findByCallId callId >>= fromMaybeM CallStatusDoesNotExist
  logDebug $ "callStatus: " <> show callStatus
  mapM_
    ( \optionId -> do
        option <- QCallFeedbackOptions.findById (ID.Id optionId) >>= fromMaybeM (CallFeedbackOptionsDoesNotExist $ optionId)
        logDebug $ "option: " <> show option
    )
    optionIds
  let rideId = callStatus.entityId
  now <- UC.getCurrentTime
  uuid <- UC.generateGUIDText
  let callFeedbackObj =
        TCallFeedback.CallFeedback
          { callId = callId,
            entityId = rideId,
            id = ID.Id uuid,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            optionIds = optionIds,
            createdAt = now,
            updatedAt = now
          }

  void $ QCallFeedback.create callFeedbackObj
  pure Success
