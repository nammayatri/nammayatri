{-# OPTIONS_GHC -Wwarn=unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Domain.Action.Dashboard.AppManagement.MerchantOnboarding
  ( merchantOnboardingInfo,
    merchantOnboardingStart,
    merchantOnboardingList,
  )
where

import qualified Data.Map as Map
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.MerchantOnboardingStep
import qualified "this" Domain.Types.MerchantOnboardingStepConfig
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logInfo, throwError)
import Servant hiding (throwError)
import Storage.Queries.MerchantOnboarding as QMO
import Storage.Queries.MerchantOnboardingStep as QMOS
import Storage.Queries.MerchantOnboardingStepConfig as QMOSC
import Tools.Auth
import Kernel.Types.Common
import Kernel.Types.Error
import Tools.Error

mkMerchantOnboardingAPI :: Domain.Types.MerchantOnboarding.MerchantOnboarding -> [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep] -> Domain.Types.MerchantOnboarding.MerchantOnboardingAPI
mkMerchantOnboardingAPI onboarding steps =
  Domain.Types.MerchantOnboarding.MerchantOnboardingAPI
    { id = onboarding.id,
      requestorId = onboarding.requestorId,
      onboardingType = onboarding.onboardingType,
      description = onboarding.description,
      status = onboarding.status,
      remarks = onboarding.remarks,
      steps = steps,
      createdAt = onboarding.createdAt,
      updatedAt = onboarding.updatedAt
    }

getStepsAndUpdate :: Kernel.Types.Id.Id Domain.Types.MerchantOnboarding.MerchantOnboarding -> Environment.Flow [Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep]
getStepsAndUpdate onboardingId = do
  steps <- QMOS.findByMerchantOnboardingId (Kernel.Types.Id.getId onboardingId)

  let stepStatusMap =
        Kernel.Prelude.foldl'
          ( \acc step ->
              Map.insert (step.id.getId) step.status acc
          )
          Map.empty
          steps

  let updatedSteps = map (updateStepStatus' stepStatusMap) steps

  let stepsToUpdate =
        filter (\(original, updated) -> original.status /= updated.status) $
          zip steps updatedSteps

  forM_ stepsToUpdate $ \(_, updatedStep) ->
    QMOS.updateStepStatus updatedStep.status updatedStep.id

  return $ if null stepsToUpdate then steps else updatedSteps
  where
    updateStepStatus' statusMap step =
      case step.status of
        Domain.Types.MerchantOnboardingStep.AVAILABLE -> step
        Domain.Types.MerchantOnboardingStep.UNAVAILABLE ->
          if allDependenciesCompleted statusMap step.dependency
            then step {Domain.Types.MerchantOnboardingStep.status = Domain.Types.MerchantOnboardingStep.AVAILABLE}
            else step
        _ -> step

    allDependenciesCompleted statusMap dependencies =
      null dependencies
        || all
          ( \depId ->
              case Map.lookup depId.getId statusMap of
                Just Domain.Types.MerchantOnboardingStep.COMPLETED -> True
                _ -> False
          )
          dependencies

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo merchantShortId opCity onboardingType requestorId = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "Requestor ID is required")

  onboarding <- QMO.findByRequestorIdAndOnboardingType reqId onboardingType >>= fromMaybeM (InvalidRequest $ "No onboarding present of type " <> show onboardingType)

  steps <- getStepsAndUpdate onboarding.id

  return $ mkMerchantOnboardingAPI onboarding steps

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart merchantShortId opCity onboardingType requestorId = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "Requestor ID is required")

  mbOnboarding <- QMO.findByRequestorIdAndOnboardingType reqId onboardingType

  case mbOnboarding of
    Just onboarding -> do
      steps <- getStepsAndUpdate onboarding.id
      return $ mkMerchantOnboardingAPI onboarding steps
    Nothing -> do
      now <- getCurrentTime
      onboardingId <- generateGUID

      stepConfigs <- QMOSC.findByOnboardingType onboardingType
      when (null stepConfigs) $ throwError (InvalidRequest $ "No step configurations found for onboarding type " <> show onboardingType)

      let onboarding =
            Domain.Types.MerchantOnboarding.MerchantOnboarding
              { id = Kernel.Types.Id.Id onboardingId,
                requestorId = reqId,
                onboardingType = onboardingType,
                description = Just $ "Onboarding process for " <> show onboardingType,
                status = Domain.Types.MerchantOnboarding.INPROGRESS,
                remarks = Nothing,
                createdAt = now,
                updatedAt = now
              }

      QMO.create onboarding

      stepNameToIdMap <- foldM
        (\m config -> do
          stepId <- generateGUID
          return $ Map.insert config.stepNameIdentifier stepId m
        )
        Map.empty
        stepConfigs

      steps <- forM stepConfigs $ \config -> do
        stepId <- (Map.lookup config.stepNameIdentifier stepNameToIdMap) & fromMaybeM (InternalError $ "Step ID not found for " <> config.stepNameIdentifier)

        let initialStatus =
              if null config.dependency
                then Domain.Types.MerchantOnboardingStep.AVAILABLE
                else Domain.Types.MerchantOnboardingStep.UNAVAILABLE

        let step =
              Domain.Types.MerchantOnboardingStep.MerchantOnboardingStep
                { id = Kernel.Types.Id.Id stepId,
                  merchantOnboardingId = onboardingId,
                  stepNameIdentifier = config.stepNameIdentifier,
                  stepDescription = config.stepDescription,
                  dependency = [],
                  status = initialStatus,
                  isApprovalRequired = config.isApprovalRequired,
                  payload = Nothing,
                  remarks = Nothing,
                  createdAt = now,
                  updatedAt = now
                }

        QMOS.create step
        return (step, config.dependency)

      forM_ steps $ \(step, dependencyNames) -> do
        let stepDependencies = mapMaybe (\depName -> Map.lookup depName stepNameToIdMap) dependencyNames
        when (not $ null stepDependencies) $
          QMOS.updateDependency (map Kernel.Types.Id.Id stepDependencies) step.id

      updatedSteps <- getStepsAndUpdate onboarding.id

      return $ mkMerchantOnboardingAPI onboarding updatedSteps

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList merchantShortId opCity requestorId = do
  reqId <- requestorId & fromMaybeM (InvalidRequest "Requestor ID is required")
  onboardings <- QMO.findAllByRequestorId reqId
  return onboardings
