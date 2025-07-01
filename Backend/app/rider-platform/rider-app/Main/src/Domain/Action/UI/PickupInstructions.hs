{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PickupInstructions
  ( postPickupinstructions,
    getPickupinstructions,
  )
where

import qualified API.Types.UI.PickupInstructions as API
import Data.List (take)
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.PickupInstructions as DPI
import qualified Domain.Types.RiderConfig as DRC
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PickupInstructions as QPI
import Tools.Auth
import Tools.Error

postPickupinstructions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.PickupInstructionsReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPickupinstructions (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  when (length req.pickupInstructions > riderConfig.pickupInstructionsThreshold) $
    throwError $ InvalidRequest $ "Maximum " <> show riderConfig.pickupInstructionsThreshold <> " pickup instructions are allowed"

  mbExistingInstructions <- QPI.findByPersonId personId

  now <- getCurrentTime
  case mbExistingInstructions of
    Just _ -> do
      QPI.updatePickupInstructionsByPersonId req.pickupInstructions personId
    Nothing -> do
      pickupInstructionsId <- generateGUID
      let newInstructions =
            DPI.PickupInstructions
              { DPI.id = pickupInstructionsId,
                DPI.personId = personId,
                DPI.merchantId = merchantId,
                DPI.merchantOperatingCityId = person.merchantOperatingCityId,
                DPI.pickupInstructions = req.pickupInstructions,
                DPI.createdAt = now,
                DPI.updatedAt = now
              }
      QPI.create newInstructions

  pure Kernel.Types.APISuccess.Success

getPickupinstructions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.PickupInstructionsResp
  )
getPickupinstructions (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  mbPickupInstructions <- QPI.findByPersonId personId

  let instructions = maybe [] DPI.pickupInstructions mbPickupInstructions

  return $
    API.PickupInstructionsResp
      { pickupInstructions = instructions
      }
