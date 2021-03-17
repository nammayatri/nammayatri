{-# LANGUAGE OverloadedLabels #-}

module Product.RideAPI.StartRide where

import App.Types (FlowHandler)
import qualified Beckn.Types.APIResult as APIResult
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (withFlowHandler)
import EulerHS.Prelude
import qualified Models.Case as QCase
import qualified Models.ProductInstance
import Product.ProductInstance (notifyUpdateToBAP)
import qualified Product.RideAPI.Handlers.StartRide as Handler
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QProductInstance

startRide :: SR.RegistrationToken -> Text -> Text -> FlowHandler APIResult.APIResult
startRide SR.RegistrationToken {..} rideId otp = withFlowHandler $ do
  Handler.startRideHandler handle _EntityId rideId otp
  where
    handle =
      Handler.ServiceHandle
        { findPersonById = QPerson.findPersonById,
          findPIById = QProductInstance.findById,
          findPIsByParentId = QProductInstance.findAllByParentId . Just,
          findCaseByIdsAndType = QCase.findByIdType,
          updatePIsStatus = Models.ProductInstance.updateStatusByIds,
          updateCaseStatus = QCase.updateStatus,
          notifyBAPRideStarted = \searchPi orderPi -> notifyUpdateToBAP searchPi orderPi (Just ProductInstance.INPROGRESS)
        }
