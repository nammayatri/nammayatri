{-# LANGUAGE OverloadedLabels #-}

module Product.Cron where

import qualified Beckn.External.FCM.Flow as FCM
import qualified Beckn.External.FCM.Types as FCM
import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as C
import qualified Beckn.Types.Storage.CaseProduct as CP
import qualified Beckn.Types.Storage.Person as PS
import qualified Beckn.Types.Storage.Products as P
import Beckn.Utils.Common (authenticate, withFlowHandler)
import Data.Aeson
import qualified Data.Text as T
import Data.Time
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.CaseProduct as CPQ
import qualified Storage.Queries.Person as PSQ
import qualified Storage.Queries.Products as PQ
import System.Environment
import Types.API.Cron

expire :: Maybe CronAuthKey -> ExpireCaseReq -> FlowHandler ExpireCaseRes
expire maybeAuth ExpireCaseReq {..} = withFlowHandler $ do
  authenticate maybeAuth
  cases <- CQ.findAllExpiredByStatus [C.NEW] C.RIDEBOOK from to
  caseProducts <- CPQ.findAllByCaseIds (C._id <$> cases)
  products <- PQ.findAllById (CP._productId <$> caseProducts)
  CQ.updateStatusByIds (C._id <$> cases) C.CLOSED
  CPQ.updateStatusByIds (CP._id <$> caseProducts) CP.EXPIRED
  notifyTransporters cases caseProducts products
  pure $ ExpireCaseRes $ length cases

notifyTransporters :: [C.Case] -> [CP.CaseProduct] -> [P.Products] -> L.Flow ()
notifyTransporters cases caseProducts products =
  traverse_
    ( \cp -> do
        let filteredProducts = filter (\x -> CP._productId cp == P._id x) products
        admins <- PSQ.findAllByOrgIds [PS.ADMIN] $ P._organizationId <$> filteredProducts
        let caseObj = filter (\x -> CP._caseId cp == C._id x) cases
        case caseObj of
          [] -> pure ()
          x : _ -> notifyTransporterOnExpiration x admins
    )
    caseProducts

notifyTransporterOnExpiration :: C.Case -> [PS.Person] -> L.Flow ()
notifyTransporterOnExpiration c =
  traverse_ (FCM.notifyPerson title body notificationData)
  where
    caseId = C._id c
    startTime = C._startTime c
    notificationData =
      FCM.FCMData
        { _fcmNotificationType = FCM.EXPIRED_CASE,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityIds = show $ _getCaseId caseId,
          _fcmEntityType = FCM.Case
        }
    title = FCM.FCMNotificationTitle $ T.pack "Ride expired!"
    body =
      FCM.FCMNotificationBody $ T.pack $
        "The ride request for "
          <> formatTime defaultTimeLocale "%T, %F" startTime
          <> " has expired as the customer failed to confirm."
          <> " You can view more details in the app."
