module Product.Cron where

import qualified Beckn.External.FCM.Flow as FCM
import qualified Beckn.External.FCM.Types as FCM
import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (authenticate, withFlowHandler)
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.ByteString.Base64 as DBB
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import Storage.Queries.Location as Loc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Products as Products
import System.Environment
import Types.API.CaseProduct
import qualified Types.API.Cron as API
import Utils.Common (verifyToken)

updateCases :: Maybe CronAuthKey -> API.ExpireCaseReq -> FlowHandler API.ExpireCaseRes
updateCases maybeAuth API.ExpireCaseReq {..} = withFlowHandler $ do
  authenticate maybeAuth
  cases <- (Case.findAllExpiredByStatus [Case.NEW] from to)
  traverse_
    ( \caseObj -> do
        let cId = Case._id caseObj
        Case.updateStatus cId Case.CLOSED
        CaseProduct.updateAllCaseProductsByCaseId cId CaseProduct.EXPIRED
        notifyOnExpiration caseObj
    )
    cases
  pure $ API.ExpireCaseRes $ length cases

notifyOnExpiration :: Case.Case -> L.Flow ()
notifyOnExpiration caseObj = do
  let caseId = Case._id caseObj
  let personId = Case._requestor caseObj
  let startTime = Case._startTime caseObj
  if isJust personId
    then do
      person <- Person.findById $ PersonId (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCM.FCMData
                  { _fcmNotificationType = FCM.EXPIRED_CASE,
                    _fcmShowNotification = FCM.SHOW,
                    _fcmEntityIds = show $ _getCaseId caseId,
                    _fcmEntityType = FCM.Case
                  }
              title = FCM.FCMNotificationTitle $ T.pack "Ride expired!"
              body =
                FCM.FCMNotificationBody $ T.pack $
                  "Your ride for "
                    <> formatTime defaultTimeLocale "%T, %F" startTime
                    <> " has expired as there were no replies."
                    <> " You can place a new request to get started again!"
          FCM.notifyPerson title body notificationData p
          pure ()
        _ -> pure ()
    else pure ()
