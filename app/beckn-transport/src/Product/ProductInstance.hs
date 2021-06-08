{-# LANGUAGE OverloadedLabels #-}

module Product.ProductInstance where

import App.Types
import Beckn.External.FCM.Types as FCM
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App as BC
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as V
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Models.Case as CQ
import qualified Models.Case as Case
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.Allocation as AQ
import qualified Storage.Queries.Case as QCase
import Storage.Queries.Location as LQ
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as PersQ
import qualified Storage.Queries.ProductInstance as PIQ
import qualified Storage.Queries.Vehicle as VQ
import qualified Types.API.Case as APICase
import Types.API.ProductInstance
import Types.App
import Types.Error
import Utils.Common
import qualified Utils.Defaults as Default
import qualified Utils.Notifications as Notify

list :: SR.RegistrationToken -> [PI.ProductInstanceStatus] -> [Case.CaseType] -> Maybe Int -> Maybe Int -> FlowHandler ProductInstanceList
list SR.RegistrationToken {..} status csTypes limitM offsetM = withFlowHandlerAPI $ do
  person <- PersQ.findPersonById (Id entityId)
  case SP.organizationId person of
    Just orgId -> do
      result <- PIQ.productInstanceJoin limit offset csTypes orgId status
      locList <- LQ.findAllByLocIds (Case.fromLocationId <$> (_case <$> result)) (Case.toLocationId <$> (_case <$> result))
      return $ buildResponse locList <$> result
    Nothing ->
      throwError (PersonFieldNotPresent "organization_id")
  where
    limit = fromMaybe Default.limit limitM
    offset = fromMaybe Default.offset offsetM
    buildResponse :: [Loc.Location] -> ProductInstanceRes -> ProductInstanceRes
    buildResponse locList res =
      ProductInstanceRes
        { _case = res ^. #_case,
          product = res ^. #product,
          productInstance = res ^. #productInstance,
          fromLocation = find (\x -> Case.fromLocationId (res ^. #_case) == Loc.id x) locList,
          toLocation = find (\x -> Case.toLocationId (res ^. #_case) == Loc.id x) locList
        }

notifyUpdateToBAP :: PI.ProductInstance -> PI.ProductInstance -> PI.ProductInstanceStatus -> Flow ()
notifyUpdateToBAP searchPi orderPi updatedStatus = do
  -- Send callback to BAP
  bapOrg <- fetchBapOrganization $ orderPi ^. #caseId
  bapCallbackUrl <- bapOrg ^. #callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  notifyTripDetailsToGateway searchPi orderPi bapCallbackUrl
  notifyStatusUpdateReq searchPi updatedStatus bapCallbackUrl
  where
    fetchBapOrganization caseId = do
      prodCase <- fetchCase caseId >>= fromMaybeM CaseNotFound
      bapOrgId <- prodCase ^. #udf4 & fromMaybeM (CaseFieldNotPresent "udf4")
      OQ.findOrganizationById $ Id bapOrgId
    fetchCase caseId = do
      QCase.findById caseId

listDriverRides :: SR.RegistrationToken -> Id SP.Person -> FlowHandler RideListRes
listDriverRides SR.RegistrationToken {..} personId = withFlowHandlerAPI $ do
  user <- PersQ.findPersonById (Id entityId)
  person <- PersQ.findPersonById personId
  hasAccess user person
  rideList <- PIQ.findAllByPersonId (SP.id person)
  locList <- LQ.findAllByLocIds (catMaybes (PI.fromLocation <$> rideList)) (catMaybes (PI.toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    hasAccess user person =
      when
        ( (user ^. #role) /= SP.ADMIN && (user ^. #id) /= (person ^. #id)
            || (user ^. #organizationId) /= (person ^. #organizationId)
        )
        $ throwError Unauthorized
    joinByIds locList ride =
      find (\x -> PI.fromLocation ride == Just (Loc.id x)) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> PI.toLocation ride == Just (Loc.id x)) locList
        prepare pRide from to =
          RideRes
            { product = pRide,
              fromLocation = from,
              toLocation = to
            }

listVehicleRides :: SR.RegistrationToken -> Id V.Vehicle -> FlowHandler RideListRes
listVehicleRides SR.RegistrationToken {..} vehicleId = withFlowHandlerAPI $ do
  user <- PersQ.findPersonById (Id entityId)
  vehicle <- VQ.findVehicleById vehicleId
  hasAccess user vehicle
  rideList <- PIQ.findAllByVehicleId (Just vehicleId)
  locList <- LQ.findAllByLocIds (catMaybes (PI.fromLocation <$> rideList)) (catMaybes (PI.toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    hasAccess user vehicle =
      when
        ( isNothing (SP.organizationId user)
            || (SP.organizationId user /= (V.organizationId <$> vehicle))
        )
        $ throwError Unauthorized
    joinByIds locList ride =
      find (\x -> PI.fromLocation ride == Just (Loc.id x)) locList
        >>= buildResponse
      where
        buildResponse k = prepare ride k <$> find (\x -> PI.toLocation ride == Just (Loc.id x)) locList
        prepare pRide from to =
          RideRes
            { product = pRide,
              fromLocation = from,
              toLocation = to
            }

listCasesByProductInstance :: SR.RegistrationToken -> Id PI.ProductInstance -> Maybe Case.CaseType -> FlowHandler APICase.CaseListRes
listCasesByProductInstance SR.RegistrationToken {..} piId csType = withFlowHandlerAPI $ do
  prodInst <- PIQ.findById piId
  piList <-
    prodInst ^. #parentId & fromMaybeM (PIFieldNotPresent "parent_id")
      >>= PIQ.findAllByParentId
  caseList <- case csType of
    Just type_ -> CQ.findAllByIdType (PI.caseId <$> piList) type_
    Nothing -> CQ.findAllByIds (PI.caseId <$> piList)
  locList <- LQ.findAllByLocIds (Case.fromLocationId <$> caseList) (Case.toLocationId <$> caseList)
  return $ catMaybes $ joinByIds locList <$> caseList
  where
    joinByIds locList cs =
      find (\x -> Case.fromLocationId cs == Loc.id x) locList
        >>= buildResponse
      where
        buildResponse k = prepare cs k <$> find (\x -> Case.toLocationId cs == Loc.id x) locList
        prepare pcs from to =
          APICase.CaseRes
            { _case = pcs,
              fromLocation = from,
              toLocation = to
            }

-- Core Utility methods are below

assignDriver :: Id PI.ProductInstance -> Id Driver -> Flow ()
assignDriver productInstanceId driverId = do
  ordPi <- PIQ.findById productInstanceId
  searchPi <- PIQ.findById =<< fromMaybeM (PIFieldNotPresent "parent_id") (ordPi ^. #parentId)
  piList <-
    ordPi ^. #parentId & fromMaybeM (PIFieldNotPresent "parent_id")
      >>= PIQ.findAllByParentId
  headPi <- case piList of
    p : _ -> pure p
    [] -> throwError PIDoesNotExist
  driver <- PersQ.findPersonById $ cast driverId
  vehicleId <-
    driver ^. #udf1
      & fromMaybeM (PersonFieldNotPresent "udf1 - vehicle")
      <&> Id
  vehicle <-
    VQ.findVehicleById vehicleId
      >>= fromMaybeM VehicleNotFound
  let piIdList = PI.id <$> piList

  DB.runSqlDBTransaction (AQ.assignDriver productInstanceId piIdList vehicle driver)

  fork "assignDriver - Notify BAP" $ do
    notifyUpdateToBAP searchPi ordPi PI.TRIP_ASSIGNED
    Notify.notifyDriver notificationType notificationTitle (message headPi) driver
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message p' =
      unwords
        [ "You have been assigned a ride for",
          showTimeIst (PI.startTime p') <> ".",
          "Check the app for more details."
        ]

notifyTripDetailsToGateway :: PI.ProductInstance -> PI.ProductInstance -> BaseUrl -> Flow ()
notifyTripDetailsToGateway searchPi orderPi bapCallbackUrl = do
  trackerCase <- CQ.findByParentCaseIdAndType (searchPi ^. #caseId) Case.LOCATIONTRACKER
  transporter <- OQ.findOrganizationById $ searchPi ^. #organizationId
  let bppShortId = getShortId $ transporter ^. #shortId
  parentCase <- CQ.findById (searchPi ^. #caseId)
  case (trackerCase, parentCase) of
    (Just x, y) -> BP.notifyTripInfoToGateway orderPi x y bapCallbackUrl bppShortId
    _ -> return ()

notifyStatusUpdateReq :: PI.ProductInstance -> PI.ProductInstanceStatus -> BaseUrl -> Flow ()
notifyStatusUpdateReq searchPi status bapCallbackUrl = do
  transporterOrg <- findOrganization
  let bppShortId = getShortId $ transporterOrg ^. #shortId
  searchCase <- Case.findById $ searchPi ^. #caseId
  let txnId = last . T.splitOn "_" . getShortId $ searchCase ^. #shortId
  case status of
    PI.CANCELLED -> do
      admins <- getAdmins transporterOrg
      BP.notifyCancelToGateway (searchPi ^. #id) bapCallbackUrl bppShortId txnId
      Notify.notifyCancelReqByBP searchPi admins
    PI.TRIP_REASSIGNMENT -> do
      admins <- getAdmins transporterOrg
      Notify.notifyDriverCancelledRideRequest searchPi admins
      notifyStatusToGateway bppShortId txnId
    _ -> notifyStatusToGateway bppShortId txnId
  where
    findOrganization = OQ.findOrganizationById $ searchPi ^. #organizationId
    getAdmins transporterOrg = do
      if transporterOrg ^. #enabled
        then PersQ.findAllByOrgIds [SP.ADMIN] [PI.organizationId searchPi]
        else pure []
    notifyStatusToGateway bppShortId txnId = do
      trackerPi <- PIQ.findByParentIdType (searchPi ^. #id) Case.LOCATIONTRACKER
      BP.notifyServiceStatusToGateway (searchPi ^. #id) trackerPi bapCallbackUrl bppShortId txnId
