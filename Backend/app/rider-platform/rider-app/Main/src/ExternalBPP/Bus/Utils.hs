module ExternalBPP.Bus.Utils where

import qualified BecknV2.FRFS.Enums as Spec
import Data.Aeson as A
import Data.List (groupBy, nub)
import qualified Data.Map as M
import Domain.Types.AadhaarVerification as DAadhaarVerification
import Domain.Types.FRFSTicketDiscount as DFRFSTicketDiscount
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Pass
import Domain.Types.PassCategory
import Domain.Types.PassType
import qualified Domain.Types.Person as DP
import Domain.Types.PurchasedPass
import qualified Domain.Types.Route as Route
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import Storage.Queries.AadhaarVerification as QAV
import Storage.Queries.FRFSTicketDiscount as QFRFSTicketDiscount
import qualified Storage.Queries.Pass as QPass
import qualified Storage.Queries.PassCategory as QPassCategory
import qualified Storage.Queries.PassType as QPassType
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Tools.DynamicLogic
import Tools.Error

data FRFSTicketDiscountDynamic = FRFSTicketDiscountDynamic
  { aadhaarData :: Maybe DAadhaarVerification.AadhaarVerification,
    discounts :: [DFRFSTicketDiscount.FRFSTicketDiscount]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

getFRFSTicketDiscountWithEligibility ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Spec.VehicleCategory ->
  Id DP.Person ->
  [Id FRFSTicketDiscount] ->
  m [(FRFSTicketDiscount, Bool)]
getFRFSTicketDiscountWithEligibility merchantId merchantOperatingCityId vehicleType personId applicableDiscountIds = do
  availableDiscounts <-
    pure . catMaybes
      =<< mapM
        ( \applicableDiscountId -> QFRFSTicketDiscount.findByIdAndVehicleAndCity applicableDiscountId vehicleType merchantId merchantOperatingCityId
        )
        applicableDiscountIds
  aadhaarVerification <- QAV.findByPersonId personId
  applicableDiscounts <- do
    let ticketDiscountData = FRFSTicketDiscountDynamic {aadhaarData = aadhaarVerification, discounts = availableDiscounts}
    localTime <- getLocalCurrentTime 19800 -- Fix Me
    (allLogics, _) <- getAppDynamicLogic (cast merchantOperatingCityId) LYT.FRFS_DISCOUNTS localTime Nothing
    response <- try @_ @SomeException $ LYTU.runLogics allLogics ticketDiscountData
    case response of
      Left e -> do
        logError $ "Error in running FRFS Discount Logic - " <> show e <> " - " <> show ticketDiscountData <> " - " <> show allLogics
        return []
      Right resp ->
        case (A.fromJSON resp.result :: Result FRFSTicketDiscountDynamic) of
          A.Success result -> return result.discounts
          A.Error err -> do
            logError $ "Error in parsing FRFSTicketDiscountDynamic - " <> show err <> " - " <> show resp <> " - " <> show ticketDiscountData <> " - " <> show allLogics
            return []
  return $ mergeDiscounts availableDiscounts applicableDiscounts
  where
    mergeDiscounts availableDiscounts applicableDiscounts =
      map (\discount -> (discount, discount `elem` applicableDiscounts)) availableDiscounts

data RedeemPassEligibilityData = RedeemPassEligibilityData
  { aadhaarData :: Maybe DAadhaarVerification.AadhaarVerification,
    passData :: Domain.Types.Pass.Pass,
    passTypeData :: Domain.Types.PassType.PassType,
    passCategoryData :: Domain.Types.PassCategory.PassCategory,
    purchasedPassData :: Maybe Domain.Types.PurchasedPass.PurchasedPass,
    vehicleServiceTierType :: Spec.ServiceTierType,
    startStationCode :: Text,
    endStationCode :: Text,
    orderQty :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

getEligibleRedeemPasses ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Spec.VehicleCategory ->
  Id DP.Person ->
  [Id Domain.Types.Pass.Pass] ->
  Text ->
  Text ->
  Int ->
  m [(RedeemPassEligibilityData, Bool)]
getEligibleRedeemPasses _merchantId _merchantOperatingCityId _vehicleType personId applicablePassIds startStationCode endStationCode quantity = do
  availablePasses <-
    mapM (QPass.findById >=> fromMaybeM (PassNotFound "Pass not found")) applicablePassIds

  purchasedPasses <- QPurchasedPass.findByPersonId personId

  let purchasedPassMap = M.fromList [(purchasedPass.passId, purchasedPass) | purchasedPass <- purchasedPasses]

  aadhaarVerification <- QAV.findByPersonId personId
  currentTime <- getCurrentTime
  eligibilityResults <- forM availablePasses $ \pass -> do
    passType <- QPassType.findById pass.passTypeId >>= fromMaybeM (PassTypeNotFound "Pass type not found")
    passCategory <- QPassCategory.findById passType.passCategoryId >>= fromMaybeM (PassCategoryNotFound "Pass category not found")

    case M.lookup pass.id purchasedPassMap of
      Nothing -> do
        logError $ "No matching PurchasedPass found for Pass ID: " <> show pass.id
        let passEligibilityData =
              RedeemPassEligibilityData
                { aadhaarData = aadhaarVerification,
                  passData = pass,
                  passTypeData = passType,
                  passCategoryData = passCategory,
                  purchasedPassData = Nothing,
                  vehicleServiceTierType = pass.vehicleServiceTierType,
                  startStationCode = startStationCode,
                  endStationCode = endStationCode,
                  orderQty = quantity
                }
        return (passEligibilityData, False)
      Just purchasedPass -> do
        let isNotExpired = maybe False (> currentTime) purchasedPass.expiryDate
        let passEligibilityData =
              RedeemPassEligibilityData
                { aadhaarData = aadhaarVerification,
                  passData = pass,
                  passTypeData = passType,
                  passCategoryData = passCategory,
                  purchasedPassData = Just purchasedPass,
                  vehicleServiceTierType = pass.vehicleServiceTierType,
                  startStationCode = startStationCode,
                  endStationCode = endStationCode,
                  orderQty = quantity
                }

        if not isNotExpired
          then do
            logError $ "PurchasedPass is expired for Pass ID: " <> show pass.id
            return (passEligibilityData, False)
          else do
            eligibilityResult <- try @_ @SomeException $ LYTU.runLogics pass.redeemEligibilityJsonLogic passEligibilityData
            case eligibilityResult of
              Left err -> do
                logError $ "Error in running Redeem Pass Eligibility Logic: " <> show err
                return (passEligibilityData, False)
              Right logicResp ->
                case (A.fromJSON logicResp.result :: Result Bool) of
                  A.Success isEligible -> return (passEligibilityData, isEligible)
                  A.Error err -> do
                    logError $ "Error in parsing Redeem Pass Eligibility Logic result: " <> show err
                    return (passEligibilityData, False)

  return eligibilityResults

data RouteStopInfo = RouteStopInfo
  { route :: Route.Route,
    startStopCode :: Text,
    endStopCode :: Text,
    totalStops :: Maybe Int,
    travelTime :: Maybe Seconds
  }

getPossibleRoutesBetweenTwoStops :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m [RouteStopInfo]
getPossibleRoutesBetweenTwoStops startStationCode endStationCode = do
  routesWithStop <- B.runInReplica $ QRouteStopMapping.findByStopCode startStationCode
  let routeCodes = nub $ map (.routeCode) routesWithStop
  routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCodes routeCodes
  currentTime <- getCurrentTime
  let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
      groupedStops = groupBy (\a b -> a.routeCode == b.routeCode) serviceableStops
      possibleRoutes =
        nub $
          catMaybes $
            map
              ( \stops ->
                  let mbStartStopSequence = (.sequenceNum) <$> find (\stop -> stop.stopCode == startStationCode) stops
                   in find
                        ( \stop ->
                            maybe
                              False
                              (\startStopSequence -> stop.stopCode == endStationCode && stop.sequenceNum > startStopSequence)
                              mbStartStopSequence
                        )
                        stops
                        <&> ( \stop -> do
                                case mbStartStopSequence of
                                  Just startStopSequence ->
                                    let totalStops = stop.sequenceNum - startStopSequence
                                        totalTravelTime =
                                          foldr
                                            ( \stop' acc ->
                                                if stop'.sequenceNum > startStopSequence && stop'.sequenceNum <= stop.sequenceNum
                                                  then case (acc, stop'.estimatedTravelTimeFromPreviousStop) of
                                                    (Just acc', Just travelTime) -> Just (acc' + travelTime)
                                                    _ -> Nothing
                                                  else acc
                                            )
                                            (Just $ Seconds 0)
                                            stops
                                     in (stop.routeCode, Just totalStops, totalTravelTime)
                                  Nothing -> (stop.routeCode, Nothing, Nothing)
                            )
              )
              groupedStops
  routes <- QRoute.findByRouteCodes (map (\(routeCode, _, _) -> routeCode) possibleRoutes)
  return $
    map
      ( \route ->
          let routeData = find (\(routeCode, _, _) -> routeCode == route.code) possibleRoutes
           in RouteStopInfo
                { route,
                  totalStops = (\(_, totalStops, _) -> totalStops) =<< routeData,
                  startStopCode = startStationCode,
                  endStopCode = endStationCode,
                  travelTime = (\(_, _, travelTime) -> travelTime) =<< routeData
                }
      )
      routes

-- TODO :: This to be handled from OTP, Currently Hardcode for Chennai
getPossibleTransitRoutesBetweenTwoStops :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m [[RouteStopInfo]]
getPossibleTransitRoutesBetweenTwoStops startStationCode endStationCode = do
  case (startStationCode, endStationCode) of
    ("MBTcSIip", "jQaLNViL") -> do
      routes <- QRoute.findByRouteCodes ["jylLjHej", "BTuKbmBy"]
      return $
        [ map
            ( \route ->
                if route.code == "jylLjHej"
                  then
                    RouteStopInfo
                      { route,
                        totalStops = Just 6,
                        startStopCode = "MBTcSIip",
                        endStopCode = "TiulEaYs",
                        travelTime = Just $ Seconds 660
                      }
                  else
                    RouteStopInfo
                      { route,
                        totalStops = Just 8,
                        startStopCode = "TiulEaYs",
                        endStopCode = "jQaLNViL",
                        travelTime = Just $ Seconds 1440
                      }
            )
            routes
        ]
    _ -> return []
