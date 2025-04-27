module Storage.GraphqlQueries.RouteStopTimeTable
  ( findByRouteCodeAndStopCode,
  )
where

import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopTimeTable
import Domain.Utils (utctTimeToDayOfWeek)
import Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.GraphqlQueries.Client as Client
import Tools.MultiModal

getISTTimeInfo :: UTCTime -> (Double, UTCTime)
getISTTimeInfo currentTime =
  let istOffset :: Double = 5.5 * 3600
      currentTimeIST = addUTCTime (secondsToNominalDiffTime $ round istOffset) currentTime
   in (istOffset, currentTimeIST)

findByRouteCodeAndStopCode ::
  ( MonadFlow m,
    ServiceFlow m r
  ) =>
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  [Text] ->
  Text ->
  m [RouteStopTimeTable]
findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOpId routeCodes stopCode = do
  now <- getCurrentTime
  let variables =
        Client.RouteStopTimeTableQueryVars
          { Client.routeCode = routeCodes,
            Client.stopCode = stopCode
          }
  transitreq <- getTransitServiceReq merchantId merchantOpId
  baseUrl <- case transitreq of
    MultiModalTypes.OTPTransitConfig cfg -> return cfg.baseUrl
    _ -> throwError $ InternalError "Transit service request is not OTPTransitConfig"
  result <- Client.executeRouteStopTimeTableQuery baseUrl variables
  logDebug $ "GraphQL query result: " <> show result

  case result of
    Left err -> do
      logError $ "GraphQL query failed: " <> show err
      pure []
    Right response -> do
      let entries' = response.routeStopTimeTables
      let (_, currentTimeIST) = getISTTimeInfo now
          today = fromEnum $ utctTimeToDayOfWeek currentTimeIST
      let entries = filter (\entry -> entry.routeCode `elem` routeCodes && (length entry.serviceability > today && (entry.serviceability !! today) > 0)) entries'
      pure $ map (parseToRouteStopTimeTable integratedBPPConfig merchantId merchantOpId) entries

-- Helper function to convert GraphQL response to domain type
parseToRouteStopTimeTable ::
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Client.TimetableEntry ->
  RouteStopTimeTable
parseToRouteStopTimeTable integratedBPPConfig mid mocid entry =
  RouteStopTimeTable
    { integratedBppConfigId = integratedBPPConfig,
      routeCode = entry.routeCode,
      serviceTierType = entry.serviceTierType,
      stopCode = entry.stopCode,
      timeOfArrival = entry.timeOfArrival,
      timeOfDeparture = entry.timeOfDeparture,
      tripId = Id entry.tripId,
      merchantId = Just mid,
      merchantOperatingCityId = Just mocid,
      createdAt = entry.createdAt,
      updatedAt = entry.updatedAt,
      serviceability = Just entry.serviceability,
      providedByGraphQl = Just True
    }
