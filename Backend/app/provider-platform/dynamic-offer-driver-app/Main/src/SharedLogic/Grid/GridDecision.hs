module SharedLogic.Grid.GridDecision
  ( callGridDecision,
    describeGridFailure,
  )
where

import qualified Control.Monad.Catch as Catch
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.External.Grid.Error as GridError
import qualified Kernel.External.Grid.Flow as Grid
import qualified Kernel.External.Grid.Types as Grid
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Network.HTTP.Types (statusCode)
import qualified Servant.Client as Servant
import Tools.Grid (getGridCfg)

callGridDecision ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, CoreMetrics m, HasRequestId r, MonadReader r m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Grid.GridQuestions ->
  NonEmpty Text ->
  m (Maybe Grid.GridDecisionResp)
callGridDecision merchantId merchantOpCityId state questions rawImages = do
  result <- Catch.try @_ @SomeException runCheck
  case result of
    Left err
      | Just (asyncErr :: SomeAsyncException) <- fromException err -> Catch.throwM asyncErr
      | otherwise -> do
        logWarning $ "Grid decision call failed: " <> describeGridFailure err
        pure Nothing
    Right mbResp -> pure mbResp
  where
    runCheck =
      getGridCfg merchantId merchantOpCityId >>= \case
        Nothing -> pure Nothing
        Just gridCfg ->
          case NE.nonEmpty (mapMaybe (eitherToMaybe . Grid.mkGridImageFromBase64 gridCfg.maxImageBytes) (toList rawImages)) of
            Nothing -> pure Nothing
            Just images ->
              Just
                <$> Grid.decide
                  gridCfg
                  Grid.GridDecisionReq
                    { model = gridCfg.model,
                      state,
                      images = toList images,
                      questions
                    }
    eitherToMaybe = either (const Nothing) Just

describeGridFailure :: SomeException -> Text
describeGridFailure err
  | Just (gridErr :: GridError.GridError) <- fromException err = show gridErr
  | Just (apiErr :: ExternalAPICallError) <- fromException err = case apiErr.clientError of
    Servant.FailureResponse _ resp -> "HTTP status " <> show (statusCode (Servant.responseStatusCode resp))
    _ -> "request error (detail withheld)"
  | otherwise = "unexpected error (detail withheld)"
