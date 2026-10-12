module Lib.Yudhishthira.Event.KaalChakra.Metrics
  ( withChakraQueryMetric,
  )
where

import Kernel.Prelude
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.ChakraQueries as DCQ
import qualified Prometheus as P

-- | KaalChakra query executions, labelled by query_name from chakra_queries so a failing query can be identified.
{-# NOINLINE chakraQueryCounter #-}
chakraQueryCounter :: P.Vector P.Label4 P.Counter
chakraQueryCounter =
  P.unsafeRegister . P.vector ("chakra", "query_name", "query_type", "result") . P.counter $
    P.Info "chakra_query_counter" "KaalChakra query executions by chakra, query_name, query_type and result (success/empty/failed)"

-- | Counts a single chakra query run as success/empty/failed. The error is rethrown, so job behaviour is unchanged.
withChakraQueryMetric ::
  (MonadIO m, MonadCatch m, Log m) =>
  LYT.Chakra ->
  DCQ.ChakraQueries ->
  (a -> Bool) ->
  m a ->
  m a
withChakraQueryMetric chakra chakraQuery isEmpty action = do
  let queryType = maybe "UNKNOWN" show chakraQuery.queryType
      incrementQueryCounter result =
        liftIO $ P.withLabel chakraQueryCounter (show chakra, chakraQuery.queryName, queryType, result) P.incCounter
  res <- try @_ @SomeException action
  case res of
    Left err -> do
      logError $ "Chakra query failed: chakra=" <> show chakra <> "; query_name=" <> chakraQuery.queryName <> "; error=" <> show err
      incrementQueryCounter "failed"
      throwM err
    Right queryResult -> do
      incrementQueryCounter (if isEmpty queryResult then "empty" else "success")
      pure queryResult
