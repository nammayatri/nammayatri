module Jobs.Common (buildRunKaalChakraJobReq) where

import Environment
import Kernel.Prelude
import Lib.Yudhishthira.Types

buildRunKaalChakraJobReq ::
  Chakra ->
  Flow Lib.Yudhishthira.Types.RunKaalChakraJobReq
buildRunKaalChakraJobReq chakra = do
  updateUserTags <- asks (.updateUserTags)
  usersInBatch <- asks (.usersInBatch)
  maxBatches <- asks (.maxBatches)
  parseQueryResults <- asks (.parseQueryResults)
  let batchDelayInSec = 2 -- FIXME
  let action = Lib.Yudhishthira.Types.RUN -- no matter for job handler
  let completeOldJob = Nothing -- no matter for job handler
  pure Lib.Yudhishthira.Types.RunKaalChakraJobReq {usersSet = Lib.Yudhishthira.Types.ALL_USERS, ..}
