module SharedLogic.KaalChakra.Actions (kaalChakraAction, Action) where

import qualified Domain.Types.Person as DPerson
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common

data Action
  = SAFE_TO_UNSAFE_COHORT
  | UNSAFE_TO_SAFE_COHORT
  deriving (Show, Read)

kaalChakraAction ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  Id DPerson.Person ->
  Action ->
  m ()
kaalChakraAction personId action = case action of
  SAFE_TO_UNSAFE_COHORT -> do
    logInfo $ "Kaal chakra action: " <> show action <> "; personId: " <> show personId
  UNSAFE_TO_SAFE_COHORT -> do
    logInfo $ "Kaal chakra action: " <> show action <> "; personId: " <> show personId
