module Core.ACL.OnConfirm where

import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import Domain.Action.Beckn.Confirm

buildOnConfirmReq :: Monad m => DOnConfirmReq -> m OnConfirm.OnConfirmMessage
buildOnConfirmReq = pure
