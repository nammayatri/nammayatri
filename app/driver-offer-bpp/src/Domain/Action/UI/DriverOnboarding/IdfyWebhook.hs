{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.IdfyWebhook
  ( onVerify,
  )
where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Utils.Common
import Environment

onVerify :: Value -> Flow AckResponse
onVerify val = withLogTag "IdfyWebhook" $ do
  logInfo $ show val
  pure Ack
