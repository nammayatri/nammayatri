{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import Beckn.Prelude
import Beckn.Utils.Common as CoreCommon
import Beckn.Utils.Shutdown
import qualified Crypto.Number.Generate as Cryptonite

generateOTPCode :: MonadFlow m => m Text
generateOTPCode =
  liftIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999

service ::
  ( Forkable m,
    MonadIO m,
    MonadReader r m,
    HasField "isShuttingDown" r Shutdown,
    MonadCatch m,
    Log m
  ) =>
  Text ->
  m () ->
  m ()
service name f =
  fork name . untilShutdown $
    f `catch` \e -> do
      log ERROR $ makeLogSomeException e
      threadDelay 1000000
