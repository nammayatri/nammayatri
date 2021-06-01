{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.Cron where

import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Flow
import Beckn.Utils.Error
import Control.Monad.Reader
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import GHC.Records (HasField (..))

authenticate ::
  ( HasField "cronAuthKey" r (Maybe CronAuthKey)
  ) =>
  Maybe CronAuthKey ->
  FlowR r ()
authenticate rauth = do
  key <- asks $ getField @"cronAuthKey"
  let parsed = T.stripPrefix "Basic " =<< rauth
  unless (key == parsed) $ throwError (AuthBlocked "Bad auth key")
