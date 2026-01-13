module Main where

import App (runMockPayment)
import App.Types (AppCfg)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Dhall (readDhallConfig)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  configPath <- fromMaybe "./dhall-configs/dev/mock-payment.dhall" <$> lookupEnv "MOCK_PAYMENT_CONFIG_PATH"
  cfg :: AppCfg <- readDhallConfig configPath
  runMockPayment cfg
