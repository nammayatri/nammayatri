{-# LANGUAGE QuantifiedConstraints #-}

module App
  ( runMockPayment,
  )
where

import App.Routes (mockPaymentAPI, mockPaymentServer)
import App.Types
import Kernel.Beam.Connection.Postgres (preparePsqlMasterConnection, preparePsqlR1Connection, prepareTables)
import Kernel.Prelude
import Kernel.Types.Common (defaultTableData)
import Kernel.Utils.Servant.Server
import Servant

runMockPayment :: AppCfg -> IO ()
runMockPayment cfg = do
  appEnv <- buildAppEnv cfg
  let initDBConnections flowRt = do
        preparePsqlMasterConnection (esqDBCfg cfg)
        preparePsqlR1Connection (esqDBCfg cfg)
        prepareTables defaultTableData
        pure flowRt
  runServerWithHealthCheck appEnv mockPaymentAPI mockPaymentServer identity identity EmptyContext releaseAppEnv initDBConnections
