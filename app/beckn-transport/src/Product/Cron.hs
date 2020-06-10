{-# LANGUAGE OverloadedLabels #-}

module Product.Cron where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common (authenticate, withFlowHandler)
import Data.Aeson
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.CaseProduct as CPQ
import qualified Storage.Queries.Products as PQ
import System.Environment
import Types.API.Cron

expire :: Maybe CronAuthKey -> ExpireCaseReq -> FlowHandler ExpireCaseRes
expire maybeAuth ExpireCaseReq {..} = withFlowHandler $ do
  authenticate maybeAuth
  cases <- CQ.findAllExpiredByStatus [Case.NEW] Case.RIDEBOOK from to
  caseProducts <- CPQ.findAllByCaseIds (Case._id <$> cases)
  products <- PQ.findAllById (CaseProduct._productId <$> caseProducts)
  CQ.updateStatusByIds (Case._id <$> cases) Case.CLOSED
  CPQ.updateStatusByIds (CaseProduct._id <$> caseProducts) Product.EXPIRED
  PQ.updateStatusByIds (Product._id <$> products) Product.EXPIRED
  pure $ ExpireCaseRes $ length cases
