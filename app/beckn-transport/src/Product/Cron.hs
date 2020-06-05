{-# LANGUAGE OverloadedLabels #-}

module Product.Cron where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common (withFlowHandler)
import Data.Aeson
import qualified Data.ByteString.Base64 as DBB
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.CaseProduct as CPQ
import qualified Storage.Queries.Products as PQ
import System.Environment
import Types.API.Cron

updateCases :: Maybe Text -> ExpireCaseReq -> FlowHandler ExpireCaseRes
updateCases maybeAuth ExpireCaseReq {..} = withFlowHandler $ do
  authenticate maybeAuth
  cases <- CQ.findAllExpiredByStatus [Case.NEW] Case.RIDEBOOK from to
  caseProducts <- CPQ.findAllByCaseIds (Case._id <$> cases)
  products <- PQ.findAllById (CaseProduct._productId <$> caseProducts)
  CQ.updateStatusByIds (Case._id <$> cases) Case.CLOSED
  CPQ.updateStatusByIds (CaseProduct._id <$> caseProducts) Product.EXPIRED
  PQ.updateStatusByIds (Product._id <$> products) Product.EXPIRED
  pure $ ExpireCaseRes $ length cases

authenticate :: Maybe Text -> L.Flow ()
authenticate maybeAuth = do
  keyM <- L.runIO $ lookupEnv "CRON_AUTH_KEY"
  let authHeader = join $ (T.stripPrefix "Basic ") <$> maybeAuth
      decodedAuthM =
        DT.decodeUtf8
          <$> (join $ ((rightToMaybe . DBB.decode . DT.encodeUtf8) <$> authHeader))
  case (decodedAuthM, keyM) of
    (Just auth, Just key) -> do
      when ((T.pack key) /= auth) throw401
      return ()
    _ -> throw401
  where
    throw401 =
      L.throwException $
        err401 {errBody = "Invalid Auth"}
