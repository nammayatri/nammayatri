module Beckn.Product.Pass where

import qualified Beckn.Data.Accessor                  as Accessor
import qualified Beckn.Storage.Queries.CustomerDetail as QCD
import qualified Beckn.Storage.Queries.Pass           as QP
import           Beckn.Types.API.Pass
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.CustomerDetail   as SCD
import           Beckn.Types.Storage.Pass
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified EulerHS.Language                     as L
import           EulerHS.Prelude
import           Servant

getPassById :: Maybe Text -> Text -> FlowHandler PassRes
getPassById regToken passId =
  withFlowHandler $ do
    reg <- verifyToken regToken
    QP.findPassById passId >>=
      maybe
        (L.throwException $ err400 {errBody = "INVALID_DATA"})
        (return . PassRes)

updatePass :: Maybe Text -> Text -> UpdatePassReq -> FlowHandler PassRes
updatePass regToken passId req =
  withFlowHandler $ do
    reg <- verifyToken regToken
    pass <-
      QP.findPassById passId >>=
      maybe (L.throwException $ err400 {errBody = "INVALID_DATA"}) return
    QP.updatePassStatus (action req) passId
    return $ PassRes (pass {_status = action req})

listPass ::
     Maybe Text
  -> PassIDType
  -> Text
  -> Maybe Int
  -> Maybe Int
  -> PassType
  -> FlowHandler ListPassRes
listPass regToken passIdType passV limitM offsetM passType =
  withFlowHandler $ do
    reg <- verifyToken regToken
    listBy <- getListBy
    ListPassRes <$> maybe (return []) getPasses listBy
  where
    getListBy =
      case passIdType of
        ORGANIZATIONID ->
          return $ Just $ QP.ByOrganizationId (OrganizationId passV)
        PASSAPPLICATIONID ->
          return $ Just $ QP.ByApplicationId (PassApplicationId passV)
        CUSTOMERID -> return $ Just $ QP.ByCustomerId (CustomerId passV)
        MOBILENUMBER -> do
          detail <- QCD.findByIdentifier SCD.MOBILENUMBER passV
          return $ (QP.ByCustomerId . SCD._CustomerId) <$> detail

    getPasses listBy =
      case (toEnum <$> limitM, toEnum <$> offsetM) of
        (Just l, Just o) -> QP.listAllPassesWithOffset l o listBy []
        _                -> QP.listAllPasses listBy []
