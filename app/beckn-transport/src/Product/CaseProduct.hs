{-# LANGUAGE OverloadedLabels #-}

module Product.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseP
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (withFlowHandler)
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.CaseProduct as DB
import Storage.Queries.Location as LQ
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Products as PQ
import qualified Storage.Queries.RegistrationToken as QR
import System.Environment
import Types.API.CaseProduct
import qualified Utils.Defaults as Default

list :: RegToken -> [CaseP.CaseProductStatus] -> Maybe Int -> Maybe Int -> FlowHandler CaseProductList
list regToken status limitM offsetM = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  person <- QP.findPersonById (PersonId _EntityId)
  case SP._organizationId person of
    Just orgId -> do
      result <- DB.caseProductJoin limit offset Case.RIDEBOOK orgId status
      locList <- LQ.findAllByLocIds (Case._fromLocationId <$> (_case <$> result)) (Case._toLocationId <$> (_case <$> result))
      return $ buildResponse locList <$> result
    Nothing ->
      L.throwException $ err400 {errBody = "organisation id is missing"}
  where
    limit = fromMaybe Default.limit limitM
    offset = fromMaybe Default.offset offsetM
    buildResponse :: [Loc.Location] -> CaseProductRes -> CaseProductRes
    buildResponse locList res =
      CaseProductRes
        { _case = res ^. #_case,
          _product = res ^. #_product,
          _caseProduct = res ^. #_caseProduct,
          _fromLocation = find (\x -> (Case._fromLocationId (res ^. #_case) == _getLocationId (Loc._id x))) locList,
          _toLocation = find (\x -> (Case._toLocationId (res ^. #_case) == _getLocationId (Loc._id x))) locList
        }
