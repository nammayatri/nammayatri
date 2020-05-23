module Product.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as Storage
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.Location as Loc
import Beckn.Utils.Common (withFlowHandler)
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Person as SP
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.CaseProduct as DB
import qualified Storage.Queries.Products as PQ
import Storage.Queries.Location as LQ
import System.Environment
import Types.API.CaseProduct

list :: Maybe Text -> CaseProdReq -> FlowHandler CaseProductList
list regToken CaseProdReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  person <- QP.findPersonById (PersonId _EntityId)
  case SP._organizationId person of
    Just orgId -> do
      prodList <- case _type of
        Just statusList -> PQ.findAllByTypeOrgId orgId statusList
        Nothing -> PQ.findAllByOrgId orgId
      caseProdList <- DB.findAllByIds _limit _offset (Product._id <$> prodList)
      caseList <- CQ.findAllByIds (Storage._caseId <$> caseProdList)
      locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
      return $ catMaybes $ joinIds prodList caseList locList <$> caseProdList
    Nothing ->
      L.throwException $ err400 {errBody = "organisation id is missing"}
  where
    joinIds :: [Product.Products] -> [Case.Case] -> [Loc.Location] -> Storage.CaseProduct -> Maybe CaseProductRes
    joinIds prodList caseList locList caseProd =
      case find (\x -> (Storage._caseId caseProd) == Case._id x) caseList of
        Just k -> buildResponse k
        Nothing -> Nothing
      where
        buildResponse k = (prepare locList caseProd k) <$> find (\z -> (Storage._productId caseProd) == Product._id z) prodList
        prepare locList caseProd cs prod =
          CaseProductRes
            { _case = cs,
              _product = prod,
              _caseProduct = caseProd,
              _fromLocation = find (\x -> (Case._fromLocationId cs == _getLocationId (Loc._id x))) locList,
              _toLocation = find (\x -> (Case._toLocationId cs == _getLocationId (Loc._id x))) locList
            }
