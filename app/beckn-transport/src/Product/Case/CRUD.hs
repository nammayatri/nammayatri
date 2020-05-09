module Product.Case.CRUD where

import           Beckn.Types.App
import           Beckn.Types.Common as BC
import           Beckn.Types.Storage.Case as Case
import           Beckn.Types.Storage.CaseProduct as CaseP
import           Beckn.Types.Storage.Products as Product
import qualified Data.Accessor as Lens
import           Data.Aeson
import qualified Data.Text as T
import           Data.Time.LocalTime
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import           Servant
import           Storage.Queries.Case as Case
import           System.Environment
import           Types.API.Case
import           Types.API.Registration
import           Types.App
import           Utils.Routes


list :: CaseReq -> FlowHandler CaseListRes
list CaseReq {..} = withFlowHandler $ do
  Case.findAllByType _limit _offset _type _status

-- Update Case
-- Transporter Accepts a Ride with Quote
update :: Text -> UpdateCaseReq -> FlowHandler Case
update caseId UpdateCaseReq {..} = withFlowHandler $ do
  c <- Case.findById $ CaseId caseId
  case _transporterChoice of
    "ACCEPTED" -> do
      p   <- createProduct c _quote
      cp  <- createCaseProduct c p
      notifyGateway c
      return c
    "DECLINED" -> return c

createProduct :: Case -> Maybe Double -> L.Flow Products
createProduct = undefined

createCaseProduct :: Case -> Products -> L.Flow CaseProduct
createCaseProduct = undefined

notifyGateway :: Case -> L.Flow ()
notifyGateway _ = undefined