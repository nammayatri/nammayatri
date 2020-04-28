module Beckn.Product.Registration where

import qualified Beckn.Data.Accessor as Lens
import qualified Beckn.Storage.Queries.Customer as QC
import qualified Beckn.Storage.Queries.RegistrationToken as QR
import Beckn.Types.Common
import Beckn.Types.API.Registration
import Beckn.Types.App
import qualified Beckn.Types.Storage.Customer as SC
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Extra
import Beckn.Utils.Routes
import Data.Aeson
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandler $ do
    uuid <- L.generateGUID
    now <- getCurrentTimeUTC
    let cust =
          SC.Customer
            (CustomerId uuid)
            Nothing
            Nothing
            False
            (req ^. Lens.role)
            Nothing
            now
            now
    QC.create cust
    hash <- undefined -- sendOTPToUser
    uuidR <- L.generateGUID
    let regToken =
          SR.RegistrationToken
            uuidR
            (req ^. Lens.medium)
            (req ^. Lens._type)
            hash
            False
            10 -- :TODO Config
            uuid
            now
            now
    QR.create regToken
    return $
      InitiateLoginRes
        { attempts = 3 -- :TODO Config
        , tokenId = uuidR
        }

login :: Text -> LoginReq -> FlowHandler LoginRes
login tokenId req = withFlowHandler $ do
  rep <-
    runMaybeT $ do
      SR.RegistrationToken {..} <- MaybeT $ QR.findRegistrationToken tokenId
      cust <- MaybeT $ QC.findCustomerById $ CustomerId _CustomerId
      MaybeT $
        if _authMedium == req ^. Lens.medium &&
           _authType ==  req ^. Lens._type &&
           _authValueHash == req ^. Lens.hash
          then return $ Just $ LoginRes tokenId cust
          else Just <$> (L.throwException $ err400 {errBody = "INVALID_VALUE"})
  maybe (L.throwException $ err400 {errBody = "INVALID_TOKEN"}) pure rep

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req = withFlowHandler $ do
  rep <-
    runMaybeT $ do
      SR.RegistrationToken {..} <- MaybeT $ QR.findRegistrationToken tokenId
      cust <- MaybeT $ QC.findCustomerById $ CustomerId _CustomerId
      MaybeT $ undefined -- sendSameOTPToUser _authValueHash
      MaybeT $ return $ Just $ InitiateLoginRes tokenId 2
  maybe (L.throwException $ err400 {errBody = "INVALID_TOKEN"}) pure rep
