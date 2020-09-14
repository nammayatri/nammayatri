{-# LANGUAGE OverloadedLabels #-}

module Product.Delhivery.Flow where

import App.Types
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified External.Delhivery.Flow as API
import External.Delhivery.Types
import qualified Storage.Queries.Delhivery as Dl
import Types.Common
import Types.Wrapper

fetchToken :: DlBAConfig -> DelhiveryConfig -> Flow Token
fetchToken DlBAConfig {..} DelhiveryConfig {..} = do
  mToken <- Dl.getToken dlClientId
  case mToken of
    Nothing -> do
      eres <- API.getToken dlTokenUrl (TokenReq dlClientId dlClientSecret "client_credentials")
      case eres of
        Left err -> throwError500 $ show err
        Right tokenRes -> do
          Dl.insertToken dlClientId (tokenRes ^. #access_token)
          return (tokenRes ^. #access_token)
    Just token -> return token
