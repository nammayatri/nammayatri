{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Dashboard.ProviderPlatform.Operator.Driver where

import API.Types.ProviderPlatform.Operator.Endpoints.Driver
import Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart

-- Create Drivers using csv --

instance FromMultipart Tmp CreateDriversReq where
  fromMultipart form = do
    fileData <- lookupFile "file" form
    pure $
      CreateDriversReq
        { file = fdPayload fileData
        }

instance ToMultipart Tmp CreateDriversReq where
  toMultipart form = do
    MultipartData [] [FileData "file" (T.pack form.file) "" form.file]
