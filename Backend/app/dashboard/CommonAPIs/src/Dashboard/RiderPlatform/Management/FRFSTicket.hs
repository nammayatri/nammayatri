{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.RiderPlatform.Management.FRFSTicket where

import API.Types.RiderPlatform.Management.Endpoints.FRFSTicket
import Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart

instance FromMultipart Tmp UpsertRouteFareReq where
  fromMultipart form = do
    UpsertRouteFareReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp UpsertRouteFareReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]
