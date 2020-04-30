module Beckn.Product.Document where

import qualified Beckn.Storage.Queries.Customer        as QC
import qualified Beckn.Storage.Queries.Document        as QD
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Customer          as SC
import           Beckn.Types.Storage.Document
import qualified Beckn.Types.Storage.RegistrationToken as SR
import           Beckn.Utils.Common
import           Beckn.Utils.Extra
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Digest.Pure.MD5
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant
import           Servant.Multipart

import qualified Data.Text                             as T

upload :: Maybe Text -> MultipartData Mem -> FlowHandler Ack
upload regToken multipartData = withFlowHandler $ do
  reg <- verifyToken regToken
  cust <- QC.findCustomerById (CustomerId $ SR._EntityId reg) >>=
          fromMaybeM400 "INVALID_DATA"
  let orgId =
        fromMaybe (OrganizationId "individual") (SC._OrganizationId cust)
      dir = storageDir orgId (SR._EntityId reg)
  docIds <-
    forM (files multipartData) $ \file -> uploadDocument file dir orgId
  -- :TODO link docIds to Customer
  return $ Ack "DONE" "Uploaded documents successfully"

uploadDocument :: FileData Mem -> Text -> OrganizationId -> L.Flow DocumentId
uploadDocument file dir orgId = do
  let contentB = fdPayload file
      content = BSL.toStrict contentB
      fileName = fdFileName file
  uuid <- generateGUID
  L.runIO $
    BS.writeFile
      (T.unpack $ dir <> (_getDocumentId uuid) <> "/" <> fileName)
      content
  now <- getCurrentTimeUTC
  let doc =
        Document
          uuid
          (dir <> (_getDocumentId uuid) <> "/")
          (BS.length content)
          (show $ md5 contentB)
          fileName
          (fdFileCType file)
          Nothing
          now
          now
  QD.create doc
  L.logInfo "Uploaded Document with name: " (show fileName)
  return uuid

storageDir orgId custId =
  "/local/storage/juspay/docs/" <>
  _getOrganizationId orgId <>
  "/" <>
  custId <>
  "/"
