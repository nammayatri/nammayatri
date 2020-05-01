module Beckn.Product.Document where

import qualified Beckn.Storage.Queries.Customer        as QC
import qualified Beckn.Storage.Queries.Document        as QD
import qualified Beckn.Storage.Queries.User            as QU
import           Beckn.Types.API.Document
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Customer          as SC
import           Beckn.Types.Storage.Document          as SD
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.User              as SU
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

import           Beckn.Types.Storage.EntityDocument
import qualified Data.Text                             as T

upload ::
  Maybe Text -> Text -> DocumentEntity -> MultipartData Mem -> FlowHandler UpdateDocumentRes
upload regToken enId enType multipartData = withFlowHandler $ do
  reg <- verifyToken regToken
  orgId <- getOrgId enId enType
  let dir = storageDir orgId enId
  documents <-
    forM (files multipartData) $ \file ->
      uploadDocument file dir orgId
  traverse (createEntity enId enType) documents
  return $
    UpdateDocumentRes $
      _getDocumentId . SD._id <$> documents

createEntity :: Text -> DocumentEntity -> Document -> L.Flow EntityDocument
createEntity custId enType Document {..} = do
  uuid <- generateGUID
  now <- getCurrentTimeUTC
  return $
   EntityDocument
    { _id = uuid
    , _EntityId = custId
    , _entityType = enType
    , _DocumentId = _getDocumentId _id
    , _documentType = _format
    , _CreatedBy = custId
    , _createdByEntityType = enType
    , _verified = False
    , _VerifiedBy = Nothing
    , _verifiedByEntityType = Nothing
    , _createdAt = now
    , _updatedAt = now
    , _info = Nothing
    }

uploadDocument :: FileData Mem -> Text -> OrganizationId -> L.Flow Document
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
  return doc

getOrgId :: Text -> DocumentEntity -> L.Flow OrganizationId
getOrgId ei eit = do
  case eit of
    CUSTOMER -> do
      cust <-
        QC.findCustomerById (CustomerId ei) >>=
          fromMaybeM400 "INVALID_CUSTOMER_ID"
      return $
        fromMaybe
          (OrganizationId "individual")
          (SC._OrganizationId cust)
    USER -> do
      user <-
        QU.findById (UserId ei) >>=
          fromMaybeM400 "INVALID_USER_ID"
      return $ SU._OrganizationId user

storageDir :: OrganizationId -> Text -> Text
storageDir orgId custId =
  "/local/storage/juspay/docs/" <>
  _getOrganizationId orgId <>
  "/" <>
  custId <>
  "/"
