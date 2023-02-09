module Domain.Action.UI.Transporter
  ( TransporterRec (..),
    UpdateTransporterReq (..),
    UpdateTransporterRes,
    updateTransporter,
    getTransporter,
  )
where

import Control.Applicative
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id (Id (..))
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QP
import Tools.Error

newtype TransporterRec = TransporterRec
  { organization :: DM.MerchantAPIEntity
  }
  deriving (Generic, ToJSON, ToSchema)

data UpdateTransporterReq = UpdateTransporterReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateTransporterRes = DM.MerchantAPIEntity

validateUpdateTransporterReq :: Validate UpdateTransporterReq
validateUpdateTransporterReq UpdateTransporterReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name
    ]

updateTransporter :: (CacheFlow m r, EsqDBFlow m r) => SP.Person -> Id DM.Merchant -> UpdateTransporterReq -> m UpdateTransporterRes
updateTransporter admin merchantId req = do
  unless (merchantId == admin.merchantId) $ throwError AccessDenied
  runRequestValidation validateUpdateTransporterReq req
  org <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  let updOrg =
        org{DM.name = fromMaybe (org.name) (req.name),
            DM.description = (req.description) <|> (org.description),
            DM.enabled = fromMaybe (org.enabled) (req.enabled)
           }
  Esq.runTransaction $ CQM.update updOrg
  CQM.clearCache updOrg
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateTransporter : ") (show updOrg)
  return $ DM.makeMerchantAPIEntity updOrg

getTransporter :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SP.Person -> m TransporterRec
getTransporter personId = do
  person <-
    Esq.runInReplica $
      QP.findById personId
        >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantId = person.merchantId
  TransporterRec . DM.makeMerchantAPIEntity <$> (CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId))
