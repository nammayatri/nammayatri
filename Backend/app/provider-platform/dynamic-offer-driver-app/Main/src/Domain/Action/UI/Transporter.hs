{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import qualified Domain.Types.Merchant.MerchantConfig as DMC
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id (Id (..))
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified Storage.CachedQueries.Merchant.MerchantConfig as CQMC
import Tools.Error

newtype TransporterRec = TransporterRec
  { organization :: DMC.MerchantConfigAPIEntity
  }
  deriving (Generic, ToJSON, ToSchema)

data UpdateTransporterReq = UpdateTransporterReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateTransporterRes = DMC.MerchantConfigAPIEntity

validateUpdateTransporterReq :: Validate UpdateTransporterReq
validateUpdateTransporterReq UpdateTransporterReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name
    ]

updateTransporter :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => SP.Person -> Id DM.Merchant -> UpdateTransporterReq -> m UpdateTransporterRes
updateTransporter admin merchantId req = do
  unless (merchantId == admin.merchantId) $ throwError AccessDenied
  runRequestValidation validateUpdateTransporterReq req
  org <-
    CQMC.findByMerchantId merchantId
      >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  let updOrg =
        org{DMC.name = fromMaybe (org.name) (req.name),
            DMC.description = (req.description) <|> (org.description),
            DMC.enabled = fromMaybe (org.enabled) (req.enabled)
           }
  _ <- CQMC.update updOrg
  CQMC.clearCache updOrg
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateTransporter : ") (show updOrg)
  return $ DMC.makeMerchantConfigAPIEntity updOrg

getTransporter :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => (Id SP.Person, Id DM.Merchant) -> m TransporterRec
getTransporter (_, merchantId) = do
  TransporterRec . DMC.makeMerchantConfigAPIEntity <$> (CQMC.findByMerchantId merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId))
