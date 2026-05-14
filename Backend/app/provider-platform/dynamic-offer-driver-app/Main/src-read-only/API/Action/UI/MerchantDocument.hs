{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.MerchantDocument
  ( API,
    handler,
  )
where

import qualified API.Types.UI.MerchantDocument
import qualified Control.Lens
import qualified Domain.Action.UI.MerchantDocument
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified MerchantDocuments.Domain.Types.MerchantDocument
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "merchant" :> "document" :> "list" :> QueryParam "language" Kernel.External.Types.Language
      :> MandatoryQueryParam
           "role"
           MerchantDocuments.Domain.Types.MerchantDocument.Role
      :> Get
           '[JSON]
           API.Types.UI.MerchantDocument.MerchantDocumentListRes
      :<|> "merchant"
      :> "document"
      :> Capture
           "documentType"
           Kernel.Prelude.Text
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> QueryParam
           "merchantCity"
           Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "merchantId"
           (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
      :> Header
           "x-forwarded-for"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument
  )

handler :: Environment.FlowServer API
handler = getMerchantDocumentList :<|> getMerchantDocument

getMerchantDocumentList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.External.Types.Language ->
    MerchantDocuments.Domain.Types.MerchantDocument.Role ->
    Environment.FlowHandler API.Types.UI.MerchantDocument.MerchantDocumentListRes
  )
getMerchantDocumentList a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MerchantDocument.getMerchantDocumentList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getMerchantDocument :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument)
getMerchantDocument a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MerchantDocument.getMerchantDocument a5 a4 a3 a2 a1
