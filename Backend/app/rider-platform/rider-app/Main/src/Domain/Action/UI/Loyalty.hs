module Domain.Action.UI.Loyalty
  ( postWalletLoyaltyInfo,
    postWalletSvpDeduct,
  )
where

import qualified API.Types.UI.Loyalty as API
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Wallet.Interface.Types as WalletTypes
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (fromMaybeM)
import qualified SharedLogic.Payment as SPayment
import qualified Storage.Queries.Person as QPerson
import Tools.Error (PersonError (PersonNotFound))
import qualified Tools.LoyaltyWallet as TLoyaltyWallet

postWalletLoyaltyInfo ::
  ( ( Maybe (Id Person.Person),
      Id Merchant.Merchant
    ) ->
    Flow WalletTypes.LoyaltyInfoResponse
  )
postWalletLoyaltyInfo (mbPersonId, merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "Missing personId in auth context")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  SPayment.getLoyaltyInfo personId.getId merchantId person.merchantOperatingCityId

postWalletSvpDeduct ::
  ( ( Maybe (Id Person.Person),
      Id Merchant.Merchant
    ) ->
    API.SvpDeductReq ->
    Flow APISuccess
  )
postWalletSvpDeduct (_mbPersonId, _merchantId) req = do
  TLoyaltyWallet.deductSvpFare
    TLoyaltyWallet.SvpDeductionParams
      { customerId = req.customerId,
        amount = req.amount
      }
  pure Success
