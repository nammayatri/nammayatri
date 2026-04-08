module Domain.Action.UI.Loyalty (postWalletLoyaltyInfo) where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Wallet.Interface.Types as WalletTypes
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (fromMaybeM)
import qualified SharedLogic.Payment as SPayment
import qualified Storage.Queries.Person as QPerson
import Tools.Error (PersonError (PersonNotFound))

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
