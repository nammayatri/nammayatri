module Domain.Action.UI.Reels where

import qualified API.Types.UI.Reels
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.ReelsData as CQReels
import qualified Storage.Queries.Person as QPerson
import Tools.Error

getReelsGetAllReelVideos :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Text -> Environment.Flow API.Types.UI.Reels.ReelsResp
getReelsGetAllReelVideos (mbPersonId, _merchantId, merchantOpCityId) mbLanguage reelKey = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  _person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let language = fromMaybe ENGLISH mbLanguage
  reels <- CQReels.findAllByMerchantOpCityIdLanguageAndKey merchantOpCityId language reelKey
  return $
    API.Types.UI.Reels.ReelsResp
      { reels = reels
      }
