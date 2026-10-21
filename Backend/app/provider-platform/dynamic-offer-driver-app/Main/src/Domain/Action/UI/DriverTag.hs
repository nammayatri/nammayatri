-- | Generic, driver-initiated writes to 'person.driverTag'.
--
-- Tags written here never expire; anything that needs a TTL (AvailableForRides, area
-- preference) keeps its own purpose-built endpoint. ADD_TAG replaces any existing tag with
-- the same name, so a tag holds at most one value per driver.
module Domain.Action.UI.DriverTag (postDriverTagUpdate) where

import qualified API.Types.UI.DriverTag as APIT
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.Queries.Person as QPerson
import Tools.Error

postDriverTagUpdate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    APIT.DriverTagUpdateReq ->
    Environment.Flow APIT.DriverTagRes
  )
postDriverTagUpdate (mbPersonId, _, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  tagName <- validateTagPart "tagName" req.tagName
  mbTagValue <- traverse (validateTagPart "tagValue") req.tagValue
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let updatedTags = case req.action of
        APIT.ADD_TAG ->
          Yudhishthira.replaceTagNameValue person.driverTag $
            maybe
              (LYT.TagNameValueExpiry tagName)
              (\tagValue -> Yudhishthira.mkTagNameValueExpiryAt (LYT.TagName tagName) (LYT.TextValue tagValue) Nothing)
              mbTagValue
        APIT.REMOVE_TAG ->
          case mbTagValue of
            Nothing -> Yudhishthira.removeTagName person.driverTag (LYT.TagNameValue tagName)
            Just tagValue -> Yudhishthira.removeTagNameValue person.driverTag (LYT.TagNameValue $ tagName <> "#" <> tagValue)
  QPerson.updateDriverTag (Just updatedTags) personId
  pure APIT.DriverTagRes {driverTags = updatedTags}

-- | Tag parts are stored joined by "#" (and multi-values by "&"), so neither may appear inside.
validateTagPart :: (MonadThrow m, Log m) => Text -> Text -> m Text
validateTagPart field raw = do
  let value = T.strip raw
  when (T.null value) $ throwError (InvalidRequest $ field <> " must not be empty")
  when (T.any (\c -> c == '#' || c == '&') value) $ throwError (InvalidRequest $ field <> " must not contain '#' or '&'")
  pure value
