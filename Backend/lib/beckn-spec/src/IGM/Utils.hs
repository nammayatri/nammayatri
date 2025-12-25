{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module IGM.Utils where

import BecknV2.Utils as Utils
import Control.Monad.Extra (anyM)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Error as Error
import Kernel.Utils.Error

validateContext :: MonadFlow m => Spec.Action -> Spec.Context -> m ()
validateContext action context = do
  isValidDomain <- anyM (`validateDomain` context) [Spec.ON_DEMAND, Spec.PUBLIC_TRANSPORT]
  unless isValidDomain $
    throwError Error.InvalidDomain
  validateContextCommons action context

validateDomain :: MonadFlow m => Spec.Domain -> Spec.Context -> m Bool
validateDomain expectedDomain context = do
  domainText <- context.contextDomain & fromMaybeM (Error.InvalidRequest "Missing contextDomain")
  domain <- A.decode (A.encode domainText) & fromMaybeM (Error.InvalidRequest $ "Error in parsing contextDomain: " <> domainText)
  pure $ domain == expectedDomain

validateContextCommons :: MonadFlow m => Spec.Action -> Spec.Context -> m ()
validateContextCommons expectedAction context = do
  validateAction expectedAction context
  validateCoreVersion context

validateAction :: MonadFlow m => Spec.Action -> Spec.Context -> m ()
validateAction expectedAction context = do
  actionText <- context.contextAction & fromMaybeM (Error.InvalidRequest "Missing contextAction")
  action <- A.decode (A.encode actionText) & fromMaybeM (Error.InvalidRequest $ "Error in parsing contextAction: " <> actionText)
  unless (action == expectedAction) $
    throwError Error.InvalidAction

validateCoreVersion :: MonadFlow m => Spec.Context -> m ()
validateCoreVersion context = do
  let supportedVersion = "1.0.0"
  version <- context.contextVersion & fromMaybeM (Error.InvalidRequest "Missing contextVersion")
  unless (version == supportedVersion) $
    throwError Error.UnsupportedCoreVer

durationToText :: NominalDiffTime -> Text
durationToText duration = T.pack $ iso8601Show $ calendarTimeTime duration

ack :: Spec.AckResponse
ack =
  Spec.AckResponse
    { ackResponseError = Nothing,
      ackResponseMessage =
        Spec.AckMessage
          { ackMessageAck =
              Spec.Ack
                { ackStatus = Just "200"
                }
          }
    }

computeTtlISO8601 :: Int -> Text
computeTtlISO8601 ttlInSec =
  let ttlToNominalDiffTime = intToNominalDiffTime ttlInSec
   in Utils.formatTimeDifference ttlToNominalDiffTime

mkOrgName :: Text -> Spec.Domain -> Maybe Text
mkOrgName orgName domain = Just $ orgName <> "::" <> show domain
