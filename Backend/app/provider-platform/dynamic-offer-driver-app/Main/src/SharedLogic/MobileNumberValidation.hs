{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.MobileNumberValidation
  ( validateMobileNumber,
  )
where

import qualified Domain.Types.TransporterConfig as DTCConfig
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Tools.Error

validateMobileNumber :: DTCConfig.TransporterConfig -> Text -> Text -> Context.Country -> Flow ()
validateMobileNumber transporterConfig mobileNumber mobileCountryCode country = do
  when (fromMaybe False transporterConfig.enableMobileNumberValidation) $ do
    result <-
      try $
        void $
          runRequestValidation
            ( \() ->
                sequenceA_
                  [ validateField "mobileNumber" mobileNumber $ P.getMobileNumberPredicate country,
                    validateField "mobileCountryCode" mobileCountryCode $ P.getMobileCountryCodePredicate country
                  ]
            )
            ()
    case result of
      Left (_ :: SomeException) -> throwError $ InvalidRequest "Invalid mobile number or country code"
      Right _ -> pure ()
