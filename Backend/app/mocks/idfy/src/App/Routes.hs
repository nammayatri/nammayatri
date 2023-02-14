 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Routes where

import App.Types
import EulerHS.Prelude
import Idfy.External.Flow
import Kernel.Types.App (FlowServerR)
import qualified Product.Idfy as P
import Servant hiding (throwError)

type MockIdfyAPI =
  VerifyDLAPI
    :<|> VerifyRCAPI
    :<|> ValidateImage
    :<|> ExtractDLImage
    :<|> ExtractRCAPI

mockIdfyAPI :: Proxy MockIdfyAPI
mockIdfyAPI = Proxy

mockIdfyServer :: FlowServerR AppEnv MockIdfyAPI
mockIdfyServer =
  P.verifyDL
    :<|> P.verifyRC
    :<|> P.validateImage
    :<|> P.extractDLImage
    :<|> P.extractRCImage
