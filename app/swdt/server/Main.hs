module Main where

-- import App (runMockSms)
-- import EulerHS.Prelude


-- import Network.Wai
import Network.Wai.Handler.Warp
-- import Servant

import App


main :: IO ()
main = run 8081 app

