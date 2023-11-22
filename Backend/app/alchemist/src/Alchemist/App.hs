{-# LANGUAGE QuasiQuotes #-}

module Alchemist.App where

import Alchemist.DSL.Parser.API (apiParser)
import Alchemist.Generator.Haskell.Servant (generateServantCode)
import Alchemist.Utils
import Kernel.Prelude
import Text.Parsec
import Text.RawString.QQ

processAPIDSL :: String -> String
processAPIDSL dsl = case parse apiParser "dsl-input" dsl of
  Left err -> "Parsing error: " ++ show err
  Right ast -> generateServantCode ast

dslInput :: String
dslInput =
  [r|POST /org/driver/services?*status:BookingStatus&limit:Int&offset:Int
  AUTH TokenAuth
  H mId Merchant
  REQ TicketBody
  RESP TicketServiceVerificationResp
|]

generateAPI :: IO ()
generateAPI = do
  let generatedCode = processAPIDSL dslInput
  writeToFile "GeneratedServantAPI.hs" generatedCode
