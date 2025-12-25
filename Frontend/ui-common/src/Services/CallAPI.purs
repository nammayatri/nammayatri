module Services.CallAPI where

import Prelude
import Presto.Core.Types.API (Header(..), Headers(..), Request(..), RestAPIOptions(..)) as API
import Presto.Core.Types.API
import Presto.Core.Types.Language.Flow (Flow, doAff, defaultState, getState, modifyState)
import Data.Either (Either(..), either, hush)
import Foreign.Generic (class Decode, ForeignError, decode, decodeJSON, encode)
import Data.Maybe
import Data.Array
import Effect.Uncurried
import Data.Function.Uncurried
import Data.Int (fromString)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (lift)
import Foreign.Object (empty, insert, lookup, Object, foldM, delete)
import Log (printLog)
import JSURI (decodeURIComponent)
import Control.Monad.Except (runExcept)
import Data.Array (length, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (modify)
import Foreign.Class (class Decode, decode, encode)
import Foreign.Generic (decodeJSON)
import Foreign.Object (empty)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Headers, Request(..), Response, URL, makeRequest)
import Presto.Core.Types.Language.Interaction (Interaction, request)
import Data.Function.Uncurried as EFn
import Effect.Aff (Aff, makeAff, nonCanceler, attempt, launchAff)
import Control.Monad.Free (Free, liftF)


foreign import addBenchMark :: Fn2 String Boolean Unit

foreign import atobImpl :: String -> String

foreign import callbackMapper :: forall a. a -> String

foreign import callApi :: EffectFn7 String String String String Boolean Boolean String Unit
foreign import callAPIWithOptions :: EffectFn8 String String String String Boolean Boolean String String Unit

callAPI :: forall a b st.
  StandardEncode a => Decode b => RestEndpoint a
  => Headers -> a -> Flow st (Either ErrorResponse (Response b))
callAPI headers a = do
  let (Request request) = makeRequest a headers
  let _ = EFn.runFn2 addBenchMark request.url false
  resp <- doAff $ makeAff
    ( \cb -> do
        let _ = runFn2 addBenchMark ("apiRunner_" <> request.url) false
        void $ pure $ printLog "callAPI request" request   
        case request.options of
          Nothing -> do
            _ <- runEffectFn7 callApi (show request.method) request.url request.payload (standardEncodeJSON headerss) shouldFormEncode isSSLPinnedURL $ callback cb
            pure $ nonCanceler
          Just (API.RestAPIOptions ops) -> do
            _ <- runEffectFn8 callAPIWithOptions (show request.method) request.url request.payload (standardEncodeJSON headerss) shouldFormEncode isSSLPinnedURL (standardEncodeJSON ops) $ callback cb
            pure $ nonCanceler
    )
  let _ = EFn.runFn2 addBenchMark request.url true
  let _ = EFn.runFn2 addBenchMark ("decode_" <> request.url) false
  case runExcept $ decodeJSON resp.response of
    Right (response :: b) -> do
          -- let _ = logSuccessUrl $ Just request.url
          let _ = EFn.runFn2 addBenchMark ("decode_" <> request.url) true
          pure $ Right { code : resp.code
                              , responseHeaders : resp.responseHeaders
                              , response : response
                              , status : resp.status
                              }
    Left _ -> do
        case runExcept $ decode (encode resp.response) of
          Right (response :: b) -> do
                    let _ = EFn.runFn2 addBenchMark ("decode_" <> request.url) true
                    -- let _ = logSuccessUrl $ Just request.url
                    pure $ Right { code : resp.code
                                    , responseHeaders : resp.responseHeaders
                                    , response : response
                                    , status : resp.status
                                    }
          Left er -> do
            let _ = EFn.runFn2 addBenchMark ("decode_" <> request.url) true
            let errorResp = { code : resp.code
                            , responseHeaders : resp.responseHeaders
                            , response : { error: true
                                        , errorMessage: resp.response
                                        , userMessage: show er <> "\n" <> resp.response
                                        }
                            , status : resp.status
                            }
            pure $ Left errorResp
            -- retryWithFallbackUrl request.fallbackUrls errorResp
                              

    where
      -- logSuccessUrl :: Maybe String -> Unit
      -- logSuccessUrl url
      --   | retryCount > 0 = _trackNetworkRetrySuccess {success_url : encode url }
      --   | otherwise      = unit 
      -- retryWithFallbackUrl :: forall st. Array URL -> ErrorResponse -> Flow st (Either ErrorResponse (Response b))
      -- retryWithFallbackUrl fallbackUrls currErrorResponse
      --     | retryCount >= length fallbackUrls = pure $ Left currErrorResponse
      --     | otherwise =  apiCall a headers
      callback cb = callbackMapper $
          mkEffectFn6 $
            \status response statusCode url responseHeaders urlEncodedResponse -> do
              let _ = runFn2 addBenchMark ("apiRunner_" <> atobImpl url) true
              let formattedResponse = { status : status,
                response : extractResponse response urlEncodedResponse,
                code : fromMaybe (-3) (fromString statusCode),
                responseHeaders : fromMaybe empty $ hush $ runExcept $ decodeJSON $ atobImpl responseHeaders
              }
              void $ pure $ printLog "callAPI response" formattedResponse
              void $ pure $ printLog "response" formattedResponse.response
              cb $ Right formattedResponse
      headerss = do
        let (Headers hs) = headers
        foldl (\acc (API.Header key value) -> insert key value acc) empty hs
      isSSLPinnedURL = lookup "x-pinned" headerss == Just "true"
      shouldFormEncode = lookup "Content-Type" headerss == Just "application/x-www-form-urlencoded"

      extractResponse response urlEncodedResponse = do
        case hush $ runExcept $ decode urlEncodedResponse of
          Just resp -> fromMaybe (atobImpl response) $ decodeURIComponent resp
          _ -> atobImpl response
              