{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | Tests for the placeholder filling of the dashboard welcome email.
--
-- The template lives in @merchant_message@ and is filled in by 'renderTemplate'. The password is
-- user-chosen text, so it must be substituted last: otherwise a password that looks like a
-- placeholder would be expanded by a later replacement.
module WelcomeEmailTemplateTests (tests) where

import qualified Data.Text as T
import "dynamic-offer-driver-app" Domain.Action.Internal.SendWelcomeEmail
  ( SendWelcomeEmailReq (..),
    renderTemplate,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Prelude

mkReq :: T.Text -> SendWelcomeEmailReq
mkReq pwd = SendWelcomeEmailReq {email = "jane@example.com", roleName = "FLEET_OWNER", password = pwd}

template :: T.Text
template =
  "Hi! Welcome to Control Centre\n\nYour role is {#role#}\n\n"
    <> "Your user email Login is: {#email#}\nYour password is: {#password#}\n\n"
    <> "In case of any queries, please email us at {#supportEmail#}"

tests :: TestTree
tests =
  testGroup
    "Dashboard welcome email template"
    [ testCase "fills all four placeholders" $
        renderTemplate (mkReq "Test@1234") "support@bridge.cab" template
          @?= "Hi! Welcome to Control Centre\n\nYour role is FLEET_OWNER\n\n"
            <> "Your user email Login is: jane@example.com\nYour password is: Test@1234\n\n"
            <> "In case of any queries, please email us at support@bridge.cab",
      testCase "leaves no placeholder behind" $
        assertBool "placeholder left" $
          not ("{#" `T.isInfixOf` renderTemplate (mkReq "Test@1234") "support@bridge.cab" template),
      testCase "replaces repeated placeholders" $
        renderTemplate (mkReq "pw") "s@x.com" "{#email#} / {#email#}" @?= "jane@example.com / jane@example.com",
      testCase "password that looks like a placeholder is not expanded" $
        renderTemplate (mkReq "{#supportEmail#}") "s@x.com" "pw={#password#}" @?= "pw={#supportEmail#}",
      testCase "template without placeholders is unchanged" $
        renderTemplate (mkReq "pw") "s@x.com" "Hello" @?= "Hello"
    ]
