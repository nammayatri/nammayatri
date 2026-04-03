{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.RiderPlatform.IssueManagement where
import EulerHS.Prelude
import Data.OpenApi (ToSchema)
import qualified API.Types.RiderPlatform.IssueManagement.Issue
import qualified API.Types.RiderPlatform.IssueManagement.IssueList
import qualified Text.Show
import qualified Text.Read
import qualified Data.List
import qualified Data.Singletons.TH



data IssueManagementUserActionType
    = ISSUE API.Types.RiderPlatform.IssueManagement.Issue.IssueUserActionType | ISSUE_LIST API.Types.RiderPlatform.IssueManagement.IssueList.IssueListUserActionType
    deriving stock (Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Text.Show.Show IssueManagementUserActionType
    where show = \case
                     ISSUE e -> "ISSUE/" <> show e
                     ISSUE_LIST e -> "ISSUE_LIST/" <> show e
instance Text.Read.Read IssueManagementUserActionType
    where readsPrec d' = Text.Read.readParen (d' > app_prec) (\r -> [(ISSUE v1, r2) | r1 <- stripPrefix "ISSUE/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1] ++ [(ISSUE_LIST v1,
                                                                                                                                                                            r2) | r1 <- stripPrefix "ISSUE_LIST/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1])
                        where app_prec = 10
                              stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [(''IssueManagementUserActionType)])

