# PR review process

## Creating PRs
If the changes are not completely straightforward, please use the channel to discuss what you are planning to do before starting.

Try to make the PRs as small and focused as possible. Try to keep new features and fixes separate from refactorings.

Please add enough information to the PR description so that others can easier understand the changes.

## Review
All developers on the team are encouraged to review PRs (but it is also okay not to if you do not have much to say about a PR or are busy with other tasks). 

Try to focus on the PRs with less than 2 approvals.

In addition to adding any improvement points, feel free to ask questions, for example if you don’t understand how a certain part of code works, or if you have no experience with a language feature, etc.

It is okay to spend some time on looking at a certain PR without necessarily approving it or asking to change something, but please leave a comment if you have anything to say (for example, if the approach looks good to you in general, but you haven’t had the chance to look in detail).

## Approval
Before approving a PR, please make sure to look at it thoroughly. You need to understand what the PR is doing well and check all the changes. You should be reasonably sure that the PR is good to merge and is not going to break anything. Also, please check that the PR leaves the code in at least as good shape as it was before, preferably better.

## Merge
The PR can be merged when it has 2 or more approvals.

Usually try to merge your own PRs as you understand them better and know when they can be merged. Test that the changes work after the rebase. If you are merging someone else’s PR, check that the code builds and the tests are passing.

Before merging, make sure to discuss on the channel if there is any uncertainty (PR introduces a new approach, contains a lot of changes, may break something, etc.).

Please rebase the PR before merging and do not create any merge commits. Avoid the sync button in Bitbucket UI. Check that fast-forward strategy is enabled in Bitbucket UI when you merge the PR.

If no further testing / frontend changes are needed, please transition the Jira issue to Done. Otherwise, transition the issue to UAT and add everyone responsible for the frontend changes to the issue watchers list. Also, add an update on the channel and tag them.

Frontend responsibles: Kavya, Ritika

DB update responsibles: Nikith

## Postman collections

If there is an update to an existing API or adding a new route, use the version control feature of postman (doc): please fork the Dev collection, make your changes and create a pull-request in postman to Dev collection. Also link this postman pull-request in your bitbucket PR so that anyone reviewing can review both and the frontend team can test the changes and update their integration accordingly.
