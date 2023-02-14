# Contributing to Namma Yatri

## Code of Conduct

The nammayatri project adheres to the [Code of Conduct][coc].
This describes the _minimum_ behavior expected from all contributors.

[coc]: https://haskell.foundation/guidelines-for-respectful-communication/

## Contributing in Issues

Please use Github issues only for reporting bugs and detailed feature requests. Everything else can be part of discussions.

For any issue, there are fundamentally three ways an individual can contribute:

1. By opening the issue for discussion: For instance, if you believe that you
   have discovered a bug in nammayatri, creating a new issue in [the
   nammayatri/nammayatri issue tracker][issue] is the way to report it.

2. By helping to triage the issue: This can be done by providing supporting
   details (a test case that demonstrates a bug), providing suggestions on how
   to address the issue, or ensuring that the issue is tagged correctly.

3. By helping to resolve the issue: Typically this is done either in the form of
   demonstrating that the issue reported is not a problem after all, or more
   often, by opening a Pull Request that changes some bit of something in
   nammayatri in a concrete and reviewable manner.

[issue]: https://github.com/nammayatri/nammayatri/issues


### Asking for General Help

If you have reviewed existing documentation and still have questions or are
having problems, you can [open a discussion] asking for help.

In exchange for receiving help, we ask that you contribute back a documentation
PR that helps others avoid the problems that you encountered.

[open a discussion]: https://github.com/nammayatri/nammayatri/discussions/new

### Submitting a Bug Report

When opening a new issue in the nammayatri issue tracker, you will be presented
with a basic template that should be filled in.
If you believe that you have uncovered a bug, please fill out this form,
following the template to the best of your ability.
Do not worry if you cannot answer every detail, just fill in what you can.

The two most important pieces of information we need in order to properly
evaluate the report is a description of the behavior you are seeing and a simple
test case we can use to recreate the problem on our own.
If we cannot recreate the issue, it becomes impossible for us to fix.

See [How to create a Minimal, Complete, and Verifiable example][mcve].

[mcve]: https://stackoverflow.com/help/mcve

### Triaging a Bug Report

Once an issue has been opened, it is not uncommon for there to be discussion
around it.
Some contributors may have differing opinions about the issue, including whether
the behavior being seen is a bug or a feature.
This discussion is part of the process and should be kept focused, helpful, and
professional.

Short, clipped responses — that provide neither additional context nor
supporting detail — are not helpful or professional.
To many, such responses are simply annoying and unfriendly.

Contributors are encouraged to help one another make forward progress as much as
possible, empowering one another to solve issues collaboratively.
If you choose to comment on an issue that you feel either is not a problem that
needs to be fixed, or if you encounter information in an issue that you feel is
incorrect, explain why you feel that way with additional supporting context, and
be willing to be convinced that you may be wrong.
By doing so, we can often reach the correct outcome much faster.

### Resolving a Bug Report

In the majority of cases, issues are resolved by opening a Pull Request.
The process for opening and reviewing a Pull Request is similar to that of
opening and triaging issues, but carries with it a necessary review and approval
workflow that ensures that the proposed changes meet the minimal quality and
functional guidelines of the nammayatri project.

### Commit message convention
Try to create commits with smaller changes that are correct and compile. This
helps to keep clean commit history and promotes readability. There is no limit
to the number of commits any single Pull Request may have, and
many contributors find it easier to review changes that are split across multiple commits.

That said, if you have a number of commits that are "checkpoints" and don't
represent a single logical change, please squash those together.

Note that multiple commits often get squashed when they are landed (see the
notes about [commit squashing](#commit-squashing)).

Each commit message consists of following parts, separated by `/`:

```text
<sub-project>/<type>: <issue-number> <short summary>
```

Ex: `backend/feat: #341 Driver onboarding flow`, `frontend/fix: #322 Font size in ride request popup`

`<sub-proj>` must refer to one of the many sub projects in this monorepo, `backend` or `frontend`

`<type>` must be one of the following:

- `chore`: Changes such as fixing formatting or addressing warnings or lints, or
  other maintenance changes
- `ci`: Changes to our CI configuration files and scripts (examples: `workflows`,
  `github actions`)
- `docs`: Documentation only changes
- `feat`: A new feature
- `fix`: A bug fix
- `perf`: A code change that improves performance
- `refactor`: A code change that neither fixes a bug nor adds a feature
- `test`: Adding missing tests or correcting existing tests

`<issue-number>` Github issue number to automatically link the commit with the issue

`<short-summary>` try to provide descriptive message here. Its also easier to write smaller
message if the commits are smaller.

### Branch naming convention
Branch naming format consists of three or four parts, separated by `/`:
```text
<sub-project>/<type>/<issue-number><very-short-description>
```
1. `<sub-project>` is described above
2. `<type>` is as described above
3. Task code, e.g. `GH-123`
4. Human readable description in kebab case

1 is required since we have different code owners per sub-project. This helps
them to easily track that are in progress.
3 or 4 may be absent, but not both at the same time. Examples: `fix/GH-123/some-thing`, 
`refactor/GH-123`, `feature/some-feature`.

## Opening the Pull Request

From within GitHub, opening a new Pull Request will present you with a
[template] that should be filled out. Please try to do your best at filling out
the details, but feel free to skip parts if you're not sure what to put.

[template]: /.github/PULL_REQUEST_TEMPLATE.md

### Discuss and update

You will probably get feedback or requests for changes to your Pull Request.
This is a big part of the submission process so don't be discouraged!
Some contributors may sign off on the Pull Request right away, others may have
more detailed comments or feedback.
This is a necessary part of the process in order to evaluate whether the changes
are correct and necessary.

**Any community member can review a PR and you might get conflicting feedback**.
Keep an eye out for comments from code owners to provide guidance on conflicting
feedback.

**Once the PR is open, do not rebase the commits**.
See [Commit Squashing](#commit-squashing) for more details.

### Commit Squashing

In most cases, **do not squash commits that you add to your Pull Request during
the review process**.
When the commits in your Pull Request land, they may be squashed into one commit
per logical change.
Metadata will be added to the commit message (including links to the Pull
Request, links to relevant issues, and the names of the reviewers).
The commit history of your Pull Request, however, will stay intact on the Pull
Request page.

## Reviewing Pull Requests

**Any nammayatri community member is welcome to review any pull request**.

All nammayatri contributors who choose to review and provide feedback on Pull
Requests have a responsibility to both the project and the individual making the
contribution.
Reviews and feedback must be helpful, insightful, and geared towards improving
the contribution as opposed to simply blocking it.
If there are reasons why you feel the PR should not land, explain what those are.
Do not expect to be able to block a Pull Request from advancing simply because
you say "No" without giving an explanation.
Be open to having your mind changed.
Be open to working with the contributor to make the Pull Request better.

Reviews that are dismissive or disrespectful of the contributor or any other
reviewers are strictly counter to the Code of Conduct.

When reviewing a Pull Request, the primary goals are for the codebase to improve
and for the person submitting the request to succeed.
**Even if a Pull Request does not land, the submitters should come away from the
experience feeling like their effort was not wasted or unappreciated**.
Every Pull Request from a new contributor is an opportunity to grow the
community.

### Review a bit at a time

Do not overwhelm new contributors.

It is tempting to micro-optimize and make everything about relative performance,
perfect grammar, or exact style matches.
Do not succumb to that temptation.

Focus first on the most significant aspects of the change:

1. Does this change make sense for nammayatri?
   Does this change cover all points as mentioned in the Github issue?
2. Does this change make nammayatri better, even if only incrementally?
3. Are there clear bugs or larger scale issues that need attending to?
4. Is the commit message readable and correct?
   If it contains a breaking change is it clear enough?

Note that only **incremental** improvement is needed to land a PR.
This means that the PR does not need to be perfect, only better than the status
quo.
Follow up PRs may be opened to continue iterating.

When changes are necessary, _request_ them, do not _demand_ them, and **do not
assume that the submitter already knows how to add a test or run a benchmark**.

Specific performance optimization techniques, coding styles and conventions
change over time.
The first impression you give to a new contributor never does.

Nits (requests for small changes that are not essential) are fine, but try to
avoid stalling the Pull Request.
Most nits can typically be fixed by the nammayatri collaborator landing the
Pull Request but they can also be an opportunity for the contributor to learn a
bit more about the project.

It is always good to clearly indicate nits when you comment: e.g.
`Nit: change foo() to bar(). But this is not blocking.`

If your comments were addressed but were not folded automatically after new
commits or if they proved to be mistaken, please, [hide them][hiding-a-comment]
with the appropriate reason to keep the conversation flow concise and relevant.

### Be aware of the person behind the code

Be aware that _how_ you communicate requests and reviews in your feedback can
have a significant impact on the success of the Pull Request.
Yes, we may land a particular change that makes nammayatri better, but the
individual might just not want to have anything to do with nammayatri ever
again.
The goal is not just having good code.

### Abandoned or Stalled Pull Requests

If a Pull Request appears to be abandoned or stalled, it is polite to first
check with the contributor to see if they intend to continue the work before
checking if they would mind if you took it over (especially if it just has nits
left).
When doing so, it is courteous to give the original contributor credit for the
work they started (either by preserving their name and email address in the
commit log, or by using an `Author:` meta-data tag in the commit.

_Adapted from the [Node.js contributing guide][node]_.

[node]: https://github.com/nodejs/node/blob/master/CONTRIBUTING.md
[hiding-a-comment]: https://help.github.com/articles/managing-disruptive-comments/#hiding-a-comment

<br>
<hr>

_Many of the convention/guidelines are adapted from [juspay/hyperswitch][juspay/hyperswitch]_.

[juspay/hyperswitch]: https://github.com/juspay/hyperswitch/blob/main/docs/CONTRIBUTING.md