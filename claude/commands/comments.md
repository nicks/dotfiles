---
description: Review the comments added by the top commit and amend it
argument-hint: "[extra guidance]"
allowed-tools: Bash(git *), Read, Edit
---
Review every code comment added or changed by the top commit (`git show HEAD`)
and revise the ones that need it. Then amend the commit with your changes.

Judge each comment against one audience: a new user who needs to know the
minimum required to use this API directly. Keep what serves that reader; cut
what doesn't.

- Keep: what the API does, when to reach for it, non-obvious constraints on
  arguments and return values, gotchas that would bite a caller.
- Cut: how the implementation works, why it was built this way, what it used to
  do, narration of the code on the next line, and restatements of the name.
- Rewrite rather than delete when a comment is aimed at the wrong reader but the
  underlying fact is worth knowing.

Leave comments outside the top commit's diff alone. If a comment is already
right, say so and move on — don't churn wording for its own sake.

Report what you changed and what you left, then amend:
`git commit --amend --no-edit` (update the message too if the diff no longer
matches it).

$ARGUMENTS
