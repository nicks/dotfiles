---
description: Read the Linear issue named by the current branch
argument-hint: "[issue id or branch name]"
allowed-tools: Bash(git *), Bash(jj *), mcp__linear__get_issue, mcp__linear__list_comments
---
Find the Linear issue this branch is for and summarize it.

Resolve the issue ID in this order, stopping at the first that works:

1. `$ARGUMENTS`, if given — either a bare issue ID (`FLA-123`) or a branch name
   to pull one out of.
2. The current git branch: `git branch --show-current`.
3. The current jj bookmark, if git reports nothing or a detached HEAD:
   `jj log -r @ --no-graph -T 'bookmarks'`.

An issue ID is `[A-Za-z]{2,7}-[0-9]+` anywhere in the name — Linear's generated
branches look like `nick/fla-123-fix-the-thing`, so match case-insensitively and
uppercase it before querying. If the name has no ID in it, say so and stop;
don't guess at the issue from the branch's words.

Fetch the issue with `get_issue` and its discussion with `list_comments`, then
report:

- Title, identifier, state, assignee, and URL.
- What the issue actually asks for, in your own words — not a transcript of the
  description. Pull out acceptance criteria and constraints if they're there.
- Anything in the comments that changes the task: scope cuts, decisions,
  blockers, links to other issues. Skip status chatter.
- Open questions the issue leaves unanswered, if any.

Don't start implementing. This command is for reading in.
