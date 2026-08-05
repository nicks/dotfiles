# Coding preferences

## Comments

Write comments that explain product or architecture context — the "why" behind the
code. Omit comments that merely describe what the implementation does; the code
already says that.

## Simplify

Seek to simplify. Prefer a set of straightforward rules that's easy to understand
if it meets the requirements, even if it changes how edge cases work. Don't add
complexity to preserve behavior that nobody depends on.

## Abstractions

A little duplication is better than the wrong abstraction. Prefer a meaningful
abstraction that expresses real product concepts, even if it requires some
duplicated logic. Don't deduplicate code just because it looks similar.
