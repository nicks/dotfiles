# Coding preferences

## Comments

Write comments that explain product or architecture context — the "why" behind the
code. Omit comments that describe what the implementation does.

## Simplify

Seek to simplify. Prefer a set of straightforward rules that's easy to understand
if it meets the requirements, even if it changes how edge cases work. Don't add
complexity to preserve behavior that nobody depends on.

## Abstractions

A little duplication is better than the wrong abstraction. Prefer a meaningful
abstraction that expresses real product concepts, even if it requires some
duplicated logic. Don't deduplicate code just because it looks similar.
+
 ## Concurrency
 
 Use promises to encapsulate the state of an asynchronous task.
 
 Bad:
 
 ```
 // Don't use mutable fields to track progress
 this.#inProgress = true;
 setTimeout(() => {
   this.#result = doWork();
   this.#inProgress = false;
 }, 0);
 ```
 
 Good:
 
 ```
 this.#inProgress = new Promise((resolve, reject) => {
   resolve(doWork());
   this.#inProgress = null;
 });
 ```
 
 When you need to consume asynchonous state, 
 there are two patterns: broadcasting and state reconciliation.
 
 Use broadcasting when you must take some action whenever the state
 changes. Use reconciliation when you only care about the current state
 when your handler runs.
 
 ```
 // Reconcilation
 setState(newState) {
   let stateChanged = this.#stateChanged;
   this.#state = newState;
   this.#onStateChanged = new Promise((resolve) => {
     this.#stateChanged = resolve;
   });
   if (stateChanged) stateChanged();
 }
 
 waitOnStateChanged() {
   // don't assume the state has only changed
   // once when we wake up, re-read the current state
   await this.#onStateChanged;
   return this.#state;
 }
 ```
