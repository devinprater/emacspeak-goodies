# emacspeak-goodies

A small collection of Emacspeak-oriented speech-enablement packages and local fixes.

## License

Because this is all AI-generated bullcrap anyways, this is public
domain. Please jigger this into whatever works for you.

## Contents

### `emacspeak-gptel-agent.el`

Adds an Emacspeak-friendly “agent/executor” workflow on top of
`gptel`, designed for hands-free/low-friction use with speech. Rather, eyes-free but the AI can't nuance about disabilities.

High-level usage:

- Load the file (e.g. from your init).
- Start an agent session via the provided interactive commands. gptel-agent.
- Give the agent a task; it will execute multi-step plans
  (reading/editing files, running commands/tests) and speak
  progress/results.
- If it needs to call a tool, it'll tell you. Hit C-c C-c to continue,
  or C-c C-k to kill that call. After it's done everything, it'll
  speak the resulting explanatory text.

### `emacspeak-elfeed.el`

An `elfeed` speech-enablement layer based on upstream Emacspeak, with a couple of behavioral adjustments:

- **Advice reorganization:** local refactoring/reshuffling of advice so related `elfeed` entry/navigation behaviors are grouped more coherently.
- **Open entry speaking behavior:** when opening an entry, it speaks the *entire buffer* (`emacspeak-speak-buffer`) instead of only the current line (`emacspeak-speak-line`). This provides immediate context for the opened article/entry.

### `emacspeak-nov.el`

An `nov` speech-enablement layer with a local fix for EPUB link navigation:

- **Link text on `TAB`:** when moving between links in `nov-mode`, Emacspeak now speaks the visible link label instead of the underlying EPUB target such as `text/ch001.xhtml`.
- **Scope:** this is implemented as NOV-specific advice around `shr-next-link` and `shr-previous-link`, so the fix applies directly to `TAB` and reverse link navigation inside `nov`.

### `emacspeak-mastodon.el`

Speech-enables `mastodon.el` timelines, notifications, and compose flows:

- **Readable status summaries:** speaks cleaned, compact summaries of the current post or notification while navigating timelines.
- **Action feedback:** announces results for common actions such as favourite, boost, and bookmark.
- **Compose support:** speaks when compose buffers open or close, with configurable handling for content warnings, alt text, and summary length.

### `emacspeak-telega.el`

Adds basic Emacspeak support for the `telega` Telegram client:

- **Chat and message navigation:** speaks the current chat or message when moving through buttons and message lists.
- **Cleaner spoken output:** strips emoji-heavy clutter, compresses whitespace, and trims prompt/timestamp noise for shorter summaries.
- **Message context:** includes sender and message text, and announces message time during message-by-message navigation.

