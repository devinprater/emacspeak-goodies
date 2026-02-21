# emacspeak-goodies

A small collection of Emacspeak-oriented speech-enablement packages and local fixes.

## License

Because this is all AI-generated bullcrap anyways, this is public
domain. Please jigger this into whatever works for you.

## Contents

### `emacspeak-gptel-agent.el`

Adds an Emacspeak-friendly “agent/executor” workflow on top of
`gptel`, designed for hands-free/low-friction use with speech. What
the fuck did the AI say hands-free for? This shit ain't hands-free.
More like eyes-free but I hate the over-use of that shit in early
Android days. So it basically speech-enables gptel-agent. Damn Codex
don't know shit about blind people.

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
At some point I'll add emacspeak-mastodon and emacspeak-telega.
