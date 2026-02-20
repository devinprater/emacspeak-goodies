# emacspeak-goodies

A small collection of Emacspeak-oriented speech-enablement packages and local fixes.

## Contents

### `emacspeak-gptel-agent.el`

Adds an Emacspeak-friendly “agent/executor” workflow on top of `gptel`, designed for hands-free/low-friction use with speech.

High-level usage:

- Load the file (e.g. from your init) and enable the provided integration.
- Start an agent session via the provided interactive commands.
- Give the agent a task; it will execute multi-step plans (reading/editing files, running commands/tests) and speak progress/results.

### `emacspeak-elfeed.el`

An `elfeed` speech-enablement layer based on upstream Emacspeak, with a couple of behavioral adjustments:

- **Advice reorganization:** local refactoring/reshuffling of advice so related `elfeed` entry/navigation behaviors are grouped more coherently.
- **Open entry speaking behavior:** when opening an entry, it speaks the *entire buffer* (`emacspeak-speak-buffer`) instead of only the current line (`emacspeak-speak-line`). This provides immediate context for the opened article/entry.
