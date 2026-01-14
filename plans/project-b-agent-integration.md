# Project B: Rapid Agent Integration

**Status:** Active - Phase 1 Complete

## Goal

Invoke AI agents from within Emacs with minimal friction. Agents generate small patches, shown as interactive diffs for accept/reject. Stay in flow while designing software.

## Core UX

1. Write instruction as comment in code (or select region)
2. Invoke agent (C-c a c for Claude, etc.)
3. Agent runs with file context
4. Proposed changes appear as diff
5. Accept (y) / Reject (n) / Edit
6. Buffer updated, continue working

## Architecture

```
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│ ai.el       │────▶│ agent-shell  │────▶│ Agent CLI   │
│ (invoke)    │     │ (ACP layer)  │     │ (claude,etc)│
└─────────────┘     └──────────────┘     └─────────────┘
                           │
                           ▼
                    ┌──────────────┐
                    │ Diff viewer  │
                    │ (accept/rej) │
                    └──────────────┘
```

## Implementation Steps

### Phase 1: Basic Integration ✓
- [x] Rewrite ai.el to use agent-shell instead of raw vterm
- [x] Pass file path + cursor position + instruction to agent
- [x] Receive patches through ACP, show via agent-shell-diff
- [x] Accept/reject flow updates buffer

**Implementation notes:**
- `ai--get-config` maps backend symbols to agent-shell configs
- `ai--ensure-shell` reuses existing shell or creates new one
- `agent-shell-insert :submit t` sends prompt and handles response
- agent-shell handles diffs/permissions automatically via ACP

### Phase 2: Polish
- [ ] Auto-revert buffer after accepted changes
- [ ] Handle tramp paths correctly (pass full path, not just filename)
- [ ] Multiple file changes in single session
- [ ] Persist agent session for follow-up prompts

### Phase 3: Workflow Optimization
- [ ] Quick re-run last instruction
- [ ] Instruction history per file
- [ ] Project-wide context (not just single file)

## Key Bindings

| Key | Function |
|-----|----------|
| `C-c a a` | Execute region with backend prompt |
| `C-c a l` | Execute current line with backend prompt |
| `C-c a c` | Execute region with Claude |
| `C-c a g` | Execute region with Gemini |
| `C-c a x` | Execute region with Codex |
| `C-c a RET` | Quick: execute line with Claude |

## Supported Backends

- `claude` - Claude Code via claude-code-acp
- `gemini` - Gemini CLI
- `codex` - OpenAI Codex
- `opencode` - OpenCode

## Files

- `lisp/ai.el` - Main entry points, keybindings (~100 lines)
- Depends on: agent-shell, acp

## Open Questions

- How to handle agent "thinking" output? (agent-shell shows in buffer)
- Should instruction comment be deleted after successful apply?
- How to handle agent errors gracefully?
- Full file path vs filename in prompt?

---

*Started: 2025-01-14*
*Phase 1 Complete: 2025-01-14*
