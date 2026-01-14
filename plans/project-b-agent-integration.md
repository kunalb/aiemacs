# Project B: Rapid Agent Integration

**Status:** Active

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

### Phase 1: Basic Integration
- [ ] Rewrite ai.el to use agent-shell instead of raw vterm
- [ ] Pass file path + cursor position + instruction to agent
- [ ] Receive patches through ACP, show via agent-shell-diff
- [ ] Accept/reject flow updates buffer

### Phase 2: Polish
- [ ] Auto-revert buffer after accepted changes
- [ ] Handle tramp paths correctly
- [ ] Multiple file changes in single session
- [ ] Persist agent session for follow-up prompts

### Phase 3: Workflow Optimization
- [ ] Quick re-run last instruction
- [ ] Instruction history per file
- [ ] Project-wide context (not just single file)

## Key agent-shell Primitives to Use

- `agent-shell-diff` - Interactive diff display with keybindings
- `acp-make-client` - Create agent connection
- `acp-send-request` - Send prompts to agent
- ACP file capabilities - Let agent read/write through Emacs

## Design Decisions

1. **Use agent-shell's ACP layer** - Don't reinvent the agent communication
2. **Keep ai.el thin** - Just orchestration, delegate to agent-shell
3. **Single file focus first** - Multi-file can come later
4. **Reuse diff UX** - agent-shell-diff already handles accept/reject well

## Files

- `lisp/ai.el` - Main entry points, keybindings
- `lisp/ai-agent.el` - Agent session management (new)
- Depends on: agent-shell, acp

## Open Questions

- How to handle agent "thinking" output? Show in minibuffer or dedicated area?
- Should instruction comment be deleted after successful apply?
- How to handle agent errors gracefully?

---

*Started: 2025-01-14*
