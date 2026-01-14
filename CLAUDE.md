# aiemacs

Experimental Emacs configuration for AI-assisted development.

## Structure

```
init.el          # Main config, organized into labeled sections
local.el         # Host-specific settings (theme, font, local LLM)
custom.el        # Emacs customize output (auto-generated)
lisp/            # Custom elisp modules
  ai.el          # AI agent invocation via agent-shell
  clipboard.el   # WSL2 clipboard (explicit Windows copy/paste)
  python.el      # Python utilities (uv-activate)
  theme.el       # Theme manipulation (desaturate, invert)
  tmux.el        # Tmux pane integration
  utils.el       # General utilities (PATH setup, timestamps)
plans/           # Project plans (versioned)
  project-a-mcp-server.md    # Future: MCP server in Emacs
  project-b-agent-integration.md  # Active: agent-shell integration
```

## AI Integration Architecture

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

**Workflow:**
1. Select instruction text (comment or region)
2. `C-c a c` (or other backend key)
3. agent-shell sends to agent with file context
4. Agent proposes changes → shown as diff
5. Accept (y) / Reject (n)
6. Buffer auto-reverts with changes

## Key Bindings

| Prefix | Purpose |
|--------|---------|
| `C-c a` | AI agents |
| `C-c a a` | Execute region (prompt for backend) |
| `C-c a l` | Execute line (prompt for backend) |
| `C-c a c` | Execute with Claude |
| `C-c a g` | Execute with Gemini |
| `C-c a x` | Execute with Codex |
| `C-c a RET` | Quick: execute line with Claude |
| `C-c e` | Edit config (i=init.el, l=local.el) |
| `C-c x/r/b/h/l` | Tmux (send/resend/buffer/region/line) |

## Conventions

- **Tiny commits**: One logical change per commit, clear messages
- **Sections**: Use `;;;;` headers with `====` separators in init.el
- **Modules**: Extract reusable functions to lisp/*.el files
- **No over-engineering**: Keep it simple, avoid premature abstraction
- **local.el**: Only host-specific settings (theme, font, local backends)
- **Plans**: Document project plans in plans/*.md

## Before Committing

1. Byte-compile check: `emacs -Q --batch -f batch-byte-compile init.el lisp/*.el`
2. Load test: `emacs --init-directory=~/.config/aiemacs --batch -l init.el`
3. No duplicate key bindings
4. No undefined functions or variables
5. Requires match provides in lisp/ modules
6. Keep CLAUDE.md and plans/ updated

## Tramp (Remote Files)

Tramp is heavily used. Key considerations:
- VC is disabled for remote files (performance)
- Project detection uses transient mode for remote dirs
- SSH ControlMaster reuses connections
- AI agents work on remote files - full tramp path passed to agent

When editing remote files, test that:
- File opens without delay
- Saving works correctly
- No VC or project scanning freezes

## Dependencies

**Emacs packages:**
- straight.el (bootstrapped automatically)
- agent-shell (ACP communication with agents)
- acp.el (Agent Client Protocol)
- shell-maker (comint-based shell)

**External CLIs:**
- claude-code-acp (for Claude backend)
- gemini (for Gemini backend)
- codex (for Codex backend)
- tmux (for tmux.el)
- clip.exe, powershell.exe (for clipboard.el on WSL2)
- SSH with ControlMaster support (for tramp)

## API Keys

Stored in `~/.keys` (sourced by shell):
- `DEEPSEEK_API_KEY`
- `ANTHROPIC_API_KEY`
