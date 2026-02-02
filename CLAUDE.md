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
  uv.el          # uv virtual environment activation
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
1. Position cursor or select region for context
2. `C-c a a` (or `C-c a c` for Claude, etc.)
3. Type instruction in minibuffer (or `C-c a b` for multi-line)
4. agent-shell sends to agent with file + context
5. Agent proposes changes → shown as diff
6. Accept (y) / Reject (n)

## Key Bindings

| Prefix | Purpose |
|--------|---------|
| `C-c a` | AI agents |
| `C-c a a` | Prompt for instruction (uses context) |
| `C-c a b` | Multi-line instruction buffer |
| `C-c a r` | Prompt with region as context |
| `C-c a .` | Repeat last instruction |
| `C-c a c` | Prompt + Claude |
| `C-c a g` | Prompt + Gemini |
| `C-c a x` | Prompt + Codex |
| `C-c e` | Config (i=edit init, l=edit local, r=reload all, R=reload init) |
| `C-c j` | Jupyter (python-mode) |
| `C-c j j` | Start Jupyter REPL |
| `C-c j J` | Start REPL + autoreload |
| `C-c j c` | Eval line or region |
| `C-c j b` | Eval buffer |
| `C-c j f` | Eval defun |
| `C-c j a` | Enable autoreload |
| `C-c j R` | Restart kernel |
| `C-c x/r/b/h/l` | Tmux (send/resend/buffer/region/line) |

## Conventions

- **Tiny commits**: One logical change per commit, clear messages
- **Sections**: Use `;;;;` headers with `====` separators in init.el
- **Modules**: Extract reusable functions to lisp/*.el files
- **No over-engineering**: Keep it simple, avoid premature abstraction
- **local.el**: Only host-specific settings (theme, font, local backends)
- **Plans**: Document project plans in plans/*.md

## Before Committing

Run `make check` to verify:
- No lisp/*.el files shadow built-in Emacs packages
- Each file's `(provide 'X)` matches its filename
- No duplicate global keybindings

Additional checks:
- `make byte-compile` - Compile all elisp files
- Load test: `emacs --init-directory=~/.config/aiemacs --batch -l init.el`
- Keep CLAUDE.md and plans/ updated

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
