# aiemacs

Experimental Emacs configuration for AI-assisted development.

## Structure

```
init.el          # Main config, organized into labeled sections
local.el         # Host-specific settings (theme, font, local LLM)
custom.el        # Emacs customize output (auto-generated)
lisp/            # Custom elisp modules
  ai.el          # AI agent execution (claude, codex, gemini CLIs)
  clipboard.el   # WSL2 clipboard (explicit Windows copy/paste)
  python.el      # Python utilities (uv-activate)
  theme.el       # Theme manipulation (desaturate, invert)
  tmux.el        # Tmux pane integration
  utils.el       # General utilities (PATH setup, timestamps)
```

## Key Bindings

| Prefix | Purpose |
|--------|---------|
| `C-c a` | AI agents (a=region, l=line, c=claude, x=codex, g=gemini) |
| `C-c e` | Edit config (i=init.el, l=local.el) |
| `C-c x/r/b/h/l` | Tmux (send/resend/buffer/region/line) |

## Conventions

- **Tiny commits**: One logical change per commit, clear messages
- **Sections**: Use `;;;;` headers with `====` separators in init.el
- **Modules**: Extract reusable functions to lisp/*.el files
- **No over-engineering**: Keep it simple, avoid premature abstraction
- **local.el**: Only host-specific settings (theme, font, local backends)

## Before Committing

1. Byte-compile check: `emacs -Q --batch -f batch-byte-compile init.el lisp/*.el`
2. Load test: `emacs --init-directory=~/.config/aiemacs --batch -l init.el`
3. No duplicate key bindings
4. No undefined functions or variables
5. Requires match provides in lisp/ modules

## Dependencies

- straight.el (bootstrapped automatically)
- External CLIs: claude, codex, gemini (for ai.el)
- tmux (for tmux.el)
- clip.exe, powershell.exe (for clipboard.el on WSL2)

## API Keys

Stored in `~/.keys` (sourced by shell):
- `DEEPSEEK_API_KEY`
- `ANTHROPIC_API_KEY`
