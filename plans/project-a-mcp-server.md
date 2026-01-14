# Project A: MCP Server in Emacs

**Status:** Future

## Goal

Run an MCP (Model Context Protocol) server within Emacs that exposes buffers, files, and editing capabilities to external agents.

## Why

- Any MCP-compatible agent can connect to Emacs
- Agents get structured access to:
  - Read buffers/files
  - Propose patches (shown as diffs)
  - Execute elisp (with permission)
  - Access project context
- Decouples agent choice from editor integration

## Open Questions

- MCP server implementation in elisp vs subprocess?
- Transport: stdio, HTTP, WebSocket?
- Security model for remote agents?
- How to handle multiple concurrent agents?

## Dependencies

- Project B working first (proves the UX for interactive patches)
- Understanding of MCP protocol spec

## Rough Scope

1. MCP protocol implementation (JSON-RPC over stdio/HTTP)
2. Tool definitions: read_file, write_file, apply_patch, list_buffers
3. Permission UI reusing agent-shell-diff patterns
4. Connection management

---

*Parked until Project B is complete and validated.*
