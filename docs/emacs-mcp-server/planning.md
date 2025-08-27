# Emacs MCP Server – Planning

## Phase 1: Proof of Concept CLI
- [ ] Scaffold Rust project with JSON-RPC and CLI crates
- [ ] Implement MCP handshake and tool discovery
- [ ] Add `dired`, `open-file`, `insert`, `split-window` tools
- [ ] Use `emacsclient --eval` for all elisp calls
- [ ] Document manual start/stop instructions

## Phase 2: Tool Expansion
- [ ] Add buffer manipulation (save, close, switch)
- [ ] Provide window management helpers
- [ ] Return structured results (JSON) for tool responses
- [ ] Add logging and diagnostics

## Phase 3: Toward Dynamic Library
- [ ] Abstract Emacs bridge behind a trait for alternate backends
- [ ] Provide shared library interface callable from elisp
- [ ] Support running as a background service with auth tokens

## Milestones
| Date (est.) | Deliverable |
|-------------|-------------|
| Week 1 | MCP handshake and `dired` tool working |
| Week 2 | Initial tool set implemented |
| Week 3 | Expanded tools and documentation |
| Week 4+ | Research and prototype dynamic library |

## Risks
- Dependency on a running Emacs server (`emacs --daemon`)
- Evolving MCP specification may require protocol changes
- Security considerations once executing arbitrary elisp

## Open Questions
- Should the server maintain session state for multiple clients?
- What permission model is appropriate for tool invocation?
