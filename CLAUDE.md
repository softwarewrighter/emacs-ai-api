# CLAUDE.md - AI Agent Development Guidelines

This document provides checkpoint procedures and development guidelines for AI agents working on this codebase.

## Checkpoint Procedures

When a **checkpoint** is requested, execute ALL of the following steps in order. Repeat steps as needed if issues are found:

### 1. Check and Update Learnings
- **BEFORE making code changes**: Review `./docs/learnings.md` for previous issues and resolutions
- Apply learnings proactively to avoid repeating past mistakes
- Document any new issues encountered during this session

### 2. Run and Fix Tests
```bash
# For Rust projects
cargo test --all-features
cargo test --doc

# For mixed projects, check for other test suites
npm test  # if package.json exists
python -m pytest  # if Python tests exist
```
- Fix all failing tests
- Ensure all tests pass before proceeding

### 3. Fix Linting and Formatting Issues
```bash
# For Rust
cargo clippy --all-targets --all-features -- -D warnings
cargo fmt

# For other languages as applicable
eslint . --fix  # JavaScript/TypeScript
black .  # Python
shellcheck scripts/*.sh  # Shell scripts
```
- Resolve ALL warnings - treat warnings as errors
- Re-run until completely clean

### 4. Update Documentation
- Review and update README.md if functionality changed
- Update inline documentation for modified functions
- Ensure doc comments are accurate
- Update `./docs/learnings.md` with any new issues and resolutions

### 5. Git Status Management
```bash
git status
```
Categorize each file:
- **Add to commit**: Relevant source code, documentation, configs
- **Add to .gitignore**: 
  - Binary files (unless essential)
  - Build artifacts (`target/`, `dist/`, `build/`)
  - Secrets and environment files (`.env`, `.env.local`)
  - Logs (`*.log`, `logs/`)
  - Test output directories (`test-output/`, `tmp-test-*`)
  - Large files (models, datasets)
  - IDE/editor configs (`.vscode/`, `.idea/`)
- **Delete**: Temporary or obsolete files

### 6. Git Commit and Push
```bash
# Add files appropriately
git add <relevant-files>
# or for everything except ignored
git add -A

# Commit with proper format
git commit -m "feat/fix/docs: Short summary (50 chars or less)

Detailed explanation of what changed and why. Wrap at 72 characters.
Can be multiple paragraphs if needed.

- Bullet point 1 for specific changes
- Bullet point 2 for additional details
- Fixes issue #X or addresses specific problem

Co-authored-by: Claude <claude@anthropic.com>"

# ALWAYS push immediately
git push origin main  # or appropriate branch
```

**Commit Message Format Requirements:**
1. **First line**: Short summary, 50 characters or less
2. **Second line**: MUST be blank
3. **Body**: Detailed explanation starting on third line, wrapped at 72 characters
4. **Footer**: Co-author attribution or issue references

**CRITICAL**: Always finish with `git push` to enable testing on multiple systems.

## Development Guidelines

### Before Making Code Changes
1. **Always check `./docs/learnings.md`** for patterns to avoid
2. Review existing code style and patterns
3. Check for existing utilities before creating new ones

### Common Rust Patterns to Follow
From learnings.md:
- Use `//!` for module-level docs, `///` for item docs
- Use inline format arguments: `format!("{var}")` not `format!("{}", var)`
- Remove unused imports immediately
- Don't over-borrow in generic contexts
- Use Rust 2024 edition for new projects

### File Size and Complexity Limits
- Keep source files under 500 lines (prefer 200-300)
- Functions under 50 lines (prefer 10-30)
- Maximum 3 TODO comments per file
- Never commit FIXMEs - resolve immediately

### Testing Requirements
- Write tests in the same language as the code (Rust tests for Rust code)
- For WASM projects: Use `wasm-bindgen-test`, not JavaScript tests
- Leave test output in gitignored directories for debugging
- Use pattern: Clean before test, no cleanup after

### Git Workflow Requirements
1. **Incremental commits**: Logical, focused changes
2. **Immediate pushes**: Push after every commit
3. **Clean history**: Clear, descriptive commit messages
4. **Proper ignores**: Keep repository clean

### Error Documentation
When fixing errors during checkpoint:
1. Note the error type and cause
2. Document the fix in `./docs/learnings.md`
3. Add proactive prevention strategy
4. Apply fix across entire codebase

## Project-Specific Commands

### LiteLLM Gateway
```bash
# Start local LiteLLM
cd llm-gateway && docker compose up -d

# Test with local Ollama
./scripts/probe.sh

# Check recent usage
./scripts/llm-history.sh
```

### Emacs Configuration
- Config files in `llm-gateway/emacs/`
- Test with: `M-x load-file RET <config.el>`
- Master key for UI: `sk-local-test-key-123`
- LiteLLM UI: http://localhost:4000/ui

## Continuous Improvement Process

1. **Document issues**: Add to `./docs/learnings.md` immediately
2. **Update patterns**: Modify this file when new patterns emerge
3. **Propagate fixes**: Apply learnings to similar code elsewhere
4. **Test thoroughly**: Especially after refactoring

## Quick Checkpoint Checklist

- [ ] Reviewed `./docs/learnings.md`
- [ ] All tests passing
- [ ] Clippy warnings resolved
- [ ] Code formatted
- [ ] Documentation updated
- [ ] Git status clean (proper ignores)
- [ ] Meaningful commit message
- [ ] Changes pushed to remote

Remember: A checkpoint is not complete until `git push` succeeds!