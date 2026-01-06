# Emacs MCP Server - Examples

This directory contains practical examples demonstrating how to use the Emacs MCP Server.

## Prerequisites

1. Build the MCP server:
   ```bash
   cd ..
   cargo build --release
   ```

2. Start Emacs in server mode:
   ```bash
   emacs --daemon
   ```

3. Install Python dependencies for web examples:
   ```bash
   pip install flask
   ```

## Examples

### 1. Code Review Assistant

A CLI tool that inserts AI-generated review comments into code files.

**Usage:**
```bash
# Make executable
chmod +x code-review.sh

# Review a file
./code-review.sh path/to/file.rs "Add error handling"

# The file opens in Emacs with the review comment inserted
```

### 2. Log Analyzer (Web Frontend)

A web application for analyzing log files with Emacs.

**Start:**
```bash
cd log-analyzer
python flask-app.py
```

**Access:** http://localhost:5000

**Features:**
- Upload log files via web interface
- Emacs opens the file with analysis markers
- Check your Emacs window to see results

### 3. Remote Pair Programming (Web Frontend)

A web-based collaboration tool for sharing an Emacs session.

**Start:**
```bash
cd pair-program
python flask-app.py
```

**Access:** http://localhost:5001

**Features:**
- Open files in shared Emacs session
- Insert text at cursor position
- Split windows for side-by-side viewing
- Multiple developers can collaborate

### 4. Documentation Generator

Automated documentation insertion for code files.

**Usage:**
```bash
# Generate docs for a single file
./generate-docs.sh path/to/file.rs

# Generate docs for all files in a directory
./generate-docs.sh project/src/
```

## Documentation

See `use-cases.org` for detailed documentation including:
- Step-by-step workflows
- Practical applications
- Architecture diagrams
- Future enhancements

## Tips

- Ensure Emacs server is running before testing
- Check that `emacsclient` is in your PATH
- For web examples, both developers need access to the same Emacs server
- Use `EMACS_SERVER_SOCKET` environment variable if using a custom socket

## Troubleshooting

**emacsclient connection failed:**
```bash
# Check if Emacs server is running
ps aux | grep emacs

# Start if needed
emacs --daemon
```

**Python Flask not found:**
```bash
pip install flask
```

**File not found errors:**
```bash
# Build the MCP server
cd ..
cargo build --release
```
