# Learning Emacs Through Self-Documentation via MCP

This document demonstrates how an AI can use Emacs's built-in documentation system through the MCP server to learn and apply Emacs concepts autonomously.

## Overview

Emacs is famously "self-documenting" - every function, variable, and concept has accessible documentation. Through the MCP eval tool, an AI can:

1. Discover related symbols with `apropos`
2. Read documentation to understand APIs
3. Write code applying what it learned
4. Test and iterate in real-time

This creates a powerful learning loop where the AI teaches itself Emacs capabilities on-demand.

## Documentation Functions

### Core Documentation Access

```elisp
;; Find symbols matching a pattern
(apropos-internal "kill-ring")
;; => (kill-ring kill-ring-max kill-ring-save kill-new ...)

;; Get function documentation
(documentation 'kill-new)
;; => "Make STRING the latest kill in the kill ring..."

;; Get variable documentation
(documentation-property 'kill-ring 'variable-documentation)
;; => "List of killed text sequences..."
```

### Available Documentation Functions

| Function | Purpose | Example |
|----------|---------|---------|
| `apropos-internal` | Find symbols matching pattern | `(apropos-internal "buffer")` |
| `documentation` | Get function docstring | `(documentation 'save-buffer)` |
| `documentation-property` | Get variable docstring | `(documentation-property 'fill-column 'variable-documentation)` |
| `describe-function` | Full interactive description | Opens help buffer |
| `describe-variable` | Full variable description | Opens help buffer |
| `where-is-internal` | Find key bindings | `(where-is-internal 'save-buffer)` |

## Example 1: Learning the Kill Ring

### Discovery Phase

```elisp
;; What kill-ring related things exist?
(apropos-internal "kill-ring")
```

Result: `(kill-ring kill-ring-max kill-ring-save kill-new current-kill ...)`

### Documentation Phase

```elisp
(documentation-property 'kill-ring 'variable-documentation)
```

Result:
> List of killed text sequences. Since the kill ring is supposed to interact nicely with cut-and-paste facilities offered by window systems, use of this variable should interact nicely with 'interprogram-cut-function' and 'interprogram-paste-function'. The functions 'kill-new', 'kill-append', and 'current-kill' are supposed to implement this interaction; you may want to use them instead of manipulating the kill ring directly.

### Application Phase

```elisp
;; Add items to the kill ring
(progn
  (kill-new "First item")
  (kill-new "Second item")
  (kill-new "Third - most recent")
  (format "Kill ring has %d items:\n1. %s\n2. %s\n3. %s"
          (length kill-ring)
          (nth 0 kill-ring)
          (nth 1 kill-ring)
          (nth 2 kill-ring)))
```

Result:
```
Kill ring has 5 items:
1. Third - most recent
2. Second item
3. First item
```

## Example 2: Learning the Advice System

### Discovery Phase

```elisp
(documentation 'advice-add)
```

Result (excerpt):
> HOW can be one of:
> - `:around` - (lambda (&rest r) (apply FUNCTION OLDFUN r))
> - `:before` - (lambda (&rest r) (apply FUNCTION r) (apply OLDFUN r))
> - `:after` - (lambda (&rest r) (prog1 (apply OLDFUN r) (apply FUNCTION r)))
> - `:filter-return` - (lambda (&rest r) (funcall FUNCTION (apply OLDFUN r)))

### Application Phase

```elisp
;; Define a simple function
(defun my-greet (name)
  (format "Hello, %s!" name))

(my-greet "World")
;; => "Hello, World!"

;; Add advice to transform the return value
(advice-add 'my-greet :filter-return
            (lambda (result) (upcase result))
            '((name . "uppercaser")))

(my-greet "Emacs")
;; => "HELLO, EMACS!"

;; Remove advice to restore original behavior
(advice-remove 'my-greet "uppercaser")

(my-greet "World")
;; => "Hello, World!"
```

## Learning Workflow

```
┌─────────────────┐
│  1. DISCOVER    │  apropos-internal, apropos-command
│  Find symbols   │  "What exists related to X?"
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  2. UNDERSTAND  │  documentation, documentation-property
│  Read docs      │  "How does X work?"
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  3. APPLY       │  Write elisp using the API
│  Write code     │  "Let me try using X"
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  4. VERIFY      │  Execute and check results
│  Test & iterate │  "Did it work as expected?"
└─────────────────┘
```

## Teaching Applications

### Interactive Concept Exploration

User: "How do hooks work in Emacs?"

AI can:
1. `(apropos-internal "hook")` - find hook-related symbols
2. `(documentation 'add-hook)` - learn the API
3. Create a demo hook and show it firing
4. Explain with working examples

### Code Explanation

Given unfamiliar elisp, AI can look up each function:

```elisp
;; For each unknown function in user's code:
(documentation 'some-unknown-function)
```

### Feature Discovery

AI can explore what's available:

```elisp
;; What can I do with buffers?
(apropos-internal "^buffer-" 'functionp)

;; What modes are available?
(apropos-internal "-mode$" 'commandp)
```

## Limitations

| Limitation | Workaround |
|------------|------------|
| Large docstrings may truncate | Request specific sections |
| Info manual not directly accessible | Use `info-lookup-symbol` or fetch online |
| Interactive commands need minibuffer | Supply arguments programmatically |
| Some docs reference other docs | Follow references manually |

## Potential Enhancements

1. **Structured documentation tool** - Parse docstrings into structured data
2. **Info manual access** - Direct access to full Emacs/Elisp manual
3. **Example extraction** - Find usage examples in loaded packages
4. **Cross-reference resolution** - Automatically follow "see also" references

## Related

- [Emacs Self-Documentation](https://www.gnu.org/software/emacs/manual/html_node/emacs/Help.html)
- [Elisp Documentation Functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation.html)
