# Dual Licensing Strategy

This document explains the licensing approach for the emacs-ai-api project and the legal requirements that informed these decisions.

## Overview

This project uses a **dual-licensing** model:

| Component | License | Rationale |
|-----------|---------|-----------|
| Root project / Emacs Lisp code | GPL-3.0-or-later | Required for Emacs integration |
| Rust MCP Server (`emacs-mcp-server/`) | MIT OR GPL-3.0-or-later | User's choice |

## Why Dual Licensing?

### Emacs and GPL Requirements

GNU Emacs is licensed under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.html). Code that integrates with Emacs has specific licensing requirements:

#### Emacs Lisp Extensions

According to the [GNU GPL FAQ](https://www.gnu.org/licenses/gpl-faq.html) and discussions in the Emacs community:

> "Any Emacs extension is more or less linked to the built-in Emacs LISP library, which is GPL licensed. In this case, Emacs extensions must use GPL as well."

Source: [spotify/dockerfile-mode#13](https://github.com/spotify/dockerfile-mode/issues/13)

The key legal principle is that Emacs Lisp code that uses Emacs's built-in functions and libraries creates a derivative work of Emacs itself. Under GPL's copyleft provisions, derivative works must be distributed under the same license.

#### Dynamic Modules (Native Code)

For native code modules that load into Emacs, there is an explicit technical requirement documented in the [GNU Emacs Lisp Reference Manual - Dynamic Modules](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html):

> "Every dynamic module should export a C-callable function named `emacs_module_init`, which Emacs will call as part of the call to load or require which loads the module. It should also export a symbol named `plugin_is_GPL_compatible` to indicate that its code is released under the GPL or compatible license; Emacs will signal an error if your program tries to load modules that don't export such a symbol."

This technical enforcement mechanism demonstrates that the Emacs project takes GPL compliance seriously for all code that integrates with it.

### Standalone Rust Code

The Rust MCP server (`emacs-mcp-server/`) is offered under dual licensing because:

1. **It can operate independently** - The MCP server communicates via standard protocols and can be used with any MCP client, not just Emacs
2. **Permissive licensing enables broader adoption** - MIT licensing allows integration into proprietary projects
3. **GPL compatibility maintained** - Users who integrate it with Emacs can choose the GPL license

## License Files

```
emacs-ai-api/
├── LICENSE                    # GPL-3.0-or-later (root project)
├── emacs-mcp-server/
│   └── LICENSE                # MIT (dual-licensed component)
└── docs/
    └── dual-license.md        # This document
```

## How This Codebase Complies

### Emacs Lisp Code (GPL-3.0-or-later)

All `.el` files in this repository include the standard GPL license header:

```elisp
;;; filename.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Michael A. Wright
;;
;; Author: Michael A. Wright
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is part of emacs-ai-api.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
```

This header format follows the recommendations from:
- [EmacsWiki: ExtraLicense](https://www.emacswiki.org/emacs/ExtraLicense)
- [GNU Coding Standards](https://www.gnu.org/prep/standards/html_node/License-Notices.html)

### Rust MCP Server (MIT OR GPL-3.0-or-later)

The `emacs-mcp-server/` directory contains its own MIT LICENSE file. Users may choose either:

1. **MIT License** - For use in proprietary or permissively-licensed projects
2. **GPL-3.0-or-later** - For integration with Emacs or other GPL projects

The Cargo.toml specifies the dual license using SPDX syntax:
```toml
license = "MIT OR GPL-3.0-or-later"
```

## Contributor Guidelines

### For Emacs Lisp contributions

By contributing Emacs Lisp code to this project, you agree to license your contributions under GPL-3.0-or-later.

### For Rust MCP Server contributions

By contributing to the `emacs-mcp-server/` directory, you agree to license your contributions under the dual MIT OR GPL-3.0-or-later license.

## References

### Official GNU Documentation
- [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.html)
- [GNU GPL FAQ](https://www.gnu.org/licenses/gpl-faq.html)
- [GNU Emacs Lisp Reference Manual - GPL](https://www.gnu.org/software/emacs/manual/html_node/elisp/GPL.html)
- [GNU Emacs Lisp Reference Manual - Dynamic Modules](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html)

### Community Resources
- [EmacsWiki: ExtraLicense](https://www.emacswiki.org/emacs/ExtraLicense)
- [SPDX License List](https://spdx.org/licenses/)

### Example Discussions
- [spotify/dockerfile-mode#13](https://github.com/spotify/dockerfile-mode/issues/13) - Discussion on why Emacs extensions require GPL
- [pd/easy-after-load#2](https://github.com/pd/easy-after-load/issues/2) - Request to specify license for Emacs packages
- [janestreet/ecaml#1](https://github.com/janestreet/ecaml/issues/1) - Discussion on Emacs modules and GPL

## Summary

| Question | Answer |
|----------|--------|
| Can I use the Emacs Lisp code in a proprietary project? | No, it's GPL-3.0-or-later |
| Can I use the Rust MCP server in a proprietary project? | Yes, under the MIT license |
| Can I fork this entire project? | Yes, but Emacs Lisp code remains GPL |
| What license should I use for contributions? | GPL for elisp, MIT OR GPL for Rust |
