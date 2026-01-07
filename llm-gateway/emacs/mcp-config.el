;;; mcp-config.el --- MCP (Model Context Protocol) integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Michael A. Wright
;;
;; Author: Michael A. Wright
;; URL: https://github.com/softwarewrighter/emacs-ai-api
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

;; Configuration for MCP servers to work with gptel
;; Supports Playwright for browser automation and custom Rust MCP servers

;;; Commentary:
;; This assumes you have mcp.el installed from:
;; https://github.com/lizqwerscott/mcp.el
;;
;; Installation:
;; 1. Install mcp.el package
;; 2. Install MCP servers:
;;    - Playwright: npm install -g @modelcontextprotocol/server-playwright
;;    - Your custom Rust MCP servers
;; 3. Load this configuration

;;; Code:

(require 'mcp nil t)  ; Soft requirement

(when (featurep 'mcp)
  
  ;; ============================================================================
  ;; MCP Server Definitions
  ;; ============================================================================
  
  ;; Playwright MCP Server (browser automation)
  (mcp-register
   :name "playwright"
   :command "npx"
   :args '("@modelcontextprotocol/server-playwright")
   :description "Browser automation and testing"
   :env '(("NODE_ENV" . "production")))
  
  ;; Filesystem MCP Server (file operations)
  (mcp-register
   :name "filesystem"
   :command "npx"
   :args '("@modelcontextprotocol/server-filesystem" 
           "--root" "/Users/mike/github")
   :description "File system operations"
   :env nil)
  
  ;; Custom Rust MCP Server example
  (when (file-executable-p "/usr/local/bin/rust-mcp-server")
    (mcp-register
     :name "rust-mcp"
     :command "/usr/local/bin/rust-mcp-server"
     :args '("--port" "3000")
     :description "Custom Rust MCP tools"
     :env '(("RUST_LOG" . "info"))))
  
  ;; SQLite MCP Server (database operations)
  (mcp-register
   :name "sqlite"
   :command "npx"
   :args '("@modelcontextprotocol/server-sqlite"
           "--db" "/Users/mike/.local/share/llm-tools/history.db")
   :description "SQLite database operations"
   :env nil)
  
  ;; Git MCP Server (version control)
  (mcp-register
   :name "git"
   :command "npx"
   :args '("@modelcontextprotocol/server-git")
   :description "Git operations"
   :env nil)
  
  ;; ============================================================================
  ;; Helper Functions
  ;; ============================================================================
  
  (defun mcp-start-all ()
    "Start all registered MCP servers."
    (interactive)
    (mcp-start-server "playwright")
    (mcp-start-server "filesystem")
    (when (mcp-server-registered-p "rust-mcp")
      (mcp-start-server "rust-mcp"))
    (mcp-start-server "sqlite")
    (mcp-start-server "git")
    (message "Started all MCP servers"))
  
  (defun mcp-stop-all ()
    "Stop all running MCP servers."
    (interactive)
    (mcp-stop-all-servers)
    (message "Stopped all MCP servers"))
  
  (defun mcp-status ()
    "Show status of all MCP servers."
    (interactive)
    (let ((servers (mcp-list-servers)))
      (if servers
          (with-current-buffer (get-buffer-create "*MCP Status*")
            (erase-buffer)
            (insert "MCP Server Status\n")
            (insert "=================\n\n")
            (dolist (server servers)
              (let* ((name (plist-get server :name))
                     (running (mcp-server-running-p name)))
                (insert (format "%-20s %s\n" 
                               name
                               (if running 
                                   (propertize "● Running" 'face '(:foreground "green"))
                                 (propertize "● Stopped" 'face '(:foreground "red")))))))
            (display-buffer (current-buffer)))
        (message "No MCP servers registered"))))
  
  (defun mcp-test-playwright ()
    "Test Playwright MCP server with a simple task."
    (interactive)
    (when (mcp-server-running-p "playwright")
      (let ((gptel-expert-commands t))
        (gptel-request
         "Using the Playwright MCP tool, navigate to https://example.com and take a screenshot"
         :system "You have access to Playwright MCP tools for browser automation."))))
  
  ;; ============================================================================
  ;; Integration with gptel
  ;; ============================================================================
  
  (defun mcp-gptel-setup ()
    "Setup MCP integration with gptel."
    (when (and (featurep 'gptel) (featurep 'mcp))
      ;; Add MCP context to gptel system prompt when needed
      (defun gptel-add-mcp-context ()
        "Add MCP tool availability to system prompt."
        (when (mcp-any-server-running-p)
          (let ((tools (mcp-list-available-tools)))
            (when tools
              (setq gptel-system-message
                    (concat gptel-system-message
                            "\n\nYou have access to the following MCP tools:\n"
                            (mapconcat #'identity tools "\n")))))))
      
      ;; Hook to add MCP context before sending
      (add-hook 'gptel-pre-send-hook #'gptel-add-mcp-context)))
  
  ;; ============================================================================
  ;; Key Bindings
  ;; ============================================================================
  
  (defvar mcp-map (make-sparse-keymap)
    "Keymap for MCP commands.")
  
  (define-key mcp-map (kbd "s") #'mcp-start-all)
  (define-key mcp-map (kbd "x") #'mcp-stop-all)
  (define-key mcp-map (kbd "l") #'mcp-status)
  (define-key mcp-map (kbd "t") #'mcp-test-playwright)
  
  ;; Bind to C-c m (you can change this)
  (global-set-key (kbd "C-c m") mcp-map)
  
  ;; ============================================================================
  ;; Auto-start servers (optional)
  ;; ============================================================================
  
  (defcustom mcp-auto-start-servers nil
    "List of MCP servers to start automatically."
    :type '(repeat string)
    :group 'mcp)
  
  (defun mcp-auto-start ()
    "Auto-start configured MCP servers."
    (dolist (server mcp-auto-start-servers)
      (when (mcp-server-registered-p server)
        (mcp-start-server server)
        (message "Auto-started MCP server: %s" server))))
  
  ;; Uncomment to auto-start servers
  ;; (setq mcp-auto-start-servers '("playwright" "filesystem"))
  ;; (add-hook 'after-init-hook #'mcp-auto-start)
  
  ) ; end of (when (featurep 'mcp)

;; ============================================================================
;; Provide
;; ============================================================================

(provide 'mcp-config)

;;; mcp-config.el ends here