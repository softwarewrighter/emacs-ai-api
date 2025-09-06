#!/usr/bin/env bash
# Validate Elisp file for structural correctness (from methodology.md)
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 path/to/file.el" >&2
  exit 2
fi
FILE="$1"
if [[ ! -f "$FILE" ]]; then
  echo "File not found: $FILE" >&2
  exit 2
fi

# 1) Structural read + check-parens
emacs --batch --eval "(progn
  (condition-case err
      (with-temp-buffer
        (insert-file-contents \"$FILE\")
        (emacs-lisp-mode)
        ;; Fast structural check
        (check-parens)
        ;; Also try reading all top-level forms
        (goto-char (point-min))
        (while (not (eobp))
          (condition-case nil
              (forward-sexp)
            (scan-error (error \"Unbalanced parens at position %s\" (point)))))
        (message \"OK: Structure valid\"))
    (error
      (princ (format \"STRUCTURE-ERROR: %s\n\" (error-message-string err)))
      (kill-emacs 1))))"

echo "✓ Structural validation passed for $FILE"