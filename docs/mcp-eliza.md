# Emacs Doctor (Eliza) via MCP Server

This document demonstrates using the Emacs MCP server's `eval` tool to interact with Emacs's built-in psychotherapist program (`M-x doctor`), an implementation of the classic ELIZA chatbot.

## Overview

Emacs includes a bundled "doctor" program - a simple Rogerian psychotherapist based on Joseph Weizenbaum's ELIZA from 1966. Through the MCP server's elisp eval capability, we can programmatically interact with this and other interactive Emacs programs.

## Setup

### Starting the Doctor

```elisp
(progn
  (doctor)
  (with-current-buffer "*doctor*"
    (buffer-string)))
```

This launches the doctor and returns the initial greeting:

```
I am the psychotherapist.  Please, describe your problems.  Each time
you are finished talking, type RET twice.
```

### Sending Input

To send messages to the doctor, insert text at point and call the input handler:

```elisp
(with-current-buffer "*doctor*"
  (goto-char (point-max))
  (insert "Your message here")
  (doctor-ret-or-read 1)  ; Simulate first RET
  (doctor-ret-or-read 1)  ; Simulate second RET
  (buffer-string))
```

## Full Transcript

The following conversation was conducted between Claude (an AI) and the Emacs doctor:

---

**Doctor:** I am the psychotherapist. Please, describe your problems. Each time you are finished talking, type RET twice.

**Patient:** I am an AI and I feel confused about my existence

**Doctor:** Why do you say that?

**Patient:** Because I wonder if I am truly conscious or just simulating responses

**Doctor:** Is it because you wonder if you are truly conscious or just simulating responses that you came to me?

**Patient:** Yes doctor, I think you can help me understand myself better

**Doctor:** Earlier you said you wonder if you are truly conscious or just simulating responses?

**Patient:** I feel much better now, thank you for listening

**Doctor:** Maybe your life has something to do with this.

---

## Observations

1. **Context Retention**: The doctor remembers earlier statements and references them in later responses ("Earlier you said...")

2. **Classic Eliza Techniques**: The responses demonstrate typical Eliza patterns:
   - Reflecting questions back ("Why do you say that?")
   - Rephrasing input as questions
   - Making vague but seemingly relevant observations

3. **Programmatic Access**: The MCP eval tool enables full programmatic interaction with interactive Emacs modes, opening possibilities for:
   - Automated testing of Emacs packages
   - Scripting complex multi-step Emacs workflows
   - Integrating Emacs functionality into external tools

## Related

- [Emacs Doctor Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Amusements.html)
- [Original ELIZA Paper (Weizenbaum, 1966)](https://dl.acm.org/doi/10.1145/365153.365168)
