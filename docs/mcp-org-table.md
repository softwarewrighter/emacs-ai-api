# Org-mode Table Calculations via MCP Server

This document demonstrates using the Emacs MCP server's `eval` tool to create and calculate org-mode tables with spreadsheet formulas.

## Overview

Org-mode tables include powerful spreadsheet capabilities with formulas for sums, averages, conditionals, and more. Through the MCP server's elisp eval, we can programmatically create tables, apply formulas, and retrieve calculated results.

## Creating a Table with Formulas

### Setup Code

```elisp
(progn
  ;; Create a buffer with an org table
  (switch-to-buffer (get-buffer-create "*org-calc-demo*"))
  (erase-buffer)
  (org-mode)
  (insert "#+TITLE: MCP Org Table Demo

| Item       | Q1 | Q2 | Q3 | Q4 | Total |
|------------+----+----+----+----+-------|
| Revenue    | 50 | 75 | 60 | 90 |       |
| Expenses   | 30 | 40 | 35 | 45 |       |
| Marketing  | 10 | 15 | 20 | 25 |       |
|------------+----+----+----+----+-------|
| Column Sum |    |    |    |    |       |
#+TBLFM: $6=vsum($2..$5)::@5$2=vsum(@2..@4)::@5$3=vsum(@2..@4)::@5$4=vsum(@2..@4)::@5$5=vsum(@2..@4)::@5$6=vsum(@2..@5)
")
  ;; Apply the table formulas
  (goto-char (point-min))
  (search-forward "| Item")
  (org-table-recalculate 'all)
  (buffer-substring-no-properties (point-min) (point-max)))
```

### Formula Breakdown

The `#+TBLFM:` line defines the spreadsheet formulas:

| Formula | Meaning |
|---------|---------|
| `$6=vsum($2..$5)` | Column 6 (Total) = sum of columns 2-5 for each row |
| `@5$2=vsum(@2..@4)` | Row 5, Column 2 = sum of rows 2-4 (Q1 column sum) |
| `@5$3=vsum(@2..@4)` | Row 5, Column 3 = sum of rows 2-4 (Q2 column sum) |
| `@5$4=vsum(@2..@4)` | Row 5, Column 4 = sum of rows 2-4 (Q3 column sum) |
| `@5$5=vsum(@2..@4)` | Row 5, Column 5 = sum of rows 2-4 (Q4 column sum) |
| `@5$6=vsum(@2..@5)` | Row 5, Column 6 = grand total |

### Reference Syntax

- `$N` - Column N (applies to all rows)
- `@N` - Row N
- `@N$M` - Specific cell at row N, column M
- `@2..@4` - Range from row 2 to row 4
- `$2..$5` - Range from column 2 to column 5

## Result

After `org-table-recalculate`, the table is populated:

```
#+TITLE: MCP Org Table Demo

| Item       | Q1 |  Q2 |  Q3 |  Q4 | Total |
|------------+----+-----+-----+-----+-------|
| Revenue    | 50 |  75 |  60 |  90 |   275 |
| Expenses   | 30 |  40 |  35 |  45 |   150 |
| Marketing  | 10 |  15 |  20 |  25 |    70 |
|------------+----+-----+-----+-----+-------|
| Column Sum | 90 | 130 | 115 | 160 |   495 |
#+TBLFM: $6=vsum($2..$5)::@5$2=vsum(@2..@4)::@5$3=vsum(@2..@4)::@5$4=vsum(@2..@4)::@5$5=vsum(@2..@4)::@5$6=vsum(@2..@5)
```

### Verification

- **Revenue total**: 50 + 75 + 60 + 90 = 275 ✓
- **Expenses total**: 30 + 40 + 35 + 45 = 150 ✓
- **Marketing total**: 10 + 15 + 20 + 25 = 70 ✓
- **Q1 sum**: 50 + 30 + 10 = 90 ✓
- **Q2 sum**: 75 + 40 + 15 = 130 ✓
- **Q3 sum**: 60 + 35 + 20 = 115 ✓
- **Q4 sum**: 90 + 45 + 25 = 160 ✓
- **Grand total**: 90 + 130 + 115 + 160 = 495 ✓

## Reading Clean Output

To retrieve table contents without text properties (fontification metadata):

```elisp
(with-current-buffer "*org-calc-demo*"
  (buffer-substring-no-properties (point-min) (point-max)))
```

## Additional Formula Functions

Org-mode tables support many spreadsheet functions:

| Function | Description |
|----------|-------------|
| `vsum` | Vertical sum |
| `vmean` | Average |
| `vmax` / `vmin` | Maximum / Minimum |
| `vcount` | Count non-empty cells |
| `vprod` | Product |
| `if(cond, yes, no)` | Conditional |
| `sqrt`, `exp`, `log` | Math functions |

## Use Cases

1. **Financial reports** - Quarterly summaries, budgets, expense tracking
2. **Data analysis** - Quick calculations without leaving Emacs
3. **Project metrics** - Story points, velocity, time tracking
4. **Gradebooks** - Student scores and weighted averages
5. **Inventory** - Stock counts, reorder calculations

## Related

- [Org-mode Table Manual](https://orgmode.org/manual/Tables.html)
- [Org-mode Spreadsheet Manual](https://orgmode.org/manual/The-Spreadsheet.html)
