# GPTel Embedded Context Manager — Requirements

## 1. Objectives
- Provide a robust, self‑contained context manager UI that can be invoked from a conversation buffer without modifying gptel core.
- Match or exceed the original context-manager feature set (mark/delete/visit/reorder, add file/buffer/region, project root management, persistence, help).
- Ensure predictable behavior in GUI/TTY and with Evil enabled.
- Preserve gptel’s current context semantics: all mutations apply to the originating conversation buffer.

## 2. Constraints & Non‑Goals
- No edits to core gptel files.
- Manager must not rely on global overrides (e.g., `overriding-local-map`).
- Must not push to Org’s mark ring during property reads/writes.
- Must remain compatible with TTY Emacs and `evil-mode`.

## 3. Invocation & Lifecycle
- **Command:** `gptel-context-manager-toggle`.
- Toggle shows/hides manager in the **same window**.
- On show: save window configuration; on hide: restore it.
- Manager buffer is named `*gptel-context: <buffer>*` (or equivalent) and is transient.
- If the target buffer is killed, the manager must close gracefully and clear its state.

## 4. UI & Presentation
- Use `tabulated-list-mode` with columns: **Mark**, **Type**, **Context Item**.
- Header line must show:
  - Target buffer name
  - Count of context items
  - Featured root (and “+N more” when applicable)
- Provide styled faces:
  - Header / target buffer name
  - Root label
  - Marked vs unmarked
  - Type (buffer/region vs file)
  - Name/details
  - Empty list message
- Empty list renders a friendly prompt (e.g., “No context items. Use a/B to add.”).
- Order matches `gptel-menu` by default; **optional reverse display** via customization.

## 5. Keybindings (Manager)
All bindings must work in Emacs and Evil (normal/motion):

- **Navigation & Actions**
  - `g` refresh
  - `RET` visit entry
  - `q` quit manager
  - `?` open help (transient)

- **Mark/Delete**
  - `d` mark delete
  - `u` unmark
  - `U` unmark all
  - `t` toggle mark
  - `%` mark regexp
  - `x` delete marked
  - `D` delete at point

- **Add**
  - `a` add file (uses featured root as default directory)
  - `B` add buffer (adds a **buffer** entry, not a region)
  - `r` add region

- **Project Roots**
  - `R` add root
  - `F` feature root (move to top)
  - `X` remove root

- **State**
  - `S` save state
  - `L` load state

- **Order**
  - `M-p` move entry up
  - `M-n` move entry down

## 6. Core Actions
### Refresh & Visit
- Refresh must rebuild the list from the target buffer.
- Visit must open the referenced file/buffer in another window.

### Mark/Delete
- Marking uses a styled mark character.
- Execute deletes all marked entries.
- Delete at point prompts for confirmation.

### Reordering
- Move up/down must reorder the underlying `gptel-context` list.
- Cursor should remain on the moved entry.
- Must work correctly when display order is reversed.

## 7. Add Commands
### Add File
- Prompts for file with default directory = featured root (if set).
- Uses gptel’s file add logic (binary checks, mime handling).

### Add Buffer
- Prompts for a live buffer.
- Adds a buffer entry (not region overlays).
- Entry type should render as **buffer**.

### Add Region
- Uses any active region in the selected buffer.
- If no region is active:
  - Offer interactive selection mode, or
  - Fall back to line‑number prompts.

## 8. Region Selection Mode
- Temporary buffer‑local minor mode enables two keys:
  - Confirm: `C-c C-;`
  - Cancel: `C-c C-,`
- Header line shows a persistent hint until confirm/cancel.
- Selection returns to manager and adds region overlays.

## 9. Project Roots
- Manager maintains `gptel-context-manager-roots` (buffer‑local to target).
- Roots are displayed in header (featured root + count of additional roots).
- Root commands update local state and refresh list immediately.

## 10. State Persistence
### Org Buffers
- Persist state to Org properties:
  - `GPTEL_PROJECT_ROOTS`
  - `GPTEL_CONTEXT`
- Use `org-entry-put` / `org-entry-delete` without pushing to mark ring.

### Non‑Org Buffers
- Maintain buffer‑local `gptel-context-manager-state`.
- With prefix arg, store file‑local variable for persistence.

## 11. Context Serialization
- Serializable entries:
  - File entries as strings or `(file :mime …)`
  - File‑backed buffer entries as `(:file PATH :lines (1 N))`
  - File‑backed region overlays as `(:file PATH :lines (START END))`
- On load:
  - Recreate overlays from `:file`/`:lines` entries
  - Skip non‑serializable items with an informative warning

## 12. Help Menu (Transient)
- Help is a **Magit‑style transient prefix** invoked by `?`.
- The menu must render fully on the first invocation.
- `q` must close the help menu; `Q` may quit the manager.
- Avoid custom display hacks unless required; use transient defaults where possible.

## 13. Evil Integration
- Provide equivalent bindings in Evil normal/motion state.
- Avoid compile/runtime macro errors (`evil-define-key` must be invoked safely).
- Set initial state to normal for the manager buffer.

## 14. Compatibility & Quality
- TTY and GUI compatibility.
- No global keymap overrides.
- Avoid side effects in unrelated buffers.
- Code should be structured, documented, and ready for open‑source review.
