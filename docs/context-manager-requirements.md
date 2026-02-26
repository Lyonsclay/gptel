# GPTel Embedded Context Manager — Requirements

## Goals
- Provide a robust, self-contained context manager UI that can be invoked from a conversation buffer without modifying gptel core.
- Maintain parity with the original context manager features (mark/delete, visit, reorder, add file/buffer/region, roots).
- Ensure behavior is predictable under Evil, TTY, and GUI Emacs.
- Keep state persistence in Org/file-local metadata without impacting gptel internals.

## Scope and Constraints
- The manager lives in a temporary buffer and reuses the current window (toggle show/hide).
- No modifications to core gptel files are required.
- All context mutations must apply to the originating (target) conversation buffer.
- UI should function in TTY and GUI.

## Functional Requirements

### Invocation + Lifecycle
- Provide `gptel-context-manager-toggle` to show/hide the manager.
- When shown, store window configuration and restore it on exit.
- If the target buffer is killed, the manager should detect and close gracefully.

### Display
- Use `tabulated-list-mode` with columns: mark, type, context item.
- Support styled faces for header, types, names, marks, and empty state.
- Display order matches the order shown in `gptel-menu` (default: same order as `gptel-context`).
- Allow an optional reversed display order via customization.

### Core Commands
- Refresh, visit, mark/unmark, mark regexp, delete, delete marked.
- Move entry up/down and keep cursor on moved entry.

### Add Commands
- Add file to context, defaulting to the featured root as the starting directory.
- Add buffer to context as a buffer entry (not a region).
- Add region to context using:
  - Active region in selected buffer, or
  - Interactive region selection mode (confirm/cancel), or
  - Line number prompts as fallback.

### Region Selection Flow
- Provide a buffer-local “region selection” minor mode with low-conflict keys:
  - Confirm: `C-c C-;`
  - Cancel: `C-c C-,`
- Persist a header-line hint while selection is active.

### Project Roots
- Maintain a buffer-local list of manager roots in the target buffer.
- Commands:
  - Add root
  - Feature root (move to top)
  - Remove root
- Header line should show the featured root and count of additional roots.

### State Persistence
- Persist manager state in Org properties when the target buffer is in Org mode:
  - `GPTEL_PROJECT_ROOTS`
  - `GPTEL_CONTEXT`
- For non-Org buffers, store `gptel-context-manager-state` (buffer-local) and optionally as a file-local variable with prefix argument.

### Context Serialization
- Serialize file contexts as strings or `(file :mime ...)` entries.
- Serialize file-backed buffers and regions as `(:file PATH :lines (START END))`.
- Restore serialized ranges by reopening the file and recreating overlays.
- Skip non-serializable entries with a warning.

### Help Menu
- A Magit-style transient menu opened via `?`.
- Should render fully on the first invocation and close via `q`.
- Avoid custom window display hacks unless required.

### Evil Integration
- Define normal/motion bindings equivalent to Emacs keymap.
- Use safe macro expansion so Evil doesn’t throw compile/runtime errors.

## Non-Functional Requirements
- Minimize global side effects (no overriding local maps globally).
- Avoid pushing to Org’s mark ring when reading/writing properties.
- Keep code structured and documented for open-source use.
