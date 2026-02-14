;;; gptel-context-manager.el --- Context manager for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience, tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a tabulated-list based manager for gptel contexts.
;; It allows viewing, deleting, and reordering context items for a specific
;; target buffer.
;;
;; Usage: M-x gptel-context-manager
;;
;; Keybindings:
;;   d       - Mark item for deletion
;;   u       - Unmark item
;;   U       - Unmark all items
;;   t       - Toggle mark on item
;;   % m     - Mark items matching regexp
;;   x       - Execute deletions (delete marked items)
;;   D       - Delete item at point immediately
;;   RET     - Visit the source of context item
;;   s / b   - Switch target buffer
;;   M-p / K - Move item up
;;   M-n / J - Move item down
;;   g / gr  - Refresh the list
;;   a       - Add file to context
;;   B       - Add buffer to context
;;   r       - Add region (instructions)
;;   c       - Toggle context scope (global/local) for target buffer
;;   q       - Quit
;;   ?       - Quick help

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'seq)
(require 'tabulated-list)
(require 'cl-lib)

(eval-when-compile
  (require 'evil-core))

;; Silence byte-compiler warnings for Evil functions
(declare-function evil-set-initial-state "evil-core" (mode state))

(defvar-local gptel-context-manager--target-buffer nil
  "The buffer whose context is currently being managed.")

(declare-function gptel-context--local-p "gptel-context" (&optional buffer))
(declare-function gptel-context-toggle-locality "gptel-context" (&optional buffer))

(defgroup gptel-context-manager nil
  "Manager for gptel context."
  :group 'gptel)

(defface gptel-context-manager-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the target buffer header in context manager."
  :group 'gptel-context-manager)

(defface gptel-context-manager-target-buffer-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the target buffer name in context manager."
  :group 'gptel-context-manager)

(defface gptel-context-manager-mark-face
  '((t :inherit dired-mark))
  "Face for deletion marks in context manager."
  :group 'gptel-context-manager)

(defface gptel-context-manager-marked-face
  '((t :inherit dired-marked))
  "Face for marked items in context manager."
  :group 'gptel-context-manager)

(defface gptel-context-manager-type-buffer-face
  '((t :inherit font-lock-type-face))
  "Face for buffer type items in context manager."
  :group 'gptel-context-manager)

(defface gptel-context-manager-type-file-face
  '((t :inherit font-lock-string-face))
  "Face for file type items in context manager."
  :group 'gptel-context-manager)

(defface gptel-context-manager-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for item names in context manager."
  :group 'gptel-context-manager)

(defface gptel-context-manager-details-face
  '((t :inherit font-lock-comment-face))
  "Face for item details in context manager."
  :group 'gptel-context-manager)

(defface gptel-context-manager-empty-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for empty context message in context manager."
  :group 'gptel-context-manager)

(defvar gptel-context-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'gptel-context-manager-mark-delete)
    (define-key map (kbd "u") #'gptel-context-manager-unmark)
    (define-key map (kbd "x") #'gptel-context-manager-execute)
    (define-key map (kbd "D") #'gptel-context-manager-delete)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "RET") #'gptel-context-manager-visit)
    (define-key map (kbd "s") #'gptel-context-manager-switch-buffer)
    (define-key map (kbd "b") #'gptel-context-manager-switch-buffer)
    (define-key map (kbd "M-p") #'gptel-context-manager-move-up)
    (define-key map (kbd "M-n") #'gptel-context-manager-move-down)
    (define-key map (kbd "q") #'gptel-context-manager-quit)
    (define-key map (kbd "?") #'gptel-context-manager-quick-help)
    (define-key map (kbd "a") #'gptel-context-manager-add-file)
    (define-key map (kbd "B") #'gptel-context-manager-add-buffer)
    (define-key map (kbd "r") #'gptel-context-manager-add-region)
    (define-key map (kbd "c") #'gptel-context-manager-toggle-scope)
    ;; Non-conflicting alternatives (especially useful under Evil).
    (define-key map (kbd "C-c c") #'gptel-context-manager-toggle-scope)
    (define-key map (kbd "C-c g") #'revert-buffer)
    (define-key map (kbd "U") #'gptel-context-manager-unmark-all)
    (define-key map (kbd "t") #'gptel-context-manager-toggle-mark)
    (define-key map (kbd "%") #'gptel-context-manager-mark-regexp)
    map)
  "Keymap for =gptel-context-manager-mode'.")

(define-derived-mode gptel-context-manager-mode tabulated-list-mode "GPTel Context"
  "Major mode for managing gptel context.

\\{gptel-context-manager-mode-map}"
  :interactive nil
  (setq tabulated-list-format [("M" 2 t)
                               ("Type" 8 t)
                               ("Context Item" 0 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'gptel-context-manager--refresh nil t)
  (tabulated-list-init-header)
  (gptel-context-manager--update-header-line))

(add-hook 'gptel-context-manager-mode-hook
          #'gptel-context-manager--setup-evil)

;; Evil integration: use normal state and bind keys for this mode.
;; We use =eval-after-load' with a quoted lambda to avoid byte-compilation
;; issues with the =evil-define-key' macro.
(defun gptel-context-manager--setup-evil ()
  "Setup evil keybindings for gptel-context-manager-mode."
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'gptel-context-manager-mode 'normal)
    (evil-define-key 'normal gptel-context-manager-mode-map
      "d" #'gptel-context-manager-mark-delete
      "u" #'gptel-context-manager-unmark
      "x" #'gptel-context-manager-execute
      "D" #'gptel-context-manager-delete
      "gr" #'revert-buffer
      (kbd "RET") #'gptel-context-manager-visit
      "s" #'gptel-context-manager-switch-buffer
      "b" #'gptel-context-manager-switch-buffer
      "K" #'gptel-context-manager-move-up
      "J" #'gptel-context-manager-move-down
      "q" #'gptel-context-manager-quit
      "?" #'gptel-context-manager-quick-help
      "a" #'gptel-context-manager-add-file
      "r" #'gptel-context-manager-add-region
      "B" #'gptel-context-manager-add-buffer
      "U" #'gptel-context-manager-unmark-all
      "t" #'gptel-context-manager-toggle-mark
      "%" #'gptel-context-manager-mark-regexp)))

;;;###autoload
(defun gptel-context-manager (&optional buffer)
  "Manage gptel context for BUFFER.

BUFFER defaults to the current buffer.  Opens a tabulated list
interface for viewing, deleting, and reordering context items."
  (interactive (list (current-buffer)))
  (let ((target (or buffer (current-buffer)))
        (buf (get-buffer-create "*gptel-context-manager*")))
    (with-current-buffer buf
      (gptel-context-manager-mode)
      (setq gptel-context-manager--target-buffer target)
      (gptel-context-manager--refresh)
      (tabulated-list-print)
      (gptel-context-manager--update-header-line))
    (switch-to-buffer buf)))

(defun gptel-context-manager--update-header-line ()
  "Update the header line to show target buffer information."
  (setq header-line-format
        (if (buffer-live-p gptel-context-manager--target-buffer)
            (list
             (propertize " Target: " 'face 'gptel-context-manager-header-face)
             (propertize (buffer-name gptel-context-manager--target-buffer)
                         'face 'gptel-context-manager-target-buffer-face)
             (propertize
              (if (gptel-context--local-p gptel-context-manager--target-buffer) " (local context)" " (global context)")
              'face 'gptel-context-manager-details-face)
             (propertize
              (format "  |  %d item(s)"
                      (length (buffer-local-value 'gptel-context
                                                  gptel-context-manager--target-buffer)))
              'face 'gptel-context-manager-details-face))
          (list (propertize " Target buffer no longer exists"
                            'face 'error)))))


(defun gptel-context-manager--refresh ()
  "Refresh the context list from the target buffer."
  (if (not (buffer-live-p gptel-context-manager--target-buffer))
      (progn
        (setq tabulated-list-entries nil)
        (message "Target buffer no longer exists"))
    (let ((context (buffer-local-value 'gptel-context gptel-context-manager--target-buffer)))
      (gptel-context-manager--update-header-line)
      (setq tabulated-list-entries
            (seq-map-indexed #'gptel-context-manager--make-entry context)))))

(defun gptel-context-manager--make-entry (item _index)
  "Create a tabulated list entry from context ITEM.
_INDEX is the position in the context list (unused)."
  (let* ((source (if (consp item) (car item) item))
         (is-buffer (bufferp source))
         (spec (cdr-safe item))
         (has-overlays (and (listp spec) (plist-get spec :overlays)))
         (has-lines (and (listp spec) (plist-get spec :lines)))
         (has-bounds (and (listp spec) (plist-get spec :bounds)))
         (type
          (cond
           ((and is-buffer (or has-overlays has-lines has-bounds)) "region")
           (is-buffer "buffer")
           ((stringp source) "file")
           (t "unknown")))
         (name (cond
                (is-buffer (buffer-name source))
                ((stringp source) (abbreviate-file-name source))
                (t (format "%S" source))))
         (details
          (cond
           ((and (stringp source) (listp spec) (plist-get spec :mime))
            (format "mime=%s" (plist-get spec :mime)))
           ((and is-buffer (listp spec) has-overlays)
            (format "%d overlay(s)" (length (plist-get spec :overlays))))
           ((and is-buffer (listp spec) has-lines)
            "lines")
           ((and is-buffer (listp spec) has-bounds)
            "bounds")
           (t nil)))
         (entry (vector ""
                        (propertize type 'face 'gptel-context-manager-details-face)
                        (propertize
                         (if details (format "%s  (%s)" name details) name)
                         'face 'gptel-context-manager-name-face))))
    (list item entry)))

(defun gptel-context-manager-quit ()
  "Quit the context manager."
  (interactive)
  (quit-window t))

(defun gptel-context-manager-quick-help ()
  "Show quick help for context manager."
  (interactive)
  (message "d:mark  u:unmark  U:unmark-all  t:toggle  %%m:mark-re  x:exec  D:del  M-p/M-n:move  RET:visit  s:switch  a:+file  B:+buf  r:+region  c:scope  g:refresh  q:quit"))

(defun gptel-context-manager-toggle-scope ()
  "Toggle whether the target buffer's `gptel-context' is global or buffer-local."
  (interactive)
  (unless (buffer-live-p gptel-context-manager--target-buffer)
    (user-error "Target buffer no longer exists"))
  (let ((target gptel-context-manager--target-buffer)
        before after)
    (setq before (gptel-context--local-p target))
    (gptel-context-toggle-locality target)
    (setq after (gptel-context--local-p target))
    (revert-buffer)
    (message "Context for %s is now %s"
             (buffer-name target)
             (if after "local" "global"))))

(defun gptel-context-manager-switch-buffer (buffer)
  "Switch the context manager to view BUFFER."
  (interactive "bTarget Buffer: ")
  (setq gptel-context-manager--target-buffer (get-buffer buffer))
  (revert-buffer))

(defun gptel-context-manager-mark-delete ()
  "Mark the entry at point for deletion."
  (interactive)
  (when (tabulated-list-get-id)
    (tabulated-list-put-tag (propertize "D" 'face 'gptel-context-manager-mark-face) t)))

(defun gptel-context-manager-unmark ()
  "Unmark the entry at point."
  (interactive)
  (when (tabulated-list-get-id)
    (tabulated-list-put-tag "" t)))

(defun gptel-context-manager-unmark-all ()
  "Unmark all entries."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (tabulated-list-get-id)
        (tabulated-list-put-tag ""))
      (forward-line 1)))
  (message "All marks removed"))

(defun gptel-context-manager-toggle-mark ()
  "Toggle the deletion mark on the entry at point."
  (interactive)
  (when-let* ((item (tabulated-list-get-id)))
    (if (eq (char-after (line-beginning-position)) ?D)
        (tabulated-list-put-tag "" t)
      (tabulated-list-put-tag (propertize "D" 'face 'gptel-context-manager-mark-face) t))))

(defun gptel-context-manager-mark-regexp (regexp)
   "Mark all entries whose name matches REGEXP."
  (interactive "sMark items matching regexp: ")
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((item (tabulated-list-get-id)))
          (let* ((source (if (consp item) (car item) item))
                 (name (if (bufferp source) (buffer-name source)
                         (if (stringp source) source ""))))
            (when (string-match-p regexp name)
              (tabulated-list-put-tag (propertize "D" 'face 'gptel-context-manager-mark-face))
              (cl-incf count))))
        (forward-line 1)))
    (message "Marked %d item(s)" count)))

(defun gptel-context-manager-delete ()
  "Delete the entry at point immediately."
  (interactive)
  (when-let* ((item (tabulated-list-get-id)))
    (when (y-or-n-p "Remove this context item? ")
      (gptel-context-manager--remove-items (list item))
      (revert-buffer))))

(defun gptel-context-manager-execute ()
  "Delete marked entries."
  (interactive)
  (let ((items nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((item (tabulated-list-get-id)))
          (when (and item (eq (char-after (line-beginning-position)) ?D))
            (push item items)))
        (forward-line 1)))
    (if (not items)
        (message "No items marked for deletion.")
      (when (y-or-n-p (format "Remove %d item(s)? " (length items)))
        (gptel-context-manager--remove-items items)
        (revert-buffer)
        (message "Removed %d item(s)." (length items))))))

(defun gptel-context-manager--remove-items (items)
  "Remove ITEMS from the target buffer's context."
  (when (buffer-live-p gptel-context-manager--target-buffer)
    ;; Clean up overlays in the items being removed
    (dolist (item items)
      (let ((props (cdr-safe item)))
        (when props
          (dolist (ov (plist-get props :overlays))
            (when (overlayp ov)
              (delete-overlay ov))))))
    ;; Update context via the official setter to ensure hooks fire and any
    ;; scope/locality behavior is preserved consistently.
    (let* ((target gptel-context-manager--target-buffer)
           (old (buffer-local-value 'gptel-context target))
           (new (cl-set-difference old items :test #'equal)))
      (gptel-context--set new target))))

(defun gptel-context-manager-visit ()
  "Visit the source of the context item at point."
  (interactive)
  (let* ((item (tabulated-list-get-id))
         (source (if (consp item) (car item) item)))
    (cond
     ((null source)
      (message "No context item at point"))
     ((bufferp source)
      (if (buffer-live-p source)
          (progn
            ;; Changed from pop-to-buffer to switch-to-buffer
            (switch-to-buffer source)
            (let ((props (cdr-safe item)))
              (when-let* ((ov (car-safe (plist-get props :overlays))))
                (when (overlayp ov)
                  (goto-char (overlay-start ov))))))
        (message "Buffer no longer exists")))
     ((stringp source)
      (if (file-exists-p source)
          ;; Changed from find-file-other-window to find-file
          (find-file source)
        (message "File does not exist: %s" source))))))


(defun gptel-context-manager-move-up ()
  "Move the current item up in the context list."
  (interactive)
  (gptel-context-manager--move -1))

(defun gptel-context-manager-move-down ()
  "Move the current item down in the context list."
  (interactive)
  (gptel-context-manager--move 1))

(defun gptel-context-manager--move (delta)
  "Move current item by DELTA positions in the list.
Negative DELTA moves up, positive moves down."
  (let* ((item (tabulated-list-get-id))
         (ctx (and (buffer-live-p gptel-context-manager--target-buffer)
                   (buffer-local-value 'gptel-context gptel-context-manager--target-buffer))))
    (if (not item)
        (message "No item at point")
      (if (not ctx)
          (message "No context in target buffer")
        (let ((pos (cl-position item ctx :test #'equal)))
          (if (not pos)
              (message "Item not found in context")
            (let ((new-pos (+ pos delta))
                  (len (length ctx)))
              (if (or (< new-pos 0) (>= new-pos len))
                  (message "Cannot move further in that direction")
                ;; Perform the swap
                (let* ((target gptel-context-manager--target-buffer)
                       (ctx-copy (copy-sequence
                                  (buffer-local-value 'gptel-context target)))
                       (item-at-pos (nth pos ctx-copy))
                       (item-at-new-pos (nth new-pos ctx-copy)))
                  (setf (nth pos ctx-copy) item-at-new-pos)
                  (setf (nth new-pos ctx-copy) item-at-pos)
                  (gptel-context--set ctx-copy target))                ;; Refresh and restore cursor position
                (revert-buffer)
                ;; Move cursor to the new position of the item
                (goto-char (point-min))
                (let ((target-line (1+ new-pos))) ; +1 for header
                  (forward-line target-line))))))))))

(defun gptel-context-manager-add-file (file)
  "Add FILE to the target buffer's context."
  (interactive "fAdd file to context: ")
  (unless (buffer-live-p gptel-context-manager--target-buffer)
    (user-error "Target buffer no longer exists"))
  (gptel-context-add-file file gptel-context-manager--target-buffer)
  (revert-buffer))

(defun gptel-context-manager-add-buffer (buffer)
  "Add BUFFER to the target buffer's context."
  (interactive "bAdd buffer to context: ")
  (let ((target gptel-context-manager--target-buffer))
    (unless (buffer-live-p target)
      (user-error "Target buffer no longer exists"))
    (let ((buf (get-buffer buffer)))
      (unless buf
        (user-error "Buffer does not exist: %s" buffer))
      ;; Use the public API so scope/locality and hooks behave consistently.
      (gptel-context-add nil nil target)
      ;; The above DWIM call will add/remove from the *current* buffer unless we
      ;; ensure BUF is current.  Temporarily switch while preserving TARGET.
      (with-current-buffer buf
        (gptel-context-add nil nil target)))

    (revert-buffer)))

(defun gptel-context-manager-add-region ()
  "Switch to a buffer to select a region to add to context."
  (interactive)
  (unless (buffer-live-p gptel-context-manager--target-buffer)
    (user-error "Target buffer no longer exists"))
  (let ((target gptel-context-manager--target-buffer))
    (let ((buf (read-buffer "Buffer to select region from: " nil t)))
      (unless buf (user-error "No buffer selected"))
      (let ((source (get-buffer buf)))
        (unless source (user-error "Buffer does not exist: %s" buf))
        (switch-to-buffer source)
        (message
         (substitute-command-keys
          (format
           "Select a region in =%s', then \\[gptel-context-add] to add it to =%s' context."
           (buffer-name source)
           (buffer-name target)))))))
  ;; Set up a one-shot hook so the next gptel-context-add targets the right buffer
  (let ((target gptel-context-manager--target-buffer))
    (let ((hook nil))
      (setq hook
            (lambda (&optional _changed-buffer)
              (remove-hook 'gptel-context-modified-hook hook)
              (when (buffer-live-p target)
                (gptel-context-manager--auto-refresh))))
      (add-hook 'gptel-context-modified-hook hook))))

;; Live update hook - refresh all active context manager buffers
(defun gptel-context-manager--auto-refresh (&optional _changed-buffer)
  "Refresh all active context manager buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'gptel-context-manager-mode)
          (gptel-context-manager--refresh)
          (tabulated-list-print t t))))))

;; Use the official hook exposed by gptel-context.
(add-hook 'gptel-context-modified-hook #'gptel-context-manager--auto-refresh)

(provide 'gptel-context-manager)
;;; gptel-context-manager.el ends here
