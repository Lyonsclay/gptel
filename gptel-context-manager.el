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
;;   x       - Execute deletions (delete marked items)
;;   D       - Delete item at point immediately
;;   RET     - Visit the source of context item
;;   s / b   - Switch target buffer
;;   M-p / K - Move item up
;;   M-n / J - Move item down
;;   g / gr  - Refresh the list
;;   q       - Quit
;;   ?       - Quick help

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'tabulated-list)
(require 'cl-lib)

;; Silence byte-compiler warnings for Evil functions
(declare-function evil-define-key "evil-core" t)
(declare-function evil-set-initial-state "evil-core" (mode state))

(defvar-local gptel-context-manager--target-buffer nil
  "The buffer whose context is currently being managed.")

(defgroup gptel-context-manager nil
  "Manager for gptel context."
  :group 'gptel)

(define-derived-mode gptel-context-manager-mode tabulated-list-mode "GPTel Context"
  "Major mode for managing gptel context.

\\{gptel-context-manager-mode-map}"
  :interactive nil
  (setq tabulated-list-format [("M" 2 t)
                               ("Type" 10 nil)
                               ("Name" 40 t)
                               ("Details" 30 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'gptel-context-manager--refresh nil t)
  (tabulated-list-init-header))

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
    map)
  "Keymap for =gptel-context-manager-mode'.")

;; Evil integration: use normal state and bind keys for this mode.
;; We use =eval-after-load' with a quoted lambda to avoid byte-compilation
;; issues with the =evil-define-key' macro.
(eval-after-load 'evil
  '(progn
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
       "?" #'gptel-context-manager-quick-help)
     ;; Ensure we start in normal state, not emacs state
     (evil-set-initial-state 'gptel-context-manager-mode 'normal)))

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
      (tabulated-list-print))
    (switch-to-buffer buf)))

(defun gptel-context-manager--refresh ()
  "Refresh the context list from the target buffer."
  (if (not (buffer-live-p gptel-context-manager--target-buffer))
      (progn
        (setq tabulated-list-entries nil)
        (message "Target buffer no longer exists"))
    ;; Update mode line to show which buffer we are managing
    (setq mode-line-process
          (list (format " [%s%s]"
                        (buffer-name gptel-context-manager--target-buffer)
                        (if (local-variable-p 'gptel-context gptel-context-manager--target-buffer)
                            ":Local" ""))))
    (let ((context (buffer-local-value 'gptel-context gptel-context-manager--target-buffer)))
      (setq tabulated-list-entries
            (seq-map-indexed #'gptel-context-manager--make-entry context)))))

(defun gptel-context-manager--make-entry (item _index)
  "Create a tabulated list entry from context ITEM.
_INDEX is the position in the context list (unused)."
  (let* ((source (if (consp item) (car item) item))
         (props (if (consp item) (cdr item) nil))
         (type (if (bufferp source) "Buffer" "File"))
         (name (if (bufferp source) (buffer-name source) (abbreviate-file-name source)))
         (details (gptel-context-manager--details source props)))
    (list item (vector "" type name details))))

(defun gptel-context-manager--details (source props)
  "Generate details string for SOURCE with PROPS."
  (cond
   ((bufferp source)
    (let ((ovs (plist-get props :overlays)))
      (if ovs (format "%d overlay%s" (length ovs) (if (= (length ovs) 1) "" "s"))
        "Full buffer")))
   ((stringp source)
    (or (plist-get props :mime) "Text"))
   (t "")))

(defun gptel-context-manager-quit ()
  "Quit the context manager."
  (interactive)
  (quit-window t))

(defun gptel-context-manager-quick-help ()
  "Show quick help for context manager."
  (interactive)
  (message
   (substitute-command-keys
    "\\[gptel-context-manager-mark-delete]:mark  \
\\[gptel-context-manager-unmark]:unmark  \
\\[gptel-context-manager-execute]:exec  \
\\[gptel-context-manager-delete]:del  \
\\[gptel-context-manager-switch-buffer]:switch  \
\\[gptel-context-manager-visit]:visit  \
\\[gptel-context-manager-move-up]/\\[gptel-context-manager-move-down]:move")))

(defun gptel-context-manager-switch-buffer (buffer)
  "Switch the context manager to view BUFFER."
  (interactive "bTarget Buffer: ")
  (setq gptel-context-manager--target-buffer (get-buffer buffer))
  (revert-buffer))

(defun gptel-context-manager-mark-delete ()
  "Mark the entry at point for deletion."
  (interactive)
  (when (tabulated-list-get-id)
    (tabulated-list-put-tag "D" t)))

(defun gptel-context-manager-unmark ()
  "Unmark the entry at point."
  (interactive)
  (when (tabulated-list-get-id)
    (tabulated-list-put-tag "" t)))

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
    (dolist (item items)
      ;; Clean up overlays in the item
      (let ((props (cdr-safe item)))
        (when props
          (dolist (ov (plist-get props :overlays))
            (when (overlayp ov) (delete-overlay ov)))))
      ;; Remove from list
      (with-current-buffer gptel-context-manager--target-buffer
        (setq gptel-context (delete item gptel-context))))))

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
            (pop-to-buffer source)
            (let ((props (cdr-safe item)))
              (when-let* ((ov (car-safe (plist-get props :overlays))))
                (when (overlayp ov)
                  (goto-char (overlay-start ov))))))
        (message "Buffer no longer exists")))
     ((stringp source)
      (if (file-exists-p source)
          (find-file-other-window source)
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
                (with-current-buffer gptel-context-manager--target-buffer
                  (let ((item-at-pos (nth pos gptel-context))
                        (item-at-new-pos (nth new-pos gptel-context)))
                    (setf (nth pos gptel-context) item-at-new-pos)
                    (setf (nth new-pos gptel-context) item-at-pos)))
                ;; Refresh and restore cursor position
                (revert-buffer)
                ;; Move cursor to the new position of the item
                (goto-char (point-min))
                (let ((target-line (1+ new-pos))) ; +1 for header
                  (forward-line target-line))))))))))

;; Live update advice - refresh all active context manager buffers
(defun gptel-context-manager--auto-refresh (&rest _)
  "Refresh all active context manager buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'gptel-context-manager-mode)
          (gptel-context-manager--refresh)
          (tabulated-list-print t t))))))

;; Add advice to gptel-context functions for live updates
(advice-add 'gptel-context-add :after #'gptel-context-manager--auto-refresh)
(advice-add 'gptel-context-remove :after #'gptel-context-manager--auto-refresh)
(advice-add 'gptel-context-remove-all :after #'gptel-context-manager--auto-refresh)
(advice-add 'gptel-add-file :after #'gptel-context-manager--auto-refresh)

(provide 'gptel-context-manager)
;;; gptel-context-manager.el ends here
