
;;; gptel-context-manager.el --- Context manager for gptel  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'tabulated-list)
(require 'gptel-context)
(require 'transient)
(require 'compat)

(defgroup gptel-context-manager nil
  "Context manager for gptel."
  :group 'gptel)

(defvar-local gptel-context-manager--target-buffer nil
  "The buffer whose context is being managed.")

(defvar-local gptel-context-manager--window-configuration nil
  "Saved window configuration before opening the manager.")

(defvar-keymap gptel-context-manager-mode-map
  :doc "Keymap for gptel-context-manager-mode."
  "g"     #'gptel-context-manager-refresh
  "q"     #'gptel-context-manager-toggle)

(define-derived-mode gptel-context-manager-mode tabulated-list-mode "gptel-context"
  "Major mode for managing gptel context."
  (setq tabulated-list-format [("Mark" 4 t)
                               ("Type" 10 t)
                               ("Context Item" 0 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'gptel-context-manager-refresh nil t)
  (tabulated-list-init-header))

(defun gptel-context-manager-refresh ()
  "Refresh the context manager list from the target buffer."
  (interactive)
  (unless (buffer-live-p gptel-context-manager--target-buffer)
    (error "Target buffer for context manager is dead"))
  (let ((entries nil)
        (ctx-alist (buffer-local-value 'gptel-context--alist gptel-context-manager--target-buffer)))
    (dolist (item ctx-alist)
      (let* ((source (car item))
             (type (if (bufferp source) "buffer"
                     (if (stringp source) "file" "unknown")))
             (name (if (bufferp source)
                       (buffer-name source)
                     (if (stringp source)
                         (file-name-nondirectory source)
                       (format "%s" source)))))
        (push (list source (vector "" type name)) entries)))
    (setq tabulated-list-entries (nreverse entries))
    (setq header-line-format
          (format " Context for: %s | %d items "
                  (buffer-name gptel-context-manager--target-buffer)
                  (length entries)))
    (tabulated-list-print t t)))

;;;###autoload
(defun gptel-context-manager-toggle ()
  "Toggle the gptel context manager for the current buffer."
  (interactive)
  (let* ((target-buf (or gptel-context-manager--target-buffer (current-buffer)))
         (mgr-buf-name (format "*gptel-context: %s*" (buffer-name target-buf)))
         (mgr-buf (get-buffer mgr-buf-name)))
    (if (and mgr-buf (eq (current-buffer) mgr-buf))
        ;; We are in the manager buffer, hide it
        (let ((win-conf gptel-context-manager--window-configuration))
          (quit-window nil (selected-window))
          (when win-conf
            (set-window-configuration win-conf)))
      ;; We are not in the manager buffer, show it
      (if (not (buffer-live-p target-buf))
          (when mgr-buf (kill-buffer mgr-buf))
        (let ((win-conf (current-window-configuration)))
          (unless mgr-buf
            (setq mgr-buf (get-buffer-create mgr-buf-name))
            (with-current-buffer mgr-buf
              (gptel-context-manager-mode)
              (setq gptel-context-manager--target-buffer target-buf)))
          (with-current-buffer mgr-buf
            (setq gptel-context-manager--window-configuration win-conf)
            (gptel-context-manager-refresh))
          (switch-to-buffer mgr-buf))))))


(defun gptel-context-manager-move-up (&optional arg)
  "Move the current entry up by ARG positions in the context manager."
  (interactive "p")
  (let ((current-id (tabulated-list-get-id)))
    (unless current-id (error "No entry at point"))
    (with-current-buffer gptel-context-manager--target-buffer
      (let* ((alist gptel-context--alist)
             (pos (cl-position current-id alist :key #'car :test #'equal)))
        (unless pos (error "Entry not found in target buffer's context"))
        (when (> pos 0)
          (let ((target-pos (max 0 (- pos (or arg 1)))))
            (let ((elem (nth pos alist)))
              (setq gptel-context--alist (append (cl-subseq alist 0 target-pos)
                                                 (list elem)
                                                 (cl-subseq alist target-pos pos)
                                                 (nthcdr (1+ pos) alist))))))))
    (gptel-context-manager-refresh)
    (gptel-context-manager--goto-id current-id)))

(defun gptel-context-manager-move-down (&optional arg)
  "Move the current entry down by ARG positions in the context manager."
  (interactive "p")
  (let ((current-id (tabulated-list-get-id)))
    (unless current-id (error "No entry at point"))
    (with-current-buffer gptel-context-manager--target-buffer
      (let* ((alist gptel-context--alist)
             (pos (cl-position current-id alist :key #'car :test #'equal)))
        (unless pos (error "Entry not found in target buffer's context"))
        (when (< pos (1- (length alist)))
          (let ((target-pos (min (length alist) (+ pos (or arg 1)))))
            (let ((elem (nth pos alist)))
              (setq gptel-context--alist (append (cl-subseq alist 0 pos)
                                                 (cl-subseq alist (1+ pos) (1+ target-pos))
                                                 (list elem)
                                                 (nthcdr (1+ target-pos) alist))))))))
    (gptel-context-manager-refresh)
    (gptel-context-manager--goto-id current-id)))

(defun gptel-context-manager--goto-id (id)
  "Move point to the line containing ID."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (equal (tabulated-list-get-id) id)))
    (forward-line 1)))

(defvar gptel-context-manager-mark-char ?*
  "Character used to mark entries.")

(defun gptel-context-manager-mark ()
  "Mark the current entry for deletion."
  (interactive)
  (tabulated-list-put-tag (string gptel-context-manager-mark-char) t))

(defun gptel-context-manager-unmark ()
  "Unmark the current entry."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun gptel-context-manager-execute ()
  "Delete marked entries from the context."
  (interactive)
  (let ((to-delete nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((mark (aref (tabulated-list-get-entry) 0)))
          (when (equal mark (string gptel-context-manager-mark-char))
            (push (tabulated-list-get-id) to-delete)))
        (forward-line 1)))
    (when to-delete
      (with-current-buffer gptel-context-manager--target-buffer
        (setq gptel-context--alist
              (cl-remove-if (lambda (item) (member (car item) to-delete))
                            gptel-context--alist)))
      (gptel-context-manager-refresh))))

(defvar-local gptel-context-manager-roots nil
  "List of project roots managed by the context manager.")

(defun gptel-context-manager-add-root (root)
  "Add ROOT to the project roots."
  (interactive "DAdd project root: ")
  (let ((expanded (expand-file-name root)))
    (with-current-buffer gptel-context-manager--target-buffer
      (cl-pushnew expanded gptel-context-manager-roots :test #'equal))
    (when (derived-mode-p 'gptel-context-manager-mode)
      (gptel-context-manager-refresh))))

(defun gptel-context-manager-feature-root (root)
  "Feature ROOT (move to top of the roots list)."
  (interactive
   (list (completing-read "Feature project root: " 
                          (buffer-local-value 'gptel-context-manager-roots gptel-context-manager--target-buffer))))
  (with-current-buffer gptel-context-manager--target-buffer
    (setq gptel-context-manager-roots
          (cons root (remove root gptel-context-manager-roots))))
  (when (derived-mode-p 'gptel-context-manager-mode)
    (gptel-context-manager-refresh)))

(defun gptel-context-manager-remove-root (root)
  "Remove ROOT from the project roots."
  (interactive
   (list (completing-read "Remove project root: " 
                          (buffer-local-value 'gptel-context-manager-roots gptel-context-manager--target-buffer))))
  (with-current-buffer gptel-context-manager--target-buffer
    (setq gptel-context-manager-roots (remove root gptel-context-manager-roots)))
  (when (derived-mode-p 'gptel-context-manager-mode)
    (gptel-context-manager-refresh)))

(defvar-local gptel-context-manager-region-selection-mode nil)

(define-minor-mode gptel-context-manager-region-selection-mode
  "Temporary minor mode for region selection."
  :lighter " Gptel-Region"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-;") #'gptel-context-manager-region-selection-confirm)
            map)
  (if gptel-context-manager-region-selection-mode
      (message "Select region, then C-c C-; to confirm")
    (message "Region selection finished")))

(defun gptel-context-manager-add-region ()
  "Start region selection mode in the target buffer."
  (interactive)
  (let ((target-buf gptel-context-manager--target-buffer))
    (gptel-context-manager-toggle) ;; hide manager
    (with-current-buffer target-buf
      (gptel-context-manager-region-selection-mode 1))))

(defun gptel-context-manager-region-selection-confirm ()
  "Confirm region selection and add to context."
  (interactive)
  (unless (region-active-p)
    (error "No active region"))
  (gptel-context-manager-region-selection-mode -1)
  (gptel-context--add-region (region-beginning) (region-end))
  (deactivate-mark)
  (gptel-context-manager-toggle))


(provide 'gptel-context-manager)
;;; gptel-context-manager.el ends here
