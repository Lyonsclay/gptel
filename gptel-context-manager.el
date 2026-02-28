
;;; gptel-context-manager.el --- Context manager for gptel  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'tabulated-list)
(require 'gptel-context)
(require 'transient)
(require 'compat)

(defgroup gptel-context-manager nil
  "Context manager for gptel."
  :group 'gptel)

;;; Faces

(defface gptel-context-manager-header-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for the header in the context manager.")

(defface gptel-context-manager-root-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the featured root.")

(defface gptel-context-manager-mark-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for marked entries.")

(defface gptel-context-manager-type-face
  '((t :inherit font-lock-type-face))
  "Face for the type column.")

(defface gptel-context-manager-file-face
  '((t :inherit font-lock-string-face))
  "Face for file context entries.")

(defface gptel-context-manager-buffer-face
  '((t :inherit font-lock-variable-name-face))
  "Face for buffer/region context entries.")

(defface gptel-context-manager-empty-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for empty list message.")

;;; Variables

(defvar-local gptel-context-manager--target-buffer nil
  "The buffer whose context is being managed.")

(defvar-local gptel-context-manager--window-configuration nil
  "Saved window configuration before opening the manager.")

(defvar-local gptel-context-manager-roots nil
  "List of project roots managed by the context manager.")

(defvar-local gptel-context-manager-state nil
  "Buffer-local persistence for non-org buffers.")

(defcustom gptel-context-manager-reverse-display nil
  "If non-nil, reverse the display order of the context manager list."
  :type 'boolean
  :group 'gptel-context-manager)

(defvar gptel-context-manager-mark-char ?*
  "Character used to mark entries.")

;;; Keymap

(defvar-keymap gptel-context-manager-mode-map
  :doc "Keymap for `gptel-context-manager-mode'."
  "g"     #'gptel-context-manager-refresh
  "q"     #'gptel-context-manager-toggle
  "RET"   #'gptel-context-manager-visit
  "?"     #'gptel-context-manager-help
  "d"     #'gptel-context-manager-mark
  "u"     #'gptel-context-manager-unmark
  "U"     #'gptel-context-manager-unmark-all
  "t"     #'gptel-context-manager-toggle-mark
  "%"     #'gptel-context-manager-mark-regexp
  "x"     #'gptel-context-manager-execute
  "D"     #'gptel-context-manager-delete-at-point
  "a"     #'gptel-context-manager-add-file
  "B"     #'gptel-context-manager-add-buffer
  "r"     #'gptel-context-manager-add-region
  "R"     #'gptel-context-manager-add-root
  "F"     #'gptel-context-manager-feature-root
  "X"     #'gptel-context-manager-remove-root
  "S"     #'gptel-context-manager-save-state
  "L"     #'gptel-context-manager-load-state
  "M-p"   #'gptel-context-manager-move-up
  "M-n"   #'gptel-context-manager-move-down)

;;; Mode Definition

(define-derived-mode gptel-context-manager-mode tabulated-list-mode "gptel-context"
  "Major mode for managing gptel context."
  (setq tabulated-list-format [("Mark" 4 t)
                               ("Type" 10 t)
                               ("Context Item" 0 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'gptel-context-manager-refresh nil t)
  (tabulated-list-init-header))

;; Evil integration
(with-eval-after-load 'evil
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'gptel-context-manager-mode 'normal))
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map gptel-context-manager-mode-map 'normal)))

;;; Display

(defun gptel-context-manager-refresh ()
  "Refresh the context manager list from the target buffer."
  (interactive)
  (unless (buffer-live-p gptel-context-manager--target-buffer)
    (error "Target buffer for context manager is dead"))
  (let* ((entries nil)
         (ctx-alist (buffer-local-value 'gptel-context--alist gptel-context-manager--target-buffer))
         (roots (buffer-local-value 'gptel-context-manager-roots gptel-context-manager--target-buffer))
         (featured-root (car roots))
         (more-roots (if (> (length roots) 1) (format " (+%d more)" (1- (length roots))) ""))
         (display-alist (if gptel-context-manager-reverse-display (reverse ctx-alist) ctx-alist)))
    
    (if (null display-alist)
        (push (list nil (vector "" "" (propertize "No context items. Use a/B to add." 'face 'gptel-context-manager-empty-face))) entries)
      (dolist (item display-alist)
        (let* ((source (car item))
               (type (cond ((bufferp source) "buffer")
                           ((stringp source) "file")
                           (t "unknown")))
               (name (cond ((bufferp source) (buffer-name source))
                           ((stringp source) (file-name-nondirectory source))
                           (t (format "%s" source))))
               (name-face (if (string= type "file") 'gptel-context-manager-file-face 'gptel-context-manager-buffer-face)))
          (push (list source (vector " " 
                                     (propertize type 'face 'gptel-context-manager-type-face) 
                                     (propertize name 'face name-face))) entries))))
    
    (setq tabulated-list-entries (nreverse entries))
    (setq header-line-format
          (concat (propertize (format " Context for: %s " (buffer-name gptel-context-manager--target-buffer)) 'face 'gptel-context-manager-header-face)
                  (format "| %d items " (length ctx-alist))
                  (when featured-root
                    (concat "| Root: " (propertize featured-root 'face 'gptel-context-manager-root-face) more-roots))))
    (tabulated-list-print t t)))

;;; Actions & Navigation

(defun gptel-context-manager-visit ()
  "Visit the entry at point in another window."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (error "No entry at point"))
    (cond
     ((bufferp id) (pop-to-buffer id))
     ((stringp id) (find-file-other-window id))
     ((and (consp id) (eq (car id) 'file)) (find-file-other-window (plist-get (cdr id) :file)))
     (t (error "Unknown context type")))))

(defun gptel-context-manager-mark ()
  "Mark the current entry for deletion."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (tabulated-list-put-tag (propertize (string gptel-context-manager-mark-char) 'face 'gptel-context-manager-mark-face) t))))

(defun gptel-context-manager-unmark ()
  "Unmark the current entry."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (tabulated-list-put-tag " " t))))

(defun gptel-context-manager-unmark-all ()
  "Unmark all entries."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (tabulated-list-get-id)
        (tabulated-list-put-tag " " t))
      (forward-line 1))))

(defun gptel-context-manager-toggle-mark ()
  "Toggle the mark on the current entry."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (if (string-match-p (regexp-quote (string gptel-context-manager-mark-char))
                          (aref (tabulated-list-get-entry) 0))
          (gptel-context-manager-unmark)
        (gptel-context-manager-mark)))))

(defun gptel-context-manager-mark-regexp (regexp)
  "Mark entries matching REGEXP."
  (interactive "sMark regexp: ")
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((entry (tabulated-list-get-entry)))
        (when (and entry (string-match-p regexp (aref entry 2)))
          (tabulated-list-put-tag (propertize (string gptel-context-manager-mark-char) 'face 'gptel-context-manager-mark-face) t)))
      (forward-line 1))))

(defun gptel-context-manager-execute ()
  "Delete marked entries from the context."
  (interactive)
  (let ((to-delete nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((mark (aref (tabulated-list-get-entry) 0)))
          (when (string-match-p (regexp-quote (string gptel-context-manager-mark-char)) mark)
            (push (tabulated-list-get-id) to-delete)))
        (forward-line 1)))
    (when to-delete
      (with-current-buffer gptel-context-manager--target-buffer
        (setq gptel-context--alist
              (cl-remove-if (lambda (item) (member (car item) to-delete))
                            gptel-context--alist)))
      (gptel-context-manager-refresh))))

(defun gptel-context-manager-delete-at-point ()
  "Delete the entry at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (error "No entry at point"))
    (when (y-or-n-p "Delete entry? ")
      (with-current-buffer gptel-context-manager--target-buffer
        (setq gptel-context--alist
              (cl-remove-if (lambda (item) (equal (car item) id)) gptel-context--alist)))
      (gptel-context-manager-refresh))))

;; Reordering

(defun gptel-context-manager-move-up (&optional arg)
  "Move the current entry up by ARG positions."
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
  "Move the current entry down by ARG positions."
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

;;; Add Commands

(defun gptel-context-manager-add-file (file)
  "Add FILE to the context."
  (interactive
   (let* ((roots (buffer-local-value 'gptel-context-manager-roots gptel-context-manager--target-buffer))
          (default-directory (or (car roots) default-directory)))
     (list (read-file-name "Add file to context: "))))
  (with-current-buffer gptel-context-manager--target-buffer
    (gptel-context-add-file file))
  (gptel-context-manager-refresh))

(defun gptel-context-manager-add-buffer (buffer)
  "Add BUFFER to the context."
  (interactive
   (list (read-buffer "Add buffer to context: " nil t)))
  (let ((buf (get-buffer buffer)))
    (unless (buffer-live-p buf) (error "Buffer is dead"))
    (with-current-buffer gptel-context-manager--target-buffer
      (gptel-context--add-buffer buf)))
  (gptel-context-manager-refresh))

;; Region Selection Mode
(defvar-local gptel-context-manager-region-selection-mode nil)

(define-minor-mode gptel-context-manager-region-selection-mode
  "Temporary minor mode for region selection."
  :lighter " Gptel-Region"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-;") #'gptel-context-manager-region-selection-confirm)
            (define-key map (kbd "C-c C-,") #'gptel-context-manager-region-selection-cancel)
            map)
  (if gptel-context-manager-region-selection-mode
      (setq header-line-format " Select region, then C-c C-; to confirm or C-c C-, to cancel ")
    (setq header-line-format nil)))

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

(defun gptel-context-manager-region-selection-cancel ()
  "Cancel region selection."
  (interactive)
  (gptel-context-manager-region-selection-mode -1)
  (deactivate-mark)
  (gptel-context-manager-toggle))

;;; Project Roots

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

;;; State Persistence

(defun gptel-context-manager--serialize-state ()
  "Serialize context and roots for persistence."
  (let ((ctx-alist (buffer-local-value 'gptel-context--alist gptel-context-manager--target-buffer))
        (roots (buffer-local-value 'gptel-context-manager-roots gptel-context-manager--target-buffer))
        (serializable-ctx nil))
    (dolist (item ctx-alist)
      (let ((source (car item))
            (overlays (cdr item)))
        (cond
         ((stringp source) (push (list :file source) serializable-ctx))
         ((bufferp source)
          (let ((file (buffer-file-name source)))
            (when file
              (if (not overlays)
                  (push (list :file file :type 'buffer) serializable-ctx)
                (dolist (ov overlays)
                  (push (list :file file :lines (list (line-number-at-pos (overlay-start ov))
                                                      (line-number-at-pos (overlay-end ov))))
                        serializable-ctx)))))))))
    `((roots . ,roots) (context . ,(nreverse serializable-ctx)))))

(defun gptel-context-manager-save-state (&optional arg)
  "Save context state. With prefix ARG, add file-local variable."
  (interactive "P")
  (let* ((state (gptel-context-manager--serialize-state))
         (ctx-str (prin1-to-string (alist-get 'context state)))
         (roots-str (prin1-to-string (alist-get 'roots state))))
    (with-current-buffer gptel-context-manager--target-buffer
      (if (derived-mode-p 'org-mode)
          (let ((org-inhibit-logging t))
            (when (fboundp 'org-entry-put)
              (org-entry-put nil "GPTEL_CONTEXT" ctx-str)
              (org-entry-put nil "GPTEL_PROJECT_ROOTS" roots-str)
              (message "Saved state to Org properties.")))
        (setq-local gptel-context-manager-state state)
        (when arg
          (add-file-local-variable 'gptel-context-manager-state state)
          (save-buffer))
        (message "Saved state to buffer locals.")))))

(defun gptel-context-manager-load-state ()
  "Load context state from Org properties or buffer locals."
  (interactive)
  (with-current-buffer gptel-context-manager--target-buffer
    (let ((state nil))
      (if (derived-mode-p 'org-mode)
          (when (fboundp 'org-entry-get)
            (let ((ctx-prop (org-entry-get nil "GPTEL_CONTEXT"))
                  (roots-prop (org-entry-get nil "GPTEL_PROJECT_ROOTS")))
              (when ctx-prop
                (setq state `((context . ,(car (read-from-string ctx-prop))))))
              (when roots-prop
                (setf (alist-get 'roots state) (car (read-from-string roots-prop))))))
        (setq state gptel-context-manager-state))
      
      (when state
        (setq gptel-context-manager-roots (alist-get 'roots state))
        (let ((ctx-data (alist-get 'context state)))
          (setq gptel-context--alist nil)
          (dolist (item ctx-data)
            (let ((file (plist-get item :file))
                  (lines (plist-get item :lines)))
              (when (and file (file-exists-p file))
                (if lines
                    (with-current-buffer (find-file-noselect file)
                      (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- (car lines)))
                        (let ((start (point)))
                          (goto-char (point-min))
                          (forward-line (1- (cadr lines)))
                          (gptel-context--add-region start (point)))))
                  (gptel-context-add-file file))))))
        (message "Loaded state."))))
  (when (derived-mode-p 'gptel-context-manager-mode)
    (gptel-context-manager-refresh)))

;;; Transient Menu

(transient-define-prefix gptel-context-manager-help ()
  "Help menu for gptel context manager."
  [["Navigation"
    ("g" "Refresh" gptel-context-manager-refresh)
    ("RET" "Visit entry" gptel-context-manager-visit)
    ("M-p" "Move up" gptel-context-manager-move-up)
    ("M-n" "Move down" gptel-context-manager-move-down)
    ("q" "Quit" gptel-context-manager-toggle)]
   ["Mark / Delete"
    ("d" "Mark" gptel-context-manager-mark)
    ("u" "Unmark" gptel-context-manager-unmark)
    ("U" "Unmark all" gptel-context-manager-unmark-all)
    ("t" "Toggle mark" gptel-context-manager-toggle-mark)
    ("%" "Mark regexp" gptel-context-manager-mark-regexp)
    ("x" "Execute delete" gptel-context-manager-execute)
    ("D" "Delete at point" gptel-context-manager-delete-at-point)]
   ["Add"
    ("a" "Add file" gptel-context-manager-add-file)
    ("B" "Add buffer" gptel-context-manager-add-buffer)
    ("r" "Add region" gptel-context-manager-add-region)]
   ["Roots & State"
    ("R" "Add root" gptel-context-manager-add-root)
    ("F" "Feature root" gptel-context-manager-feature-root)
    ("X" "Remove root" gptel-context-manager-remove-root)
    ("S" "Save state" gptel-context-manager-save-state)
    ("L" "Load state" gptel-context-manager-load-state)]])

;;; Main Toggle

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

(provide 'gptel-context-manager)
;;; gptel-context-manager.el ends here
