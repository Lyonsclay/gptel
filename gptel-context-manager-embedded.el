;;; gptel-context-manager-embedded.el --- Embedded context manager for gptel -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Embedded context manager for gptel that opens in the current window and
;; operates on the originating conversation buffer without touching gptel core.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'gptel-context)
(require 'transient)

(eval-when-compile
  (require 'evil-core nil t))

(defgroup gptel-context-manager-embedded nil
  "Embedded context manager for gptel."
  :group 'gptel)

(defcustom gptel-context-manager-display-reversed nil
  "When non-nil, display context entries in reverse order."
  :type 'boolean
  :group 'gptel-context-manager-embedded)

(defcustom gptel-context-manager-help-show-popup t
  "Whether to force transient help to show immediately."
  :type 'boolean
  :group 'gptel-context-manager-embedded)

(defconst gptel-context-manager-org-context-property "GPTEL_CONTEXT"
  "Org property used to persist serialized context entries.")

(defconst gptel-context-manager-org-roots-property "GPTEL_PROJECT_ROOTS"
  "Org property used to persist manager project roots.")

(defvar-local gptel-context-manager--target nil
  "Target conversation buffer for this manager buffer.")

(defvar-local gptel-context-manager--manager-buffer nil
  "Manager buffer associated with a conversation buffer.")

(defvar-local gptel-context-manager--window-config nil
  "Window configuration saved before showing the manager.")

(defvar-local gptel-context-manager-roots nil
  "List of project roots tracked by the embedded manager.")

(defvar-local gptel-context-manager-state nil
  "Last collected state plist for the embedded manager.")

(defvar gptel-context-manager--pending-region nil)
(defvar-local gptel-context-manager--saved-header-line nil)

;;; Faces

(defface gptel-context-manager-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the header portion of the context manager.")

(defface gptel-context-manager-target-buffer-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the name of the buffer being managed.")

(defface gptel-context-manager-root-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face for project roots displayed in the header.")

(defface gptel-context-manager-mark-face
  '((t :inherit dired-mark))
  "Face for the deletion mark column.")

(defface gptel-context-manager-marked-face
  '((t :inherit dired-marked))
  "Face for rows that are currently marked.")

(defface gptel-context-manager-type-buffer-face
  '((t :inherit font-lock-type-face))
  "Face for buffer/region type entries.")

(defface gptel-context-manager-type-file-face
  '((t :inherit font-lock-string-face))
  "Face for file entries.")

(defface gptel-context-manager-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for the context entry name column.")

(defface gptel-context-manager-details-face
  '((t :inherit font-lock-comment-face))
  "Face for auxiliary details.")

(defface gptel-context-manager-empty-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face used when there are no context items.")

(defun gptel-context-manager--mark-string ()
  "Return the styled deletion mark string."
  (propertize "D" 'face 'gptel-context-manager-mark-face))

(defmacro gptel-context-manager--with-target (&rest body)
  "Evaluate BODY in the originating conversation buffer.

Falls back to the current buffer when the target is not set or dead."
  `(let* ((buf (or gptel-context-manager--target (current-buffer)))
          (buf (if (buffer-live-p buf) buf (current-buffer))))
     (with-current-buffer buf ,@body)))

(defun gptel-context-manager--target-buffer ()
  "Return the current target buffer or signal if it is dead."
  (let ((buf gptel-context-manager--target))
    (unless (buffer-live-p buf)
      (user-error "Conversation buffer no longer exists"))
    buf))

(defun gptel-context-manager--with-shared-context (buffer fn)
  "Run FN inside BUFFER while sharing the target buffer's context list.
Return the result of FN."
  (unless (buffer-live-p buffer)
    (user-error "Buffer %s no longer exists" (buffer-name buffer)))
  (gptel-context-manager--with-target
    (let ((ctx gptel-context)
          result)
      (with-current-buffer buffer
        (let ((gptel-context ctx))
          (setq result (funcall fn))
          (setq ctx gptel-context)))
      (setq gptel-context ctx)
      result)))

(defun gptel-context-manager--featured-root ()
  "Return the primary project root for the target buffer, if any."
  (car gptel-context-manager-roots))

(defun gptel-context-manager--featured-root-path ()
  "Return the featured root for the target buffer, even when called from manager."
  (when (buffer-live-p gptel-context-manager--target)
    (with-current-buffer gptel-context-manager--target
      (gptel-context-manager--featured-root))))

(defun gptel-context-manager--roots-readable ()
  (mapcar #'abbreviate-file-name gptel-context-manager-roots))

(defun gptel-context-manager--ensure-target-state (buffer)
  "Ensure BUFFER has the local variables used by the manager."
  (with-current-buffer buffer
    (unless (local-variable-p 'gptel-context-manager-roots)
      (setq-local gptel-context-manager-roots nil))
    (unless (local-variable-p 'gptel-context-manager-state)
      (setq-local gptel-context-manager-state nil))
    (gptel-context-manager--maybe-auto-load-state)))

(defmacro gptel-context-manager--without-org-mark (&rest body)
  "Run BODY without pushing to Org's mark ring."
  `(cl-letf (((symbol-function 'org-mark-ring-push) #'ignore))
     ,@body))

(defun gptel-context-manager--org-store (property value)
  "Store VALUE (or clear it) in PROPERTY at point."
  (gptel-context-manager--without-org-mark
    (if value
        (org-entry-put (point) property (prin1-to-string value))
      (org-entry-delete (point) property))))

(defun gptel-context-manager--serializable-context ()
  "Return a list of context entries that can be serialized.

File-backed buffers and regions are stored as (:file PATH :lines (START END))."
  (let (items)
    (dolist (entry gptel-context)
      (cond
       ((stringp entry) (push entry items))
       ((and (consp entry) (stringp (car entry)))
        (push (copy-sequence entry) items))
       ((bufferp entry)
        (with-current-buffer entry
          (when-let* ((file (buffer-file-name)))
            (push (list :file file
                        :lines (list 1 (line-number-at-pos (point-max))))
                  items))))
       ((and (consp entry) (bufferp (car entry)))
        (let* ((buf (car entry))
               (spec (cdr entry)))
          (with-current-buffer buf
            (when-let* ((file (buffer-file-name)))
              (if-let* ((overlays (plist-get spec :overlays)))
                  (dolist (ov overlays)
                    (when (overlayp ov)
                      (push (list :file file
                                  :lines (list (line-number-at-pos (overlay-start ov))
                                               (line-number-at-pos (overlay-end ov))))
                            items)))
                (push (list :file file
                            :lines (list 1 (line-number-at-pos (point-max))))
                      items))))))))
    (nreverse items)))

(defun gptel-context-manager--state-collect ()
  "Collect the current state plist from the target buffer."
  (list :roots (copy-sequence gptel-context-manager-roots)
        :context (gptel-context-manager--serializable-context)))

(defun gptel-context-manager--state-store-local ()
  "Store the current state in `gptel-context-manager-state'."
  (setq gptel-context-manager-state (gptel-context-manager--state-collect)))

(defun gptel-context-manager--read-value (string)
  "Read STRING and return the resulting Lisp object, or nil on error."
  (condition-case nil
      (read string)
    (error nil)))

(defun gptel-context-manager--state-from-org ()
  "Return state plist stored in Org properties, if any."
  (when (derived-mode-p 'org-mode)
    (when (require 'org nil t)
      (save-excursion
        (gptel-context-manager--without-org-mark
          (let* ((roots-raw (org-entry-get (point) gptel-context-manager-org-roots-property t))
                 (ctx-raw (org-entry-get (point) gptel-context-manager-org-context-property t))
                 (roots-list (when roots-raw (gptel-context-manager--read-value roots-raw)))
                 (context-list (when ctx-raw (gptel-context-manager--read-value ctx-raw))))
            (when (or roots-list context-list)
              (list :roots roots-list :context context-list))))))))

(defun gptel-context-manager--state-find ()
  "Locate a serialized state for the current buffer."
  (or (gptel-context-manager--state-from-org)
      gptel-context-manager-state))

(defun gptel-context-manager--maybe-auto-load-state ()
  "Load state from Org/file-local sources if nothing is set yet."
  (when (null gptel-context-manager-roots)
    (when-let* ((state (gptel-context-manager--state-find)))
      (gptel-context-manager--state-apply state)
      (setq gptel-context-manager-state state))))

(defun gptel-context-manager--state-apply (state)
  "Apply STATE plist to the target buffer."
  (let ((roots (plist-get state :roots))
        (context (plist-get state :context)))
    (when roots (setq gptel-context-manager-roots roots))
    (when context
      (setq gptel-context nil)
      (dolist (entry context)
        (cond
         ((stringp entry) (push entry gptel-context))
         ((and (consp entry) (stringp (car entry)))
          (push entry gptel-context))
         ((and (listp entry) (plist-get entry :file) (plist-get entry :lines))
          (let* ((file (plist-get entry :file))
                 (lines (plist-get entry :lines))
                 (buf (find-file-noselect file)))
            (when (and (consp lines) (numberp (car lines)) (numberp (cadr lines)))
              (gptel-context-manager--add-range
               buf
               (gptel-context-manager--line-position buf (car lines))
               (gptel-context-manager--line-position buf (cadr lines) t)
               t t))))))
      (setq gptel-context (nreverse gptel-context)))))

(defvar gptel-context-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'gptel-context-manager-refresh)
    (define-key map (kbd "d") #'gptel-context-manager-mark-delete)
    (define-key map (kbd "u") #'gptel-context-manager-unmark)
    (define-key map (kbd "U") #'gptel-context-manager-unmark-all)
    (define-key map (kbd "t") #'gptel-context-manager-toggle-mark)
    (define-key map (kbd "x") #'gptel-context-manager-execute)
    (define-key map (kbd "D") #'gptel-context-manager-delete)
    (define-key map (kbd "%") #'gptel-context-manager-mark-regexp)
    (define-key map (kbd "RET") #'gptel-context-manager-visit)
    (define-key map (kbd "a") #'gptel-context-manager-add-file)
    (define-key map (kbd "B") #'gptel-context-manager-add-buffer)
    (define-key map (kbd "r") #'gptel-context-manager-add-region)
    (define-key map (kbd "R") #'gptel-context-manager-add-root)
    (define-key map (kbd "F") #'gptel-context-manager-feature-root)
    (define-key map (kbd "X") #'gptel-context-manager-remove-root)
    (define-key map (kbd "S") #'gptel-context-manager-save-state)
    (define-key map (kbd "L") #'gptel-context-manager-load-state)
    (define-key map (kbd "M-p") #'gptel-context-manager-move-up)
    (define-key map (kbd "M-n") #'gptel-context-manager-move-down)
    (define-key map (kbd "q") #'gptel-context-manager-quit)
    (define-key map (kbd "?") #'gptel-context-manager-help)
    map)
  "Keymap for the embedded gptel context manager.")

(declare-function evil-define-key "evil-core")
(declare-function evil-set-initial-state "evil")

(defun gptel-context-manager--setup-evil ()
  "Mirror keybindings into Evil states when available."
  (when (require 'evil-core nil t)
    (evil-set-initial-state 'gptel-context-manager-mode 'normal)
    (evil-define-key 'normal gptel-context-manager-mode-map
      "g" #'gptel-context-manager-refresh
      "d" #'gptel-context-manager-mark-delete
      "u" #'gptel-context-manager-unmark
      "t" #'gptel-context-manager-toggle-mark
      "x" #'gptel-context-manager-execute
      "D" #'gptel-context-manager-delete
      "%" #'gptel-context-manager-mark-regexp
      "a" #'gptel-context-manager-add-file
      "B" #'gptel-context-manager-add-buffer
      "r" #'gptel-context-manager-add-region
      "R" #'gptel-context-manager-add-root
      "F" #'gptel-context-manager-feature-root
      "X" #'gptel-context-manager-remove-root
      "S" #'gptel-context-manager-save-state
      "L" #'gptel-context-manager-load-state
      "M-p" #'gptel-context-manager-move-up
      "M-n" #'gptel-context-manager-move-down
      "q" #'gptel-context-manager-quit
      "?" #'gptel-context-manager-help
      (kbd "RET") #'gptel-context-manager-visit)
    (evil-define-key 'motion gptel-context-manager-mode-map
      "g" #'gptel-context-manager-refresh
      "d" #'gptel-context-manager-mark-delete
      "u" #'gptel-context-manager-unmark
      "t" #'gptel-context-manager-toggle-mark
      "x" #'gptel-context-manager-execute
      "D" #'gptel-context-manager-delete
      "%" #'gptel-context-manager-mark-regexp
      "a" #'gptel-context-manager-add-file
      "B" #'gptel-context-manager-add-buffer
      "r" #'gptel-context-manager-add-region
      "R" #'gptel-context-manager-add-root
      "F" #'gptel-context-manager-feature-root
      "X" #'gptel-context-manager-remove-root
      "S" #'gptel-context-manager-save-state
      "L" #'gptel-context-manager-load-state
      "M-p" #'gptel-context-manager-move-up
      "M-n" #'gptel-context-manager-move-down
      "q" #'gptel-context-manager-quit
      "?" #'gptel-context-manager-help
      (kbd "RET") #'gptel-context-manager-visit)))

(define-derived-mode gptel-context-manager-mode tabulated-list-mode "GPTel-Context"
  "Major mode for inspecting gptel context entries in-line.

\{gptel-context-manager-mode-map}"
  (setq tabulated-list-format [("M" 2 t)
                               ("Type" 8 t)
                               ("Context" 0 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'gptel-context-manager-refresh nil t)
  (add-hook 'kill-buffer-hook #'gptel-context-manager-quit nil t)
  (gptel-context-manager--setup-evil))

(defun gptel-context-manager--active-p (buffer)
  "Return non-nil if a manager is currently showing for BUFFER."
  (and (buffer-live-p buffer)
       (buffer-local-value 'gptel-context-manager--manager-buffer buffer)
       (buffer-live-p (buffer-local-value 'gptel-context-manager--manager-buffer buffer))))

(defun gptel-context-manager--show (buffer)
  "Show the manager for BUFFER." 
  (gptel-context-manager--ensure-target-state buffer)
  (with-current-buffer buffer
    (setq gptel-context-manager--window-config (current-window-configuration)))
  (let* ((buf-name (format "*gptel-context: %s*" (buffer-name buffer)))
         (manager (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq gptel-context-manager--manager-buffer manager))
    (with-current-buffer manager
      (gptel-context-manager-mode)
      (setq-local gptel-context-manager--target buffer))
    (switch-to-buffer manager)
    (gptel-context-manager-refresh)))

(defun gptel-context-manager--hide (buffer)
  "Hide the manager for BUFFER." 
  (let ((manager (buffer-local-value 'gptel-context-manager--manager-buffer buffer))
        (window-config (buffer-local-value 'gptel-context-manager--window-config buffer)))
    (when (buffer-live-p manager)
      (with-current-buffer manager
        (remove-hook 'kill-buffer-hook #'gptel-context-manager-quit t)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer (current-buffer)))))
    (when window-config
      (set-window-configuration window-config))
    (with-current-buffer buffer
      (setq gptel-context-manager--manager-buffer nil)
      (setq gptel-context-manager--window-config nil))))

;;;###autoload
(defun gptel-context-manager-toggle ()
  "Toggle the embedded context manager for the current buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (gptel-context-manager--active-p buffer)
        (gptel-context-manager--hide buffer)
      (gptel-context-manager--show buffer))))

(defun gptel-context-manager-refresh ()
  "Refresh the tabulated list from the target buffer."
  (interactive)
  (let ((target gptel-context-manager--target))
    (if (not (buffer-live-p target))
        (progn
          (setq tabulated-list-entries nil)
          (tabulated-list-print t)
          (setq header-line-format
                (propertize "Original buffer killed" 'face 'error)))
      (let* ((entries (with-current-buffer target (copy-sequence gptel-context)))
             (ordered (if gptel-context-manager-display-reversed
                          (nreverse (copy-sequence entries))
                        entries))
             (rows (if ordered
                       (gptel-context-manager--format ordered
                                                      gptel-context-manager-display-reversed
                                                      (length entries))
                     (list (gptel-context-manager--empty-row)))))
        (setq tabulated-list-entries rows)
        (gptel-context-manager--update-header ordered)
        (tabulated-list-print t)))))

(defun gptel-context-manager--empty-row ()
  (list nil (vector "" "" (propertize "No context items. Use a/B to add."
                                        'face 'gptel-context-manager-empty-face))))

(defun gptel-context-manager--type-face (type)
  (pcase type
    ((or "buffer" "region") 'gptel-context-manager-type-buffer-face)
    ("file" 'gptel-context-manager-type-file-face)
    (_ 'gptel-context-manager-details-face)))

(defun gptel-context-manager--format (entries &optional reversed original-length)
  (cl-loop for entry in entries
           for idx from 0
           for original-index = (cond
                                 ((and reversed original-length)
                                  (- (1- original-length) idx))
                                 (t idx))
           collect (list (list :entry entry :index original-index)
                         (gptel-context-manager--format-entry entry))))

(defun gptel-context-manager--format-entry (entry)
  (let* ((type (gptel-context-manager--type entry))
         (label (gptel-context-manager--label entry))
         (details (gptel-context-manager--details entry))
         (display (if details
                      (concat (propertize label 'face 'gptel-context-manager-name-face)
                              "  "
                              (propertize (format "(%s)" details)
                                          'face 'gptel-context-manager-details-face))
                    (propertize label 'face 'gptel-context-manager-name-face))))
    (vector ""
            (propertize type 'face (gptel-context-manager--type-face type))
            display)))

(defun gptel-context-manager--details (entry)
  (let* ((source (car-safe entry))
         (spec (cdr-safe entry)))
    (cond
     ((and (stringp source) (plist-get spec :mime))
      (format "mime=%s" (plist-get spec :mime)))
     ((and (bufferp source) (plist-get spec :overlays))
      (format "%d overlay(s)" (length (plist-get spec :overlays))))
     ((and (bufferp source) (plist-get spec :lines)) "lines")
     ((and (bufferp source) (plist-get spec :bounds)) "bounds")
     (t nil))))

(defun gptel-context-manager--type (entry)
  (let ((source (car-safe entry)))
    (cond
     ((bufferp source)
      (if (plist-get (cdr-safe entry) :overlays) "region" "buffer"))
     ((stringp source) "file")
     ((bufferp entry) "buffer")
     ((stringp entry) "file")
     (t "unknown"))))

(defun gptel-context-manager--label (entry)
  (let ((source (car-safe entry)))
    (cond
     ((bufferp source) (buffer-name source))
     ((stringp source) (abbreviate-file-name source))
     ((bufferp entry) (buffer-name entry))
     ((stringp entry) (abbreviate-file-name entry))
     (t (format "%S" entry)))))

(defun gptel-context-manager--update-header (entries)
  (let* ((target gptel-context-manager--target)
         (name (if (buffer-live-p target) (buffer-name target) "<dead>"))
         (count (length entries))
         (roots (when (buffer-live-p target)
                  (with-current-buffer target
                    (copy-sequence gptel-context-manager-roots))))
         (primary (when roots (abbreviate-file-name (car roots))))
         (extra (when (> (length roots) 1)
                  (format " (+%d more)" (1- (length roots))))))
    (setq header-line-format
          (list (propertize "Managing " 'face 'gptel-context-manager-header-face)
                (propertize name 'face 'gptel-context-manager-target-buffer-face)
                (propertize (format " • %d item%s" count (if (= count 1) "" "s"))
                            'face 'gptel-context-manager-details-face)
                (propertize "  |  Root: " 'face 'gptel-context-manager-header-face)
                (if primary
                    (propertize primary 'face 'gptel-context-manager-root-face)
                  (propertize "(none)" 'face 'gptel-context-manager-details-face))
                (when extra
                  (propertize extra 'face 'gptel-context-manager-details-face))
                (propertize "  |  q to quit" 'face 'gptel-context-manager-details-face)))))

(defun gptel-context-manager--id-entry (id)
  (and (listp id) (plist-get id :entry)))

(defun gptel-context-manager--id-index (id)
  (and (listp id) (plist-get id :index)))

(defun gptel-context-manager--goto-entry (entry)
  (goto-char (point-min))
  (let (found)
    (while (and (not (eobp)) (not found))
      (let ((id (tabulated-list-get-id)))
        (when (and id (equal (gptel-context-manager--id-entry id) entry))
          (setq found t))
        (unless found (forward-line 1))))
    (unless found
      (goto-char (point-min)))))

(defun gptel-context-manager--list-move (list from to)
  (let* ((len (length list))
         (from (max 0 (min (1- len) from)))
         (to (max 0 (min (1- len) to)))
         (elem (nth from list))
         (head (cl-subseq list 0 from))
         (tail (cl-subseq list (1+ from)))
         (without (append head tail)))
    (append (cl-subseq without 0 to)
            (list elem)
            (cl-subseq without to)))

(defun gptel-context-manager-mark-delete ()
  (interactive)
  (when (gptel-context-manager--id-entry (tabulated-list-get-id))
    (tabulated-list-put-tag (gptel-context-manager--mark-string) t)))

(defun gptel-context-manager-unmark ()
  (interactive)
  (when (gptel-context-manager--id-entry (tabulated-list-get-id))
    (tabulated-list-put-tag "" t)))

(defun gptel-context-manager-unmark-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag "")
      (forward-line 1)))
  (message "All marks removed"))

(defun gptel-context-manager-toggle-mark ()
  (interactive)
  (when (gptel-context-manager--id-entry (tabulated-list-get-id))
    (let ((marked (eq (char-after (line-beginning-position)) ?D)))
      (tabulated-list-put-tag (if marked "" (gptel-context-manager--mark-string)) t))))

(defun gptel-context-manager-mark-regexp (regexp)
  (interactive "sMark items matching regexp: ")
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((id (tabulated-list-get-id))
                    (entry (gptel-context-manager--id-entry id))
                    (label (gptel-context-manager--label entry)))
          (when (string-match-p regexp label)
            (setq count (1+ count))
            (tabulated-list-put-tag (gptel-context-manager--mark-string) t)))
        (forward-line 1)))
    (message "Marked %d item(s)" count)))

(defun gptel-context-manager-delete ()
  (interactive)
  (when-let* ((entry (gptel-context-manager--id-entry (tabulated-list-get-id))))
    (when (y-or-n-p "Remove this context item? ")
      (gptel-context-manager--remove entry)
      (gptel-context-manager-refresh))))

(defun gptel-context-manager-execute ()
  (interactive)
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((id (tabulated-list-get-id)))
          (when (and (eq (char-after (line-beginning-position)) ?D)
                     (gptel-context-manager--id-entry id))
            (push (gptel-context-manager--id-entry id) items)))
        (forward-line 1)))
    (if (null items)
        (message "No marks to delete")
      (when (y-or-n-p (format "Remove %d item(s)? " (length items)))
        (mapc #'gptel-context-manager--remove items)
        (gptel-context-manager-refresh)
        (message "Removed %d item(s)" (length items))))))

(defun gptel-context-manager--remove (entry)
  (gptel-context-manager--with-target
   (let ((source (car-safe entry))
         (spec (cdr-safe entry)))
     (cond
      ((and (bufferp source) (plist-get spec :overlays))
       (mapc #'gptel-context-remove (plist-get spec :overlays)))
      ((bufferp source) (gptel-context-remove source))
      ((stringp source) (gptel-context-remove source))
      ((bufferp entry) (gptel-context-remove entry))
      ((stringp entry) (gptel-context-remove entry))
      (t (message "Don't know how to remove %S" entry))))))

(defun gptel-context-manager-visit ()
  (interactive)
  (let ((entry (gptel-context-manager--id-entry (tabulated-list-get-id))))
    (unless entry (user-error "No context at point"))
    (gptel-context-manager--with-target
     (let ((source (car-safe entry)))
       (cond
        ((bufferp source)
         (switch-to-buffer-other-window source))
        ((stringp source)
         (if (file-exists-p source)
             (find-file-other-window source)
           (message "File missing: %s" source)))
        ((bufferp entry) (switch-to-buffer-other-window entry))
        ((stringp entry)
         (if (file-exists-p entry)
             (find-file-other-window entry)
           (message "File missing: %s" entry)))
        (t (message "Cannot visit this item"))))))

(defun gptel-context-manager-add-file ()
  (interactive)
  (gptel-context-manager--with-target
   (let ((default-directory (or (gptel-context-manager--featured-root-path)
                                default-directory)))
     (call-interactively #'gptel-context-add-file)))
  (gptel-context-manager-refresh))

(defun gptel-context-manager-add-buffer ()
  (interactive)
  (let* ((buf-name (read-buffer "Add buffer: " (buffer-name (gptel-context-manager--target-buffer)) t))
         (buf (get-buffer buf-name)))
    (unless (buffer-live-p buf)
      (user-error "Buffer %s no longer exists" buf-name))
    (gptel-context-manager--with-target
     (cl-pushnew (list buf) gptel-context :test #'equal)
     (gptel-context-manager--state-store-local))
    (gptel-context-manager-refresh)
    (message "Buffer %s added to context." buf-name)))

(defvar gptel-context-manager-region-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-;") #'gptel-context-manager--confirm-region)
    (define-key map (kbd "C-c C-,") #'gptel-context-manager--cancel-region)
    map)
  "Keymap for temporary region selection mode.")

(define-minor-mode gptel-context-manager-region-mode
  "Minor mode active while selecting a region for the context manager."
  :init-value nil
  :lighter " CtxSel"
  :keymap gptel-context-manager-region-mode-map)

(defun gptel-context-manager--select-region (buffer)
  "Switch to BUFFER and prompt for a region, returning to the manager on confirm."
  (let ((manager (current-buffer))
        (window-config (current-window-configuration)))
    (setq gptel-context-manager--pending-region
          (list :buffer buffer
                :manager manager
                :window-config window-config))
    (switch-to-buffer buffer)
    (setq gptel-context-manager--saved-header-line header-line-format)
    (setq header-line-format
          (propertize "Select a region. C-c C-; to add, C-c C-, to cancel." 'face 'shadow))
    (gptel-context-manager-region-mode 1)
    (recursive-edit)))

(defun gptel-context-manager--confirm-region ()
  "Confirm the region selection and add it to context."
  (interactive)
  (unless (and gptel-context-manager--pending-region (use-region-p))
    (user-error "No active region to add"))
  (let* ((pending gptel-context-manager--pending-region)
         (buffer (plist-get pending :buffer))
         (manager (plist-get pending :manager))
         (window-config (plist-get pending :window-config))
         (start (region-beginning))
         (end (region-end)))
    (setq gptel-context-manager--pending-region nil)
    (setq header-line-format gptel-context-manager--saved-header-line)
    (setq gptel-context-manager--saved-header-line nil)
    (gptel-context-manager-region-mode -1)
    (with-current-buffer manager
      (gptel-context-manager--add-range buffer start end t)
      (gptel-context-manager-refresh))
    (when window-config
      (set-window-configuration window-config)))
  (exit-recursive-edit))

(defun gptel-context-manager--cancel-region ()
  "Cancel an in-progress region selection."
  (interactive)
  (when gptel-context-manager--pending-region
    (let ((window-config (plist-get gptel-context-manager--pending-region :window-config)))
      (setq gptel-context-manager--pending-region nil)
      (setq header-line-format gptel-context-manager--saved-header-line)
      (setq gptel-context-manager--saved-header-line nil)
      (gptel-context-manager-region-mode -1)
      (when window-config
        (set-window-configuration window-config))))
  (exit-recursive-edit))

(defun gptel-context-manager--read-region-source ()
  (let* ((choice (completing-read "Add region from: " '("buffer" "file") nil t nil nil "buffer")))
    (if (string= choice "file")
        (let* ((root (or (gptel-context-manager--featured-root-path) default-directory))
               (file (read-file-name "File: " root nil t)))
          (find-file-noselect file))
      (let ((buf (get-buffer (read-buffer "Buffer: " (buffer-name (gptel-context-manager--target-buffer)) t))))
        (unless (buffer-live-p buf)
          (user-error "Buffer no longer exists"))
        buf))))

(defun gptel-context-manager--region-bounds (buffer)
  (with-current-buffer buffer
    (when (use-region-p)
      (list (region-beginning) (region-end)))))

(defun gptel-context-manager--read-region-lines (buffer)
  (with-current-buffer buffer
    (let* ((max-line (max 1 (line-number-at-pos (point-max))))
           (start (read-number (format "Start line (1-%d): " max-line) 1))
           (end (read-number (format "End line (>= %d, <= %d): " start max-line) max-line)))
      (when (> start end)
        (user-error "Start must be <= end"))
      (list start end))))

(defun gptel-context-manager--line-position (buffer line &optional end)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (if end (line-end-position) (line-beginning-position)))))

(defun gptel-context-manager--add-range (buffer start-pos end-pos &optional advance skip-state)
  (gptel-context-manager--with-target
   (mapc #'gptel-context-remove (gptel-context--in-region buffer start-pos end-pos)))
  (gptel-context-manager--with-shared-context
   buffer
   (lambda ()
     (gptel-context--make-overlay start-pos end-pos advance)))
  (unless skip-state
    (gptel-context-manager--with-target
     (gptel-context-manager--state-store-local))))

(defun gptel-context-manager-add-region ()
  "Add a region defined by an active selection or line numbers."
  (interactive)
  (let* ((buffer (gptel-context-manager--read-region-source))
         (bounds (gptel-context-manager--region-bounds buffer))
         (start-pos nil)
         (end-pos nil)
         (start-line nil)
         (end-line nil))
    (cond
     (bounds
      (setq start-pos (nth 0 bounds)
            end-pos (nth 1 bounds)
            start-line (with-current-buffer buffer (line-number-at-pos start-pos))
            end-line (with-current-buffer buffer (line-number-at-pos end-pos))))
     ((y-or-n-p "No region active. Select a region now? ")
      (gptel-context-manager--select-region buffer)
      (setq buffer nil))
     (t
      (let* ((lines (gptel-context-manager--read-region-lines buffer)))
        (setq start-line (nth 0 lines)
              end-line (nth 1 lines)
              start-pos (gptel-context-manager--line-position buffer start-line)
              end-pos (gptel-context-manager--line-position buffer end-line t)))))
    (when buffer
      (gptel-context-manager--add-range buffer start-pos end-pos t)
      (gptel-context-manager-refresh)
      (message "Added lines %d-%d from %s" start-line end-line (buffer-name buffer)))))

(defun gptel-context-manager-add-root (dir)
  "Add DIR (prompted) as a project root for this buffer."
  (interactive "DProject root: ")
  (gptel-context-manager--with-target
   (let* ((path (file-name-as-directory (expand-file-name dir)))
          (new (cl-remove-duplicates
                (append gptel-context-manager-roots (list path))
                :test #'string-equal)))
     (setq gptel-context-manager-roots new)
     (gptel-context-manager--state-store-local)))
  (gptel-context-manager-refresh)
  (message "Added project root."))

(defun gptel-context-manager-feature-root ()
  "Promote a root to the top of the list."
  (interactive)
  (gptel-context-manager--with-target
   (unless gptel-context-manager-roots
     (user-error "No project roots defined"))
   (let* ((choices gptel-context-manager-roots)
          (selection (completing-read "Feature root: " choices nil t nil nil (car choices))))
     (setq gptel-context-manager-roots
           (cons selection
                 (cl-remove selection choices :test #'string-equal)))
     (gptel-context-manager--state-store-local)))
  (gptel-context-manager-refresh)
  (message "Root promoted."))

(defun gptel-context-manager-remove-root ()
  "Remove a project root from the list."
  (interactive)
  (gptel-context-manager--with-target
   (unless gptel-context-manager-roots
     (user-error "No project roots defined"))
   (let* ((choices gptel-context-manager-roots)
          (selection (completing-read "Remove root: " choices nil t nil nil (car choices)))
          (remainder (cl-remove selection choices :test #'string-equal)))
     (setq gptel-context-manager-roots remainder)
     (gptel-context-manager--state-store-local)))
  (gptel-context-manager-refresh)
  (message "Removed project root."))

(defun gptel-context-manager-save-state (&optional arg)
  "Persist project roots and context. With ARG, add file-local variable."
  (interactive "P")
  (gptel-context-manager--with-target
   (let* ((total (length gptel-context))
          (state (gptel-context-manager--state-collect))
          (roots (plist-get state :roots))
          (context (plist-get state :context))
          (saved (length context))
          (skipped (max 0 (- total saved))))
     (cond
      ((derived-mode-p 'org-mode)
       (unless (require 'org nil t)
         (user-error "Org is required to store state in properties"))
       (save-excursion
         (gptel-context-manager--org-store gptel-context-manager-org-roots-property roots)
         (gptel-context-manager--org-store gptel-context-manager-org-context-property context))
       (message "Saved roots (%d) and context (%d) to Org properties%s." (length roots) saved
                (if (> skipped 0) (format " (skipped %d buffer entries)" skipped) "")))
      (t
       (setq gptel-context-manager-state state)
       (when (and arg (buffer-file-name))
         (add-file-local-variable 'gptel-context-manager-state state))
       (message "Saved manager state%s.%s"
                (if arg " (and file-local)" "")
                (if (> skipped 0)
                    (format " Note: skipped %d buffer entries." skipped)
                  ""))))))
  (gptel-context-manager-refresh))

(defun gptel-context-manager-load-state (&optional force-org)
  "Load persisted state. With FORCE-ORG, prefer Org property even if cached."
  (interactive "P")
  (let ((state (gptel-context-manager--with-target
                (or (and force-org (gptel-context-manager--state-from-org))
                    (gptel-context-manager--state-find)))))
    (if state
        (progn
          (gptel-context-manager--with-target
           (gptel-context-manager--state-apply state)
           (gptel-context-manager--state-store-local))
          (gptel-context-manager-refresh)
          (message "Loaded %d roots and %d file contexts."
                   (length (or (plist-get state :roots) '()))
                   (length (or (plist-get state :context) '()))))
      (message "No stored manager state found."))))

(defun gptel-context-manager-move-up ()
  (interactive)
  (gptel-context-manager--move -1))

(defun gptel-context-manager-move-down ()
  (interactive)
  (gptel-context-manager--move 1))

(defun gptel-context-manager--move (delta)
  (let* ((id (tabulated-list-get-id))
         (entry (gptel-context-manager--id-entry id))
         (index (gptel-context-manager--id-index id))
         (delta (if gptel-context-manager-display-reversed (- delta) delta)))
    (unless entry (user-error "No entry at point"))
    (let* ((new-pos (+ index delta))
           (moved
            (gptel-context-manager--with-target
             (let* ((ctx (copy-sequence gptel-context))
                    (len (length ctx)))
               (if (or (< new-pos 0) (>= new-pos len))
                   (progn (message "Cannot move further") nil)
                 (setq gptel-context (gptel-context-manager--list-move ctx index new-pos))
                 (gptel-context-manager--state-store-local)
                 t)))))
      (when moved
        (gptel-context-manager-refresh)
        (gptel-context-manager--goto-entry entry)))))

(transient-define-prefix gptel-context-manager-help-menu ()
  "Show GPTel context manager bindings."
  ["Navigate"
   ("g" "Refresh" gptel-context-manager-refresh :transient t)
   ("RET" "Visit entry" gptel-context-manager-visit :transient t)
   ("q" "Close help" transient-quit-one)
   ("Q" "Quit manager" gptel-context-manager-quit)]
  ["Mark/Delete"
   ("d" "Mark delete" gptel-context-manager-mark-delete :transient t)
   ("u" "Unmark" gptel-context-manager-unmark :transient t)
   ("U" "Unmark all" gptel-context-manager-unmark-all :transient t)
   ("t" "Toggle mark" gptel-context-manager-toggle-mark :transient t)
   ("x" "Delete marked" gptel-context-manager-execute)
   ("D" "Delete at point" gptel-context-manager-delete)
   ("%" "Mark regexp" gptel-context-manager-mark-regexp :transient t)]
  ["Add context"
   ("a" "Add file" gptel-context-manager-add-file)
   ("B" "Add buffer" gptel-context-manager-add-buffer)
   ("r" "Add region" gptel-context-manager-add-region)]
  ["Project roots"
   ("R" "Add root" gptel-context-manager-add-root)
   ("F" "Feature root" gptel-context-manager-feature-root)
   ("X" "Remove root" gptel-context-manager-remove-root)]
  ["State"
   ("S" "Save state" gptel-context-manager-save-state)
   ("L" "Load state" gptel-context-manager-load-state)
   ("M-p" "Move entry up" gptel-context-manager-move-up)
   ("M-n" "Move entry down" gptel-context-manager-move-down)])

(defun gptel-context-manager-help ()
  "Open the transient help menu."
  (interactive)
  (let ((transient-show-popup gptel-context-manager-help-show-popup))
    (gptel-context-manager-help-menu)))

(defun gptel-context-manager-quit ()
  "Quit the manager and restore the previous window configuration."
  (interactive)
  (remove-hook 'kill-buffer-hook #'gptel-context-manager-quit t)
  (let ((target gptel-context-manager--target))
    (when (buffer-live-p target)
      (gptel-context-manager--hide target))))

(with-eval-after-load 'evil
  (gptel-context-manager--setup-evil))

(provide 'gptel-context-manager-embedded)
;;; gptel-context-manager-embedded.el ends here
