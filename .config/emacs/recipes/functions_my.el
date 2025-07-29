;;; functions_my.el --- File for custom code and functions

;;; Code:

;; Moving
;;;###autoload
(defun vd/move-line-up ()
  "Move line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

;;;###autoload
(defun vd/move-line-down ()
  "Move line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;;;###autoload
(defun vd/copy-line (arg)
  "Copy lines (as many as prefix ARG) in the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;;;###autoload
(defun vd/get-point (symbol &optional arg)
  "Copy word current SYMBOL belongs.  With additional ARG."
  (funcall symbol arg)
  (point)
  )

;;;###autoload
(defun vd/copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between BEGIN-OF-THING & END-OF-THING into kill ring.
With optional ARG."
  (save-excursion
    (let ((beg (vd/get-point begin-of-thing 1))
          (end (vd/get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

;;;###autoload
(defun vd/copy-word (&optional arg)
  "Copy words at point into KILL-RING.  With optional ARG."
  (interactive "P")
  (vd/copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

;;;###autoload
(defun vd/delete-line ()
  "Delete text from begin to end of line char."
  (interactive)
  (kill-region
   (move-beginning-of-line 1)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  )

;;;###autoload
(defun vd/revert-buffer ()
  "Revert buffer."
  (interactive)
  (revert-buffer t t)
  )


(defvar newline-and-indent t)

;;;###autoload
(defun vd/open-next-line ()
  "Open new line (vi's o command)."
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;;;###autoload
(defun vd/open-previous-line (arg)
  "Open a new line before the current one.
See also `newline-and-indent'.  Behave like vi's O command.  With optional ARG."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;;;###autoload
(defun vd/save-all-buffers ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t)
)

;;;###autoload
(defun vd/kill-emacs-with-save ()
  "Kill terminal."
  (interactive)
  (save-buffers-kill-terminal "y")
)

;;;###autoload
(defun vd/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
;; C-x r y for paste multiple cursors

;;;###autoload
(defun vd/find-in-directory (&optional dir)
  "Find files in DIR."
  (interactive)
  (let ((dir (or dir "~")))
        (setq default-directory dir)
        (call-interactively 'find-file)
    )
)

;;;###autoload
(defun vd/find-in-config ()
  "Find in config directory."
  (interactive)
  (vd/find-in-directory "~/.config/")
)

;;;###autoload
(defun vd/delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation."
  (interactive
  (list (buffer-file-name (buffer-base-buffer))
        current-prefix-arg))
    (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          (kill-buffer buf)
          (message "Deleted %S" short-path))))))

;;;###autoload
(defun vd/highlight-todos ()
    "Add custom highlighting for specific words."
    (font-lock-add-keywords nil
                            '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t))))

;;;###autoload
(defun vd/highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
      file))

;;;###autoload
(defun vd/highlight-enabled-mode (cmd)
    "If MODE (CMD) is enabled, highlight it as `font-lock-constant-face`."
    (let ((sym (intern cmd)))
      (if (or (eq sym major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)))
          (propertize cmd 'face 'font-lock-constant-face)
        cmd)))

;;;###autoload
(defun vd/minibuffer-history ()
  "Make minibuffer-insert function result add to the minibuffer history."
  (unless (eq minibuffer-history-variable t)
    (add-to-history minibuffer-history-variable (minibuffer-contents))))

;;;###autoload
(defun vd/ollama-list-remote-models (ollama-api-endpoint)
  "List Ollama model names from a local or remote Ollama REST API.
This version is corrected to handle string-wrapped JSON responses
and uses modern JSON parsing conventions, with added robustness
for different JSON parser configurations (hash-table vs. alist)."
  ;; The 's' in interactive makes it prompt for a string directly.
  (interactive "sOllama API Endpoint (e.g., http://localhost:11434): ")
  ;; Ensure required libraries are loaded.
  (require 'json)
  (require 'url)
  (require 'cl-lib)
  (let* ((url (or ollama-api-endpoint "http://localhost:11434"))
         ;; Construct the full URL for the /api/tags endpoint.
         (full-url (format "%s/api/tags" url))
         ;; Use with-temp-buffer for automatic cleanup, which is cleaner
         ;; than unwind-protect and manual kill-buffer.
         (model-names
          (with-temp-buffer
            (message "Fetching Ollama models from %s..." full-url)
            ;; Use `url-insert-file-contents`. A non-nil return value indicates success.
            (if (condition-case-unless-debug err
                    (url-insert-file-contents full-url)
                  ;; Catch potential errors during the HTTP request.
                  (error (message "Error fetching URL: %s" err) nil))
                ;; --- BEGIN SUCCESSFUL FETCH ---
                (let* ((raw-response (buffer-string))
                       (json-data nil))
                  ;; Try to parse the raw response. The API might return a JSON object
                  ;; directly, or it might wrap the JSON object inside a JSON string.
                  (condition-case err
                      (setq json-data (json-read-from-string raw-response))
                    (error
                     (message "Error parsing initial JSON response: %s" err)
                     (setq json-data nil)))

                  ;; If the first parse resulted in a string, it means the JSON was
                  ;; wrapped. We need to parse this inner string to get the object.
                  (when (stringp json-data)
                    (condition-case err
                        (setq json-data (json-read-from-string json-data))
                      (error
                       (message "Error parsing unwrapped JSON string: %s" err)
                       (setq json-data nil))))

                  ;; Now, `json-data` should be a parsed JSON structure.
                  ;; Process it, handling either hash-table or alist representations.
                  (cond
                   ;; CASE 1: Parsed as a hash-table (e.g., modern json.el default).
                   ((hash-table-p json-data)
                    (let ((models (gethash "models" json-data)))
                      (if (vectorp models)
                          (let ((names (mapcar (lambda (model)
                                                 (when (hash-table-p model)
                                                   (gethash "name" model)))
                                               (cl-coerce models 'list))))
                            (message "Successfully fetched %d models." (length (cl-remove-if-not #'stringp names)))
                            (cl-remove-if-not #'stringp names)) ; Return names
                        (message "Ollama API response format unexpected ('models' key not a vector). Response: %S" json-data))))

                   ;; CASE 2: Parsed as an association list (alist).
                   ((and (listp json-data) (assoc 'models json-data))
                    (let* ((models (cdr (assoc 'models json-data)))
                           ;; The array part could be a vector or a list.
                           (model-list (if (vectorp models) (cl-coerce models 'list) models)))
                      (if (listp model-list)
                          (let ((names (mapcar (lambda (model)
                                                 (when (listp model) ; Should be an alist
                                                   (cdr (assoc 'name model))))
                                               model-list)))
                            (message "Successfully fetched %d models." (length (cl-remove-if-not #'stringp names)))
                            (cl-remove-if-not #'stringp names)) ; Return names
                        (message "Ollama API response format unexpected ('models' value not a list/vector). Response: %S" json-data))))

                   ;; DEFAULT: Unrecognized format.
                   (t
                    (message "Ollama API response was not a valid or recognized JSON object. Final parsed data: %S" json-data)
                    nil))) ; Return nil
              ;; --- END SUCCESSFUL FETCH ---

              ;; --- BEGIN FAILED FETCH ---
              (message "Failed to retrieve data from Ollama API at %s. Check URL and server." full-url)))))
    ;; The value of the `with-temp-buffer` block is the list of names (or nil on failure).
    ;; Return this value.
    model-names))


;;;###autoload
(defun vd/ollama-list-installed-models ()
  "Return the installed models"
  (let* ((ret (shell-command-to-string "ollama list"))
         (models (cdr (string-lines ret))))
    (if (and (string-match-p "NAME[[:space:]]*ID[[:space:]]*SIZE[[:space:]]*MODIFIED" ret) (length> models 0))
        (mapcar (lambda (m) (car (string-split m))) models)
      (message "Cannot detect installed models, please make sure Ollama server is started"))))

(provide 'functions_my)
;;; Commentary:
;;
;;; functions_my.el ends here
