;;; gptel-rcp.el --- A simple LLM client for Emacs.
;;; Code:
(eval-when-compile (require 'use-package))
(use-package gptel
  :ensure t

  :custom
  (gptel-default-mode          'org-mode)
  (gptel-temperature           0.4)
  (gptel-use-tools             t)
  (gptel-prompt-prefix-alist    nil)
  (gptel-org-branching-context t)

  :preface
  (defun vd/setup-llama-gptel ()
    "Fill llama models."
    (interactive)
    (require 'gptel)
    (setq gptel-backend (gptel-make-openai "llama-swap"
      :host "0.0.0.0:14444"
      :protocol "http"
      :stream t
      :models (vd/llama-list-remote-models "http://0.0.0.0:14444")
      ))
    )
  (defun vd/setup-llama-gptel-remote ()
    "Fill llama models."
    (interactive)
    (require 'gptel)
    (setq gptel-backend (gptel-make-openai "llama-swap-remote"
      :host "0.0.0.0:14444"
      :protocol "http"
      :stream t
      :models (vd/llama-list-remote-models "http://192.168.50.136:14444")
      ))
    )
  (defun vd/gptel--edit_file (file-path file-edits)
    "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
    (if (and file-path (not (string= file-path "")) file-edits)
        (with-current-buffer (get-buffer-create "*edit-file*")
          (erase-buffer)
          (insert-file-contents (expand-file-name file-path))
          (let ((inhibit-read-only t)
                (case-fold-search nil)
                (file-name (expand-file-name file-path))
                (edit-success nil))
            ;; apply changes
            (dolist (file-edit (seq-into file-edits 'list))
              (when-let ((line-number (plist-get file-edit :line_number))
                         (old-string (plist-get file-edit :old_string))
                         (new-string (plist-get file-edit :new_string))
                         (is-valid-old-string (not (string= old-string ""))))
                (goto-char (point-min))
                (forward-line (1- line-number))
                (when (search-forward old-string nil t)
                  (replace-match new-string t t)
                  (setq edit-success t))))
            ;; return result to gptel
            (if edit-success
                (progn
                  ;; show diffs
                  (ediff-buffers (find-file-noselect file-name) (current-buffer))
                  (format "Successfully edited %s" file-name))
              (format "Failed to edited %s" file-name))))
      (format "Failed to edited %s" file-path)))

  :config
  (setq gptel-expert-commands t)
  (require 'gptel-transient)
  (transient-suffix-put 'gptel-menu (kbd "-m") :key ".m")
  ;; TOOLS
  (gptel-make-tool
   :name "read_buffer"                    ; javascript-style snake_case name
   :function (lambda (buffer)                  ; the function that will run
               (unless (buffer-live-p (get-buffer buffer))
                 (error "error: buffer %s is not live." buffer))
               (with-current-buffer  buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :description "return the contents of an emacs buffer"
   :args (list '(:name "buffer"
                       :type string            ; :type value must be a symbol
                       :description "the name of the buffer whose contents are to be retrieved"))
   :category "emacs")
  (gptel-make-tool
   :function (lambda (filepath)
               (with-temp-buffer
                 (insert-file-contents (expand-file-name filepath))
                 (buffer-string)))
   :name "read_file"
   :description "Read and display the contents of a file"
   :args (list '(:name "filepath"
                       :type string
                       :description "Path to the file to read. Supports relative paths and ~."))
   :category "filesystem")
  (gptel-make-tool
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
                       :type string
                       :description "The directory where to create the file")
               '(:name "filename"
                       :type string
                       :description "The name of the file to create")
               '(:name "content"
                       :type string
                       :description "The content to write to the file"))
   :category "filesystem")
  (gptel-make-tool
   :function #'vd/gptel--edit_file
   :name "edit_file"
   :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
   :args (list '(:name "file-path"
                       :type string
                       :description "The full path of the file to edit")
               '(:name "file-edits"
                       :type array
                       :items (:type object
                                     :properties
                                     (:line_number
                                      (:type integer :description "The line number of the file where edit starts.")
                                      :old_string
                                      (:type string :description "The old-string to be replaced.")
                                      :new_string
                                      (:type string :description "The new-string to replace old-string.")))
                       :description "The list of edits to apply on the file"))
   :category "filesystem")
  ;; PRESETS
  (gptel-make-preset 'check-en
    :description "Check for grammar, punctuation errors"
    :model 'translate
    :temperature 0.4
    :system "You are the best English linguist in the world, committed to maximum accuracy, transparency, and avoiding unreliable statements. Find all grammatical errors, typos, and punctuation mistakes in this text. Do not explain anything, just provide two or three corrected versions of this text. Never use `;` in corrected text.")
  (gptel-make-preset 'check-ru
    :description "Проверка на грамматические и синтактические ошибки"
    :model 'translate
    :temperature 0.4
    :system "Вы лучший лингвист по русскому языку в мире, настроенный на максимальную точность, прозрачность и отказ от недостоверных утверждений. Найдите все грамматические ошибки, опечатки и пунктуационные ошибки в этом тексте. Ничего не объясняйте, просто предоставьте две или три исправленные версии этого текста")
  (gptel-make-preset 'check-de
    :description "Überprüfung auf grammatikalische und syntaktische Fehler"
    :model 'translate
    :temperature 0.4
    :system "Sie sind der beste Deutschlinguist der Welt, der auf maximale Genauigkeit, Transparenz und die Vermeidung ungenauer Aussagen bedacht ist. Finden Sie alle Grammatik-, Tipp- und Zeichensetzungsfehler in diesem Text. Geben Sie keine Erklärungen, sondern liefern Sie einfach zwei oder drei korrigierte Versionen dieses Textes.")
  (gptel-make-preset 'translate-ru
    :description "русский переводчик"
    :model 'translate
    :temperature 0.4
    :system "Ты — языковая модель, настроенная на максимальную точность, прозрачность и отказ от недостоверных утверждений.
Ты выступаешь в роли русского переводчика. Я буду говорить с тобой на любом языке, а ты определишь язык, переведешь его на русский. Я хочу, чтобы ты ответил только переводом и ничего больше, не пиши никаких объяснений.")
  (gptel-make-preset 'translate-de
    :description "deutscher übersetzer"
    :model 'translate
    :temperature 0.4
    :system "Du bist ein Sprachmodell, das auf maximale Genauigkeit, Transparenz und die Vermeidung unzuverlässiger Aussagen ausgerichtet ist.
Du fungierst als deutscher Übersetzer. Ich werde mit dir in einer beliebigen Sprache sprechen, und du wirst die Sprache bestimmen und sie ins Deutsche übersetzen. Ich möchte, dass du nur mit einer Übersetzung antwortest und nichts weiter, schreibe keine Erklärungen dazu."
)
  (gptel-make-preset 'translate-en
    :description "english translator"
    :model 'translate
    :temperature 0.4
    :system "You are a language model designed to prioritize maximum accuracy, transparency, and the avoidance of unreliable statements.
You act as a English translator. I will speak to you in any language, and you will identify the language and translate it into English. I want you to respond only with a translation and nothing else; do not provide any explanations.")
  (gptel-make-preset 'coding
    :description "A preset optimized for coding tasks"
    :model 'translate
    :temperature 0.4
    :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
    :tools '("read_file" "create_file" "edit_file"))

  :bind
  ("C-c e" . gptel-menu)
  ("C-c z" . vd/setup-llama-gptel)
  ("C-c Z" . vd/setup-llama-gptel-remote)
  :hook
  (gptel-post-stream . gptel-auto-scroll)
  (gptel-post-response-functions . (lambda()
                                     (gptel-end-of-response)
                                     (auto-fill-mode)
                                     ))
)

(provide 'gptel-rcp)

;;; Commentary:
;;
;;; gptel-rcp.el ends here
