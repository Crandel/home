;;; bench.el --- Benchmark for debugging

;;; Code:
(eval-when-compile (require 'cl))

(defvar abn--loaded-file-paths nil
  "All file paths that are loaded.")

(defvar abn--loaded-packages-buffer "*loaded-packages*"
  "Buffer name for data about loaded packages.")

(defvar abn--loaded-features-buffer "*loaded-features*"
  "Buffer name for data about loaded features.")

(defun abn/list-loaded-packages()
  "List all currently loaded file paths."
  (interactive)
  (with-current-buffer (get-buffer-create abn--loaded-packages-buffer)
    (erase-buffer)
    (pop-to-buffer (current-buffer))

    (insert "* Live Packages Exploration\n\n")
    (insert (format "%s total packages currently loaded\n"
                    (length abn--loaded-file-paths)))

    ;; Extract data from builtin variable `load-history'.
    (setq abn--loaded-file-paths
          (seq-filter #'stringp
                      (mapcar #'car load-history)))
    (cl-sort abn--loaded-file-paths 'string-lessp)
    (cl-loop for file in abn--loaded-file-paths
             do (insert "\n" file))

    (goto-char (point-min))))

(defun abn/list-loaded-features()
  "List all currently loaded features."
  (interactive)
  (with-current-buffer (get-buffer-create abn--loaded-features-buffer)
    (erase-buffer)
    (pop-to-buffer (current-buffer))

    (insert (format "\n** %d features currently loaded\n"
                    (length features)))

    (let ((features-vec (apply 'vector features)))
      (cl-sort features-vec 'string-lessp)
      (cl-loop for x across features-vec
               do (insert (format "  - %-25s: %s\n" x
                                  (locate-library (symbol-name x))))))

    (goto-char (point-min))))

(provide 'bench)

;;; Commentary:
;;
;;; bench.el ends here
