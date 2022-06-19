;;; multi-compile-rcp.el --- "Multi-compile" is multi target interface to "compile" command.

;;; Code:
(eval-when-compile (require 'use-package))
(use-package multi-compile
  :ensure t
  :defer t
  :custom
  (multi-compile-alist '((go-mode . (("gr" ("go run -v %file-name")
                                      (locate-dominating-file buffer-file-name ".git"))
                                     ("gb" "go build -v"
                                      (locate-dominating-file buffer-file-name ".git"))
                                     ("gt" "go test -v"
                                      (locate-dominating-file buffer-file-name ".git"))
                                     ("gbr" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
                                      (multi-compile-locate-file-dir ".git"))))
                         (scala-mode . (("scl" "scala"
                                         (buffer-file-name))
                                        ("sbrun" "sbt run"
                                         (locate-dominating-file buffer-file-name ".git"))
                                        ("sbpack" "sbt package"
                                         (locate-dominating-file buffer-file-name ".git"))
                                        ("sbass" "sbt assembly"
                                         (locate-dominating-file buffer-file-name ".git"))
                                        ("sbtall" "sbt compile; sbt test; sbt run"
                                         (multi-compile-locate-file-dir ".git"))))
                         (rust-mode . (("rw" "cargo build --release -q"
                                        (locate-dominating-file buffer-file-name ".git"))
                                       ("rb" "cargo build --release --color never"
                                        (locate-dominating-file buffer-file-name ".git"))
                                       ("rbr" "cargo run --release --color never"
                                        (multi-compile-locate-file-dir ".git"))))
                         (java-mode . (("mpac" "mvn package"
                                        (locate-dominating-file buffer-file-name ".git"))
                                       ("mtest" "mvn validate"
                                        (locate-dominating-file buffer-file-name ".git"))))
                         (c-mode . (("cpac" "make"
                                     (locate-dominating-file buffer-file-name ".git"))
                                    ("cclean" "make clean"
                                     (locate-dominating-file buffer-file-name ".git"))))
                         (c++-mode . (("cpac" "make --no-print-directory -C build"
                                       (locate-dominating-file buffer-file-name ".git"))
                                      ("cclean" "make --no-print-directory -C build clean"
                                       (locate-dominating-file buffer-file-name ".git"))))))
  :bind
  ([f8] . multi-compile-run)
)

(provide 'multi-compile-rcp)
;;; Commentary:
;;
;;; multi-compile-rcp.el ends here
