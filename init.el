;;; init.el --- Loads Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; Author: Pratik Abhyankar
;; Created: May 29 2022

;;; Code:

;; Load all literate Emacs config with ~org-babel~ from ~config/~ directory.
;; Since I want these files to load in a specific order, I load each of them
;; individually rather than looping through them.
(require 'org)
(require 'ob-tangle)

; By default, org-babel :tangle is set to "no". Change it to "yes" so that
; all literate config config files are tangled and loaded by 'org-babel'.
(setq org-babel-default-header-args
      (cons '(:tangle . "yes")
            (assq-delete-all :tangle org-babel-default-header-args)))

(defun pratik/load-config-file (file)
  "Load a literate org config file from the 'config' directory"
  (defconst FILE-PATH (expand-file-name (concat user-emacs-directory "config/" file)))
  (org-babel-load-file FILE-PATH))

(pratik/load-config-file "constants.org")
(pratik/load-config-file "bootstrap.org")
(pratik/load-config-file "appearance.org")
(pratik/load-config-file "core.org")
(pratik/load-config-file "org.org")
(pratik/load-config-file "dev.org")

;;; init.el ends here
