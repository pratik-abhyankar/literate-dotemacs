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


(defconst CONFIG-DIR (expand-file-name "config/" user-emacs-directory))

(defun pratik/load-config-file (file)
  "Load a literate org config FILE from the 'config' directory."
  (defconst ORG-CONFIG-FILE (expand-file-name (concat CONFIG-DIR  file ".org")))
  (defconst EL-CONFIG-FILE (expand-file-name (concat CONFIG-DIR file ".el")))
  (org-babel-tangle-file ORG-CONFIG-FILE)
  (if (file-exists-p EL-CONFIG-FILE)
    (org-babel-load-file ORG-CONFIG-FILE)))

(defun pratik/load-mode-config-file (file)
  "Load a literate org mode config FILE from the 'config/modes/' directory."
  (defconst MODES-DIR (expand-file-name "modes/" CONFIG-DIR))
	(defconst ORG-MODE-CONFIG-FILE (expand-file-name (concat MODES-DIR  file ".org")))
	(defconst EL-MODE-CONFIG-FILE (expand-file-name (concat MODES-DIR file ".el")))
	(org-babel-tangle-file ORG-MODE-CONFIG-FILE)
	(if (file-exists-p EL-MODE-CONFIG-FILE)
			(org-babel-load-file ORG-MODE-CONFIG-FILE)))

(pratik/load-config-file "constants")
(pratik/load-config-file "bootstrap")
(pratik/load-config-file "appearance")
(pratik/load-config-file "core")
(pratik/load-config-file "org")
(pratik/load-config-file "dev")
(pratik/load-config-file "eloquence")

(pratik/load-mode-config-file "ledger")
(pratik/load-mode-config-file "python")

;;; init.el ends here
