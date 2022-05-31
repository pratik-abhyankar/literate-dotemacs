(defvar bootstrap-version)
(let ((bootstrap-file
 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
(bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/master/install.el"
	 'silent 'inhibit-cookies)
(goto-char (point-max))
(eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package straight
			 :custom (straight-use-package-by-default t))
(setq use-package-always-ensure t)

(straight-use-package
  '(sensible-defaults :type git :host github :repo "hrs/sensible-defaults.el"))
(load-file "~/.emacs.d/straight/build/sensible-defaults/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq-default
 inhibit-startup-screen t
 initial-scratch-message "")

(global-display-line-numbers-mode)
(column-number-mode t)
(global-hl-line-mode t)

(setq-default
 tab-always-indent 'complete
 tab-width 2)

(setq-default
 auto-save-default nil
 make-backup-files nil
 create-lockfiles nil)

(setq-default
 select-enable-clipboard t
 mouse-yank-at-point t
 kill-whole-line t)

(setq user-full-name "Pratik Abhyankar")
(setq user-mail-address "abhyankarpratik@gmail.com")
