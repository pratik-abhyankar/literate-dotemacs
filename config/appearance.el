(unless (file-exists-p CUSTOM-FILE)
    (with-temp-buffer (write-file CUSTOM-FILE)))
(setq custom-file CUSTOM-FILE)
(load-file custom-file)

(use-package all-the-icons
	:if (display-graphic-p)
	:config
	(unless (member "all-the-icons" (font-family-list))
		(all-the-icons-install-fonts t))
	(setq inhibit-compacting-font-caches t))

(set-face-attribute 'default nil :height (if IS-MAC 150 110) :font "Fira Code")

(use-package doom-modeline
  :init (doom-modeline-mode))

(straight-use-package
  '(solarized-emacs :type git :host github :repo "bbatsov/solarized-emacs"))
(require 'solarized)
(setq
  x-underline-at-descent-line nil
  solarized-scale-markdown-headlines t
  solarized-use-variable-pitch nil)
(load-theme 'solarized-light-high-contrast t)

(setq active-theme 'solarized-light-high-contrast)
(defun toggle-dark-light-theme ()
  "Toggle between dark and light mode theme."
  (interactive)
  (if (eq active-theme 'solarized-light-high-contrast)
      (setq active-theme 'solarized-dark-high-contrast)
    (setq active-theme 'solarized-light-high-contrast))
  (load-theme active-theme t))
(bind-key "C-x c" 'toggle-dark-light-theme)
