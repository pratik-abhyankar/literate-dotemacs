(use-package which-key
  :ensure t
  :config
  (setq which-key-add-column-padding 1
        which-key-sort-uppercase-first nil
        which-key-show-early-on-C-h t
        which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.05)
  (which-key-mode))
