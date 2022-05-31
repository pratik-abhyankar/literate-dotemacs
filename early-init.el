;;; early-init.el --- Declares early init config for Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Author: Pratik Abhyankar
;; Created: May 29 2022

;;; Code:

;; Disable the built in package.el package manager. The config will use straight.el instead.
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here