;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Increase gc-cons-threshold, depending on your system you may set it back to a
;; lower value in your dotfile (function `dotspacemacs/user-config')
(setq gc-cons-threshold 100000000)

(defconst spacemacs-version         "0.200.13" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))
  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)
  (spacemacs-buffer/display-startup-note)
  (spacemacs/setup-startup-hook)
  (require 'server)
  (unless (server-running-p) (server-start)))

;; (setq c-default-style "linux"
;;       indent-tabs-mode nil
;;       tab-width 4
;;       c-basic-offset 4)

(c-add-style "linux"
             '((c-basic-offset . 4)
               (c-offsets-alist
                (statement-cont . c-lineup-assignments)
                (arglist-close . 0)
                )))

(c-add-style "stroustrup"
             '((c-basic-offset . 4)
               (c-offsets-alist
                (statement-cont . c-lineup-assignments)
                (arglist-close . 0)
                )))

(defun my-linum-mode-hook ()
  (when linum-mode
    (setq-local linum-format
                (let ((w (length (number-to-string
                                  (count-lines (point-min) (point-max))))))
                  (concat "%" (number-to-string w) "d ")))))

(add-hook 'linum-mode-hook #'my-linum-mode-hook)
;; (global-linum-mode 1)

(defun set-c-toggle-hungry-state()
  (c-toggle-hungry-state 1))

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "linux")
            (flyspell-prog-mode)
            (turn-on-auto-fill)
            (c-toggle-auto-state 1)
            (c-toggle-auto-newline 0)
            ))
(add-hook 'c-mode-hook 'set-c-toggle-hungry-state)

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "stroustrup")
            (flyspell-prog-mode)
            (turn-on-auto-fill)
            (c-toggle-auto-state 1)
            (c-toggle-auto-newline 0)
            ))
(add-hook 'c++-mode-hook 'set-c-toggle-hungry-state)

