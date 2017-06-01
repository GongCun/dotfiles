(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(require 'dired)
(define-key dired-mode-map "c" 'find-file)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(require 'evil)
(evil-mode 1)

(ac-config-default)

;; Highlight parentheses
(show-paren-mode)
(setq show-paren-delay 0)

;; Hide the welcome screen and startup message
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


;; Add ispell-mode
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.ispell")
(require 'ispell)
(setq-default flyspell-mode t)
(global-set-key (kbd "<f9>") 'ispell-word)
(global-set-key (kbd "<f10>") 'flyspell-mode)


