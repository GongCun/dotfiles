
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq default-major-mode 'text-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(show-paren-mode)
(setq show-paren-delay 0)

(ac-config-default)

(setq-default file-column 78)

(setq column-number-mode t)
(setq line-number-mode t)
(global-set-key "\C-xp" 'picture-mode)
(global-set-key "\C-ci" 'string-insert-rectangle)
(global-set-key "\C-xt" 'visit-tags-table)
(global-set-key [f8] 'compile)

(add-hook 'c-mode-hook
	  '(lambda ( )
	     (c-set-style "k&r")))

(global-linum-mode t)
(global-set-key "\M-*" 'pop-tag-mark)

(setq lazy-highlight-cleanup nil)
(setq save-interprogram-paste-before-kill t)
(setq yank-pop-change-selection t)


;;disable backup
(setq backup-inhibited t)

;;disable auto-save
(setq auto-save-default nil)

(global-auto-revert-mode 1)

(load-theme 'tsdh-dark t)

;; Define vi simulation operations
(defun vi-open-line-above ()
  "Insert a newline above the current line and restore the point"
  (interactive)
  (save-excursion
    (unless (bolp)
      (beginning-of-line))
    (newline)
    (forward-line -1)
    (indent-according-to-mode)))

(defun vi-open-line-below ()
  "Insert a newline below the current line and restore the point"
  (interactive)
  (save-excursion 
    (unless (eolp)
      (end-of-line))
    (newline-and-indent)))

(defun kill-current-line (&optional n)
  "Delete the current line"
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((kill-whole-line t))
      (kill-line n))))

(defun kill-to-char (ch)
  "Kill up to and including first occurrence of ch.
Case-sensitive, Goes backward if ARG is negative; error if ch not
found."
  (interactive "cCharacter: ")
  (let ((case-fold-search nil))  
    (zap-to-char 1 ch)
    (indent-for-tab-command)))

(defun kill-to-pre-char (ch)
  "Kill up to and including first previous occurrence of ch.
Case-sensitive."
  (interactive "cPrevious Character: ")
  (let ((case-fold-search nil))
    (zap-to-char -1 ch)))

(defun vi-join-line ()
  (interactive)
  (save-excursion
    (forward-line 1)
    (delete-indentation)))

(defun vi-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-end-position arg))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun vi-remove-to-begin ()
  "Remove from begin of line to current point"
  (interactive)
  (let ((beg (line-beginning-position))
	(end (point)))
    (delete-region beg end)))

(global-set-key "\C-\M-o" 'vi-open-line-above)
(global-set-key "\C-o" 'vi-open-line-below)
(global-set-key "\C-cd0" 'vi-remove-to-begin)
(global-set-key "\C-cdd" 'kill-current-line)
(global-set-key "\C-cdf" 'kill-to-char)
(global-set-key "\C-cdt" 'kill-to-pre-char)
(global-set-key "\C-cj" 'vi-join-line)
(global-set-key "\C-cyy" 'vi-copy-line)

;; Hide the welcome screen and startup message
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


;; Add ispell-mode
(add-to-list 'exec-path "C:\Program Files (x86)\aspell\bin")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.ispell")
(require 'ispell)
(setq-default flyspell-mode t)
(global-set-key (kbd "<f9>") 'ispell-word)
(global-set-key (kbd "C-<f9>") 'flyspell-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compile-command "make ")
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes (quote ("46ac0485dd25a2bc40caec7d70952020f890c583de5552aeb567f63f4afe6d13" default)))
 '(exec-path (quote ("C:Program Files (x86)/aspell/bin" "c:/Program Files (x86)/Common Files/NetSarang" "C:/Program Files (x86)/Intel/iCLS Client/" "C:/Program Files/Intel/iCLS Client/" "C:/windows/system32" "C:/windows" "C:/windows/System32/Wbem" "C:/windows/System32/WindowsPowerShell/v1.0/" "C:/Program Files (x86)/QuickTime/QTSystem/" "C:/Program Files/Intel/Intel(R) Management Engine Components/DAL" "C:/Program Files/Intel/Intel(R) Management Engine Components/IPT" "C:/Program Files (x86)/Intel/Intel(R) Management Engine Components/DAL" "C:/Program Files (x86)/Intel/Intel(R) Management Engine Components/IPT" "C:/Program Files (x86)/Intel/OpenCL SDK/3.0/bin/x86" "C:/Program Files (x86)/Intel/OpenCL SDK/3.0/bin/x64" "C:/PROGRA~1/SQLLIB/BIN" "C:/PROGRA~1/SQLLIB/FUNCTION" "C:/Program Files (x86)/IBM/Personal Communications/" "C:/Program Files (x86)/IBM/Trace Facility/" "C:/Program Files/Microsoft/Web Platform Installer/" "C:/Program Files (x86)/Microsoft ASP.NET/ASP.NET Web Pages/v1.0/" "C:/Program Files (x86)/Windows Kits/8.0/Windows Performance Toolkit/" "C:/Program Files/Microsoft SQL Server/110/Tools/Binn/" "C:/Program Files/Git/cmd" "c:/Program Files/emacs-24.3/bin" "c:/Program Files/emacs-24.3/lib-src/oo-spd/i386" "c:/Program Files/emacs-24.3/lib-src/oo/i386")))
 '(package-selected-packages (quote (auto-complete))))


;; Move to beginning or end of buffer
(global-set-key (kbd "C-c <home>") 'beginning-of-buffer)
(global-set-key (kbd "C-c <end>") 'end-of-buffer)
