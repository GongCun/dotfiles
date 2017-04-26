
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
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq url-proxy-services
  '(("no_proxy". "^\\(localhost\\)")
    ("http" . "proxysvr.bocmo.com:8080")
    ("https" . "proxysvr.bocmo.com:8080")))

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
    (kill-region beg end)))

(defun vi-yank-range(start end)
  "Yank lines from start to end"
  (interactive "nBegin #line: \nnEnd #line: ")
  (save-excursion
    (goto-line start)
    (setq BEG (point))
    (goto-line end)
    (setq END (line-end-position))
    (kill-ring-save BEG END)))

(defun vi-paste-line ()
  "Paste lines in newline"
  (interactive)
  (end-of-line)
  (newline)
  (yank))

(defun vi-remove-range(start end)
  "Remove lines from start to end, if start > end, switch the two values."
  (interactive "nBegin #line: \nnEnd #line: ")
  (save-excursion
    (if (> start end)
        (progn
          (setq tmp start)
          (setq start end)
          (setq end tmp)))
    (goto-line start)
    (setq BEG (point))
    (goto-line end)
    (setq END (line-end-position))
    (kill-region BEG END)
    (setq lines (+ (- end start) 1))
    (message "%d line%s removed" lines (if (= 1 lines) "" "s"))))


(defun vi-yank-range(start end)
  "Yank lines from start to end, if start > end, switch the two values."
  (interactive "nBegin #line: \nnEnd #line: ")
  (save-excursion
    (if (> start end)
        (progn
          (setq tmp start)
          (setq start end)
          (setq end tmp)))
    (goto-line start)
    (setq BEG (point))
    (goto-line end)
    (setq END (line-end-position))
    (kill-ring-save BEG END)
    (setq lines (+ (- end start) 1))
    (message "%d line%s copied" lines (if (= 1 lines) "" "s"))))

(defun vi-remove-to-end-of-buffer ()
  "Remove from current line to the end of buffer"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (point-max)))
    (kill-region beg end)))

(defun vi-remove-to-begin-of-buffer ()
  "Remove from begin of buffer to current line"
  (interactive)
  (let ((beg (point-min))
        (end (line-end-position)))
    (kill-region beg end)))

;; Move to dipalyed line functions
(defun move-to-window-bottom ()
    (interactive)
    (move-to-window-line -1))

(defun move-to-window-top ()
    (interactive)
    (move-to-window-line 0))

(defun window-half-height ()
    (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun move-to-window-middle ()
    (interactive)
    (move-to-window-line (window-half-height)))

(defun vi-move-to-char(ch)
  "Move to the certain character"
  (interactive "cchar: ")
  (setq savep (point))
  (forward-char) ;; Don't include the starting position
  (while (and (not (eq (point) (line-end-position)))
	      (not (eq ch (char-after))))
    (forward-char))
  (if (eq (point) (line-end-position))
      (progn
	(goto-char savep)
	(message "can't find the character!"))))
    
(defun vi-move-to-previous-char(ch)
  "Move to the previous certain character"
  (interactive "cchar: ")
  (setq savep (point))
  (backward-char) ;; Don't include the starting position
  (while (and (not (eq (point) (line-beginning-position)))
	      (not (eq ch (char-after))))
    (backward-char))
  (if (eq (point) (line-beginning-position))
      (progn
	(goto-char savep)
	(message "can't find the character!"))))


(global-set-key "\C-\M-o" 'vi-open-line-above)
(global-set-key "\C-o" 'vi-open-line-below)
(global-set-key "\C-cd0" 'vi-remove-to-begin)
(global-set-key "\C-cdG" 'vi-remove-to-end-of-buffer)
(global-set-key "\C-cdg" 'vi-remove-to-begin-of-buffer)
(global-set-key "\C-cdd" 'kill-current-line)
(global-set-key "\C-cdf" 'kill-to-char)
(global-set-key "\C-cdt" 'kill-to-pre-char)
(global-set-key "\C-cj" 'vi-join-line)
(global-set-key "\C-cyy" 'vi-copy-line)
(global-set-key "\C-cry" 'vi-yank-range)
(global-set-key "\C-crd" 'vi-remove-range)
(global-set-key "\C-cp" 'vi-paste-line)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-c." 'repeat)
;;(global-set-key "\C-cm" 'point-to-register)
;;(global-set-key "\C-c`" 'jump-to-register)
(global-set-key "\C-cm" 'bookmark-set)
(global-set-key "\C-c`" 'bookmark-jump)
(global-set-key "\C-cH" 'move-to-window-top)
(global-set-key "\C-cL" 'move-to-window-bottom)
(global-set-key "\C-cM" 'move-to-window-middle)
(global-set-key "\C-cf" 'vi-move-to-char)
(global-set-key "\C-ct" 'vi-move-to-previous-char)
(global-set-key "\C-c!" 'shell-command)

;; Hide the welcome screen and startup message
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Move to beginning or end of buffer
(global-set-key (kbd "\C-c <up>") 'beginning-of-buffer)
(global-set-key (kbd "\C-c <down>") 'end-of-buffer)
(global-set-key (kbd "\C-c <left>") 'move-beginning-of-line)
(global-set-key (kbd "\C-c <right>") 'move-end-of-line)


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
 '(exec-path (quote ("C:/Program Files (x86)/aspell/bin" "c:/Program Files (x86)/Common Files/NetSarang" "C:/Program Files (x86)/Intel/iCLS Client/" "C:/Program Files/Intel/iCLS Client/" "C:/windows/system32" "C:/windows" "C:/windows/System32/Wbem" "C:/windows/System32/WindowsPowerShell/v1.0/" "C:/Program Files (x86)/QuickTime/QTSystem/" "C:/Program Files/Intel/Intel(R) Management Engine Components/DAL" "C:/Program Files/Intel/Intel(R) Management Engine Components/IPT" "C:/Program Files (x86)/Intel/Intel(R) Management Engine Components/DAL" "C:/Program Files (x86)/Intel/Intel(R) Management Engine Components/IPT" "C:/Program Files (x86)/Intel/OpenCL SDK/3.0/bin/x86" "C:/Program Files (x86)/Intel/OpenCL SDK/3.0/bin/x64" "C:/PROGRA~1/SQLLIB/BIN" "C:/PROGRA~1/SQLLIB/FUNCTION" "C:/Program Files (x86)/IBM/Personal Communications/" "C:/Program Files (x86)/IBM/Trace Facility/" "C:/Program Files/Microsoft/Web Platform Installer/" "C:/Program Files (x86)/Microsoft ASP.NET/ASP.NET Web Pages/v1.0/" "C:/Program Files (x86)/Windows Kits/8.0/Windows Performance Toolkit/" "C:/Program Files/Microsoft SQL Server/110/Tools/Binn/" "C:/Program Files/Git/cmd" "c:/Program Files/emacs-24.3/bin" "c:/Program Files/emacs-24.3/lib-src/oo-spd/i386" "c:/Program Files/emacs-24.3/lib-src/oo/i386" "C:/Program Files/multimarkdown_5.3.0/bin")))
 '(package-selected-packages (quote (auto-complete))))


;; Move to beginning or end of buffer
(global-set-key (kbd "C-c <home>") 'beginning-of-buffer)
(global-set-key (kbd "C-c <end>") 'end-of-buffer)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; For markdown-mode
(setq markdown-command "multimarkdown")
      
;; Copy from stack-overflow -- Full-line completion
(defun vi-full-line-completion()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))

(global-set-key "\C-c\C-l" 'vi-full-line-completion)
