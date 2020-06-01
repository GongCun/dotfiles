;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; chinese
     themes-megapack
     git
     ;; version-control
     yaml
     (shell :variables shell-enable-smart-eshell t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'top
            shell-default-shell 'shell)
     ;; (shell :variables shell-default-term-shell "c:/cygwin64/bin/bash.exe")
     command-log
     auto-completion
     d
     graphviz
     html
     windows-scripts
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     ;; git
     markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     spell-checking
     syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(evil-tabs vimish-fold htmlize)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(evil-escape auto-complete)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(subatomic
                         spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("InputMono"
                               ;; "Source Code Pro"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq ispell-dictionary "english")
  (load-file "~/.emacs.d/private/local/.myemacs")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  ;; use spaces instead of tabs
  (setq-default indent-tabs-mode nil)
  (setq tab-width 8)
  ;; seems not work, but keep here
  (setq c-default-style "linux"
        c-basic-offset 4)
  (add-to-list 'c-offsets-alist '(substatement-open . 0))

  (global-unset-key (kbd "C-w"))
  (global-set-key (kbd "C-w") 'kill-ring-save)
  (global-unset-key (kbd "M-w"))
  (global-set-key (kbd "M-w") 'kill-region)
  (global-unset-key (kbd "M-a"))
  (global-set-key (kbd "M-a") 'back-to-indentation)
  (global-company-mode)
  (setq ac-auto-start 4
        ac-auto-show-menu 0.8)

  ;; (global-set-key "\M-'" 'auto-complete-mode)
  (global-set-key "\M-'" 'company-mode)
  ;; (global-set-key "\M-/" 'dabbrev-expand)
  (global-set-key (kbd "C-S-j") 'delete-indentation)
  (global-set-key (kbd "C-S-k") 'delete-region)
  ;; (global-set-key (kbd "C-M-k") 'delete-region)
  (global-unset-key (kbd "C-M-k"))
  (global-set-key (kbd "C-M-k") 'kill-sexp)
  (global-set-key [C-M-backspace] 'backward-kill-sexp)
  (setq tramp-default-method "plink")
  ;; It's bound to 'tmm-menuber' default, but will hang the spacemacs, temporary
  ;; disable it.
  (global-unset-key (kbd "M-`"))
  (global-set-key "\M-`" 'auto-fill-mode)
  (global-set-key (kbd "C-M-`") 'not-modified)

  (global-unset-key "\C-x\C-l")
  (global-set-key "\C-x\C-l" 'recenter-top-bottom)

  (defun scroll-up-one () "Scroll up 1 line." (interactive)
         (scroll-up (prefix-numeric-value current-prefix-arg)))
  (defun scroll-down-one () "Scroll down 1 line." (interactive)
         (scroll-down (prefix-numeric-value current-prefix-arg)))

  (global-unset-key "\C-l")
  (global-set-key "\C-l" 'scroll-up-one)

  (global-unset-key "\M-l")
  (global-set-key "\M-l" 'scroll-down-one)
  (add-to-list 'evil-emacs-state-modes 'vc-git-log-edit-mode)
  (eval-after-load "evil-maps"
    (define-key evil-motion-state-map "\C-wo" nil))
  (add-to-list 'evil-emacs-state-modes 'shell-mode)
  (add-to-list 'evil-emacs-state-modes 'eshell-mode)

  ;; (global-unset-key (kbd "M-m SPC"))
  ;; (global-set-key (kbd "M-m SPC") (lambda () (interactive) (other-window 1)))
  ;; (global-set-key (kbd "M-m M-m") (lambda () (interactive) (unexpand-abbrev)))
  (global-set-key (kbd "M-m d") 'downcase-word)
  (global-set-key (kbd "M-m SPC") 'other-window)
  (global-set-key (kbd "M-m M-m") 'unexpand-abbrev)
  (global-set-key (kbd "M-m h") 'windmove-left)
  (global-set-key (kbd "M-m j") 'windmove-down)
  (global-set-key (kbd "M-m k") 'windmove-up)
  (global-set-key (kbd "M-m l") 'windmove-right)
  (global-set-key (kbd "M-m u") 'winner-undo)
  (global-set-key (kbd "M-m U") 'winner-redo)
  ;;; cpoy from: https://github.com/tumashu/cnfonts
  (set-frame-font
   "-outline-InputMono-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")

  (when (display-graphic-p)
    (setq fonts '("InputMono"	"WenQuanYi Micro Hei Mono")))

  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "WenQuanYi Micro Hei Mono" :size 16)))

  ;;; copy from: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bintl/chinese
  ;; (spacemacs//set-monospaced-font   "InputMono" "Microsoft Yahei" 15 16)


  (load-file "~/.emacs.d/private/local/send-previous-input-to-shell.el")
  (global-set-key (kbd "M-m C-e") 'send-previous-input-to-shell)
  (global-set-key (kbd "M-m C-d") 'shell-cd-current-directory)
  ;; maybe should (load-file .myemacs) here
  (require 'evil)
  (eval-after-load "evil-map"
    (progn
      (dolist (map '(evil-motion-state-map
                     evil-insert-state-map
                     evil-normal-state-map
                     evil-visual-state-map
                     ))
        (define-key (eval map) [C-M-backspace] nil)
        ;; (define-key (eval map) "\C-u" nil)
        (define-key (eval map) "\C-e" nil)
        (define-key (eval map) "\C-a" nil)
        ;; (define-key (eval map) "\C-d" nil)
        (define-key (eval map) "\C-k" nil)
        (define-key (eval map) "\C-n" nil)
        (define-key (eval map) "\C-p" nil)
        (define-key (eval map) "\C-w" 'kill-ring-save)
        (define-key (eval map) "\C-y" nil))))
      ;; (define-key evil-motion-state-map "\C-wo" nil)))

  ;;; setup spell check & save
  (load-file "~/.emacs.d/private/local/flyspell-goto-previous-error.el")
  (global-set-key (kbd "C-c ,")
                  (lambda () (interactive) (push-mark)
                    (evil--next-flyspell-error nil)))
  (global-set-key (kbd "C-c .") 'evil-prev-flyspell-error-save-word)

  ;;;
  (spaceline-toggle-window-number-on)

  ;;; Remove key-mapping from org-mode
  (with-eval-after-load "org"
    (progn
      (define-key org-mode-map (kbd "M-a") nil)
      (define-key org-mode-map (kbd "C-c ,") nil)
      (define-key org-mode-map (kbd "C-c .") nil)
      (define-key org-mode-map (kbd "C-c k") nil)
      (define-key org-mode-map (kbd "C-c C-'") 'org-edit-special)
      ;; (define-key org-mode-map (kbd "C-c C-x C-f") (kbd "C-c C-x f"))
      ))

  (defun check-evil-mode ()
    (let ((list minor-mode-list))
      (while list
        (if (string-match "\\<evil-mode\\>" (prin1-to-string (car list)))
            (evil-emacs-state))
        (setq list (cdr list)))))

  (add-hook 'org-src-mode-hook
            (lambda ()
              ;; (setq-local evil-default-state 'emacs)
              ;; (evil-emacs-state)
              (check-evil-mode)
              (local-set-key (kbd "C-c C-'") 'org-edit-src-exit)
              (local-set-key (kbd "C-c C-x C-f") 'org-footnote-action)
              (delete-other-windows)))
  ;; (define-key org-src-mode-hook (kbd "C-c C-'") 'org-edit-src-exit)

  ;;; remove whitespace-mode
  (let ((i 0)
        (len (length diff-mode-hook)))
    (while (< i len)
      (if (string= "whitespace-mode" (prin1-to-string (elt diff-mode-hook i)))
          (remove-hook 'diff-mode-hook 'whitespace-mode))
      (setq i (1+ i))))

  ;;;
  (add-hook 'after-init-hook 'global-company-mode)

  ;;;
  (add-hook 'emacs-lisp-mode-hook 'hungry-delete-mode)

  ;;;
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(c-basic-offset 4)
 '(c-electric-pound-behavior (quote (alignleft)))
 '(c-mode-hook
   (quote
    (macrostep-c-mode-hook set-c-toggle-hungry-state
                           (lambda nil
                             (c-set-style "linux")
                             (flyspell-prog-mode)
                             (turn-on-auto-fill)
                             (c-toggle-auto-state 1)
                             (c-toggle-auto-newline 1)))))
 '(desktop-save-mode t)
 '(diff-mode-hook (quote (spacemacs//set-whitespace-style-for-diff)))
 '(evil-want-C-d-scroll nil)
 '(evil-want-C-u-scroll nil)
 '(evil-want-C-w-delete nil)
 '(evil-want-Y-yank-to-eol nil)
 '(explicit-bash-args (quote ("--noediting")))
 '(org-agenda-files
   (quote
    ("~/.emacs.d/org/work.org" "~/.emacs.d/org/home.org" "d:/Documents/GongCun/plan/plan.org")))
 '(org-export-with-sub-superscripts (quote {}))
 '(org-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("IEEEtran" "\\documentclass[9pt,journal,compsoc]{IEEEtran}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
 '(org-log-done (quote time))
 '(package-selected-packages
   (quote
    (xr pangu-spacing find-by-pinyin-dired ace-pinyin pinyinlib flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip auto-dictionary git-commit with-editor transient diff-hl zenburn-theme zen-and-art-theme yaml-mode white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme orgit organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme magit-gitflow magit-popup madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme helm-gitignore hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme evil-magit magit xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help command-log-mode helm-company helm-c-yasnippet fuzzy company-web web-completion-data company-statistics company-dcd ivy flycheck-dmd-dub flycheck company auto-yasnippet yasnippet ac-ispell auto-complete d-mode graphviz-dot-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode htmlize powershell vimish-fold evil-tabs elscreen markdown-toc mmm-mode markdown-mode gh-md ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(tab-width 8))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#303347" :foreground "#e5e5e5"))))
 '(company-tooltip-common ((((class color) (min-colors 89)) (:background "#5f5f5f" :foreground "#5fafd7"))))
 '(company-tooltip-common-selection ((((class color) (min-colors 89)) (:background "#767676" :foreground "#afd7ff" :bold t))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#b2b2b2" :slant italic))))
 '(font-lock-comment-face ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "forest green" :slant italic :weight bold))))
 '(sh-heredoc ((t (:foreground "medium sea green" :weight bold)))))
