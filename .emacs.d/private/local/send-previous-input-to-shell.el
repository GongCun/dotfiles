;;; send-previous-input-to-shell.el --- send last shell input to the specified
;;; shell buffer. Reference to https://www.emacswiki.org/emacs/essh.el

(defun process-shell ()
  "return a list with existing shell processes."
  (interactive)
  (setq proc-list (process-list))
  (setq shell-list '())
  (while proc-list
    (if (string-match "\\<shell\\>" (prin1-to-string (car proc-list)))
        ;; (print (process-buffer (car proc-list)))
        (add-to-list 'shell-list (buffer-name (process-buffer (get-process (car proc-list)))))
      )
    (setq proc-list (cdr proc-list)))
  shell-list)

(defun process-shell-choose ()
  "returns which process to use"
  (interactive)
  (setq shell-buf nil)
  (setq shell-list (process-shell))
  ;; (print shell-list)
  (setq shell-len (length shell-list))
  (if (eq shell-len 0)
      (setq shell-buf nil)
    (if (eq shell-len 1)
        (setq shell-buf (car shell-list))
      (setq shell-buf
            (completing-read "run in: " shell-list nil t nil))
      ))
  shell-buf)

(defun send-previous-input-to-shell (&optional arg)
  "Run previous command in shell window without switching buffer.
If there are more than one shell running, the buffer that was
switched last time will be selected for execution, it the buffer
is not shell-mode, will ask user to choose the buffer name."
  (interactive "*P")
  (if arg
      (setq arg (prefix-numeric-value arg))
    (setq arg 1))

  (setq buflen (length (process-shell)))

  (setq buffer nil)

  (if (> buflen 1)
      (let ((buf (car (car (window-prev-buffers)))))
        (setq mode (with-current-buffer buf major-mode))
        (if (string= "shell-mode" (format "%s" mode))
            (setq buffer buf)
          )))

  (unless buffer (setq buffer (process-shell-choose)))
  (unless buffer (error "no shell buffer"))

  (save-excursion
    (setq cbuf (current-buffer))
    (switch-to-buffer buffer)
    (switch-to-buffer cbuf))


  (with-current-buffer buffer
    (goto-char (point-max))
    (comint-previous-input arg)
    (comint-send-input)))


(defun shell-eval-line (buffer command)
  "Evaluate a single command into the shell process."
  ;; (setq command (concat command "\n"))
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert command)
    (push-mark (point-max))
    (comint-send-input)
    )
  )

(defun shell-cd-current-directory ()
  "Changes the shell working directory to the current buffer's one."
  (interactive)
  (setq buffer (process-shell-choose))
  (unless buffer
    (let ((cbuf (current-buffer)))
      (shell)
      (switch-to-buffer cbuf)
      (setq buffer (process-shell-choose))
      (sleep-for 0.5)))
  (setq command (format "cd %s" (file-name-directory default-directory)))
  (shell-eval-line buffer command)
  )

