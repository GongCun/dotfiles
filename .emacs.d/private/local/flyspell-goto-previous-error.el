;; move point to previous error
;; copy and change from:
;; http://pragmaticemacs.com/emacs/jump-back-to-previous-typo/

(require 'flyspell)
(require 'evil)

(defun flyspell-goto-previous-error ()
  "Go to the previous spelling error."
  (interactive)
  (setq save-position (point))
  (let ((pos (point))
        (min (point-min)))

    (if (eq (current-buffer) flyspell-old-buffer-error)
        (progn
          (while (and (not (eq pos flyspell-old-pos-error))
                      (> pos min)
                      (let ((ovs (overlays-at pos))
                            (r '()))
                        (while (and (not r) (consp ovs))
                          (if (flyspell-overlay-p (car ovs))
                              (setq r t)
                            (setq ovs (cdr ovs))))
                        (not r)))
            (backward-word 1)
            (setq pos (point)))
          ;; save the current location for next invocation

          (if (= pos min)
              (progn
                (goto-char save-position)
                (message "No more miss-spelled word!")
                (error nil))
            (progn
              ;; (setq flyspell-old-pos-error pos)
              ;; (setq flyspell-old-buffer-error (current-buffer))
              (goto-char pos)))))))

(defun my-save-word ()
  "Add the current word in the point to dictionary."
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct
       'save nil
       (car word)
       current-location
       (cadr word)
       (caddr word) current-location))))



(defun flyspell-goto-previous-error-save-word ()
  "Move to the previous mis-spelling word and add it to the dictionary."
  (interactive)
  (save-excursion
    (if (flyspell-goto-previous-error)
        (my-save-word))))


(defun evil-prev-flyspell-error-save-word ()
  "Move to the previous mis-spelling word and add it to the
dictionary by evil-functions."
  (interactive)
  (save-excursion
    (setq save-position (point))
    (evil--next-flyspell-error nil)
    (if (not (eq save-position (point)))
        (my-save-word))))
