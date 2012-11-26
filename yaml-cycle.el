(require 'outline)

(defun string-repeat (str n)
  "Repeat STR N times."
  (apply 'concat (make-list n str)))

(defun yaml-back-to-heading ()
  (beginning-of-line))

(defun yaml-current-heading-level ()
  (save-excursion
    (yaml-back-to-heading)
    (if (re-search-forward "[^ ]" (line-end-position) t)
        (- (point) 1 (line-beginning-position))
      0)))

(defun yaml-next-heading ()
  (re-search-forward "^ *[^:#]+:" nil t))

(defun yaml-next-heading-same-level ()
  (let ((level (yaml-current-heading-level))
        (start (point)))
    (forward-line)
    (catch 'ret
      (while (yaml-next-heading)
        (let ((new-level (yaml-current-heading-level)))
          (cond ((< new-level level)
                 (goto-char start)
                 (throw 'ret nil))
                ((= new-level level)
                 (throw 'ret (point)))))))))

(defun yaml-next-heading-next-level ()
  (let ((level (yaml-current-heading-level))
        (start (point)))
    (forward-line)
    (yaml-next-heading)
    (if (< level (yaml-current-heading-level))
        (point)
      (goto-char start)
      nil)))

(defun yaml-end-of-current-heading ()
  (save-excursion
    (let ((level (yaml-current-heading-level)))
      (forward-line)
      (or (yaml-next-heading)
          (1- (point-max)))
      (catch 'end
        (while (> (yaml-current-heading-level) level)
          (or (yaml-next-heading)
              (throw 'end (point-max))))
        (1- (line-beginning-position))))))

(defun yaml-flag-current-heading (flag)
  (outline-flag-region (line-end-position) (yaml-end-of-current-heading) flag))

(defun yaml-heading-visible-p ()
  (let ((begin (point))
        (end (yaml-end-of-current-heading)))
    (= end (next-single-char-property-change begin 'invisible nil end))))

(defun yaml-current-heading-invisible-p ()
  (outline-invisible-p (line-end-position)))

(defun yaml-show-children ()
  (save-excursion
    (yaml-flag-current-heading nil)
    (when (yaml-next-heading-next-level)
      (yaml-flag-current-heading t)
      (while (yaml-next-heading-same-level)
        (yaml-flag-current-heading t)))))

(defun yaml-cycle-local ()
  (interactive)
  (if (outline-invisible-p (line-end-position))
      (yaml-show-children)
    (if (yaml-heading-visible-p)
        (yaml-flag-current-heading t)
      (yaml-flag-current-heading nil))))

(defun yaml-contents ()
  (save-excursion
    (goto-char (point-min))
    (show-all)
    (when (yaml-next-heading)
      (yaml-flag-current-heading t)
      (while (yaml-next-heading-same-level)
        (yaml-flag-current-heading t)))))

(defun yaml-cycle-global ()
  (interactive)
  (if (= (point-max) (next-single-char-property-change (point-min) 'invisible))
      (yaml-contents)
    (show-all)))

(defun yaml-cycle ()
  (outline-minor-mode 1)
  (define-key yaml-mode-map (kbd "<C-tab>") 'yaml-cycle-local)
  (define-key yaml-mode-map (kbd "<backtab>") 'yaml-cycle-global))

(add-hook 'yaml-mode-hook 'yaml-cycle)

(provide 'yaml-cycle)
