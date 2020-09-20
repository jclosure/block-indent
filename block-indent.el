;;; block-indent.el --- Indent/de-indent whole blocks under control with no magic
;; Package-Version: 20200918.900
;;; Commentary:
;; Indent single line and regions as blocks from left margin.  Allows tab stop
;; indentation to manage whitespace between left margin from anywhere in a
;; line.  Convenient for coding environments where tabs do not get interspersed
;; after left indent.
;;
;; usage example:
;;
;;   (require 'block-indent)
;;   (add-hook 'text-mode-hook 'block-indent-mode)
;;
;;; Code:

(defvar block-indent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backtab>") 'block-indent-left)
    (define-key map (kbd "<tab>") 'block-indent-right)
    (define-key map (kbd "TAB") 'block-indent-right)
    map))

(defun block-indent--select-current-line ()
  "Select the current line"
  (interactive)
  (save-excursion
    (end-of-line) ; move to end of line
    (set-mark (line-beginning-position))))

(defun block-indent--select-region (beg end)
  (interactive "r")
  (setq deactivate-mark nil))

(defun block-indent--region-beginning-adjusted ()
  (save-excursion
    (goto-char (region-beginning))
    (line-beginning-position)))

(defun block-indent--region-end-adusted ()
  (let ((pos (point)))
    (if (= pos (region-end))
        (- pos 1)
      (region-end))))

(defun block-indent--current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun block-indent-tab-string ()
  "Returns a string representing a tab"
  (if indent-tabs-mode "\t" (make-string tab-width ? )))

(defun block-indent-right-region (beg end)
  (interactive "r")
  (message "region indent")
  (indent-rigidly-right-to-tab-stop beg end)
  (block-indent--select-region beg end))

(defun block-indent-right ()
  "Indent the current line or block the 'block' way"
  (interactive)
  (message "line indent")
  (if (not (use-region-p))
      (progn
        (block-indent--select-current-line)
        (if (or (block-indent--current-line-empty-p) (= (point) (line-beginning-position)))
            (insert (block-indent-tab-string))
          (indent-rigidly-right-to-tab-stop  (region-beginning) (region-end))))
    (block-indent-right-region
     (block-indent--region-beginning-adjusted)
     (block-indent--region-end-adusted))))

(defun block-indent-left-region (beg end)
  (interactive "r")
  (indent-rigidly-left-to-tab-stop beg end)
  (block-indent--select-region beg end))

(defun block-indent-left ()
  "De-indent the current line or block the 'block' way"
  (interactive)
  (save-excursion
    (if (not (use-region-p))
        (progn
          (back-to-indentation)
          (if (= (point) (line-beginning-position))
              (when (looking-at-p (block-indent-tab-string))
                (delete-char (length (block-indent-tab-string))))
            (when (looking-back (block-indent-tab-string))
              (delete-char (- 0 (length (block-indent-tab-string)))))))
      (block-indent-left-region
       (block-indent--region-beginning-adjusted)
       (region-end)))))

;;;###autoload
(define-minor-mode block-indent-mode
  "Get your indents in the right places."
  :lighter "block-indent"
  :keymap block-indent-mode-map)

(provide 'block-indent)

;;; block-indent.el ends here
