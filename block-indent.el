;;; block-indent --- Indent/de-indent whole blocks under control with no magic
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

(defun indent-forward ()
  "tab two space forward"
  (interactive)
  (block-indent-right))

(defun indent-back ()
  "tab two spaces back"
  (interactive)
  (block-indent-left))

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (save-excursion
    (end-of-line) ; move to end of line
    (set-mark (line-beginning-position))))

(defun block-select-region (beg end)
  (interactive "r")
  (setq deactivate-mark nil))

(defun block-indent-right-region (beg end)
  (interactive "r")
  (message "region indent")
  (indent-rigidly-right-to-tab-stop beg end)
  (block-select-region beg end))

(defun  region-beginning-adjusted ()
  (save-excursion
    (goto-char (region-beginning))
    (line-beginning-position)))

(defun region-end-adusted ()
  (let ((pos (point)))
    (if (= pos (region-end))
        (- pos 1)
      (region-end))))

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun block-indent-tab-string ()
  "Returns a string representing a tab"
  (if indent-tabs-mode "\t" (make-string tab-width ? )))

(defun block-indent-right ()
  "Indent the current line or block the 'block' way"
  (interactive)
  (message "line indent")
  (if (not (use-region-p))
      (progn
        (select-current-line)
        (if (or (current-line-empty-p) (= (point) (line-beginning-position)))
            (insert (block-indent-tab-string))
          (indent-rigidly-right-to-tab-stop  (region-beginning) (region-end))))
    (block-indent-right-region
     (region-beginning-adjusted)
     (region-end-adusted))))

(defun block-indent-left-region (beg end)
  (interactive "r")
  (indent-rigidly-left-to-tab-stop beg end)
  (block-select-region beg end))

(defun region-length ()
  (- (region-end) (region-beginning)))

(defun block-indent-left ()
  "De-indent the current line or block the 'block' way"
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
       (region-beginning-adjusted)
       (region-end)))))

;;;###autoload
(define-minor-mode block-indent-mode
  "Get your block-indents in the right places."
  :lighter " block-indent"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<backtab>") 'indent-back)
            (define-key map (kbd "<tab>") 'indent-forward)
            (define-key map (kbd "TAB") 'indent-forward)
            map))

(provide 'block-indent)
