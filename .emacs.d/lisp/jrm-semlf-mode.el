;; Copyright 2023 Roi Martin. All rights reserved.
;; Copyright 2017 The Go Authors. All rights reserved.
;; Use of this source code is governed by a BSD-style license that can
;; be found in https://github.com/golang/proposal/blob/master/LICENSE

;; This makes fill-paragraph add line breaks at sentence boundaries in
;; addition to normal wrapping.
;;
;; It can be enabled with M-x jrm-semlf-mode
;;
;; This is sensitive to the setting of `sentence-end-double-space',
;; which defaults to t. If `sentence-end-double-space' is t, but a
;; paragraph has only a single space between sentences, this will not
;; insert line breaks where expected.

(defun jrm-fill-semlf (&optional justify)
  "Fill paragraph at point, breaking lines at sentence boundaries."
  (interactive)
  (save-excursion
    ;; Do a trial fill and get the fill prefix for this paragraph.
    (let ((prefix (or (fill-paragraph) ""))
          (end (progn (fill-forward-paragraph 1) (point)))
          (beg (progn (fill-forward-paragraph -1) (point))))
      (save-restriction
        (narrow-to-region (line-beginning-position) end)
        ;; Unfill the paragraph.
        (let ((fill-column (point-max)))
          (fill-region beg end))
        ;; Fill each sentence.
        (goto-char (point-min))
        (while (not (eobp))
          (if (bobp)
              ;; Skip over initial prefix.
              (goto-char beg)
            ;; Clean up space between sentences.
            (skip-chars-forward " \t")
            (delete-horizontal-space 'backward-only)
            (insert "\n" prefix))
          (let ((sbeg (point))
                (fill-prefix prefix))
            (forward-sentence)
            (fill-region-as-paragraph sbeg (point)))))
      prefix)))

(define-minor-mode jrm-semlf-mode
  "Make fill-paragraph add line breaks at sentence boundaries in
addition to normal wrapping."
  :lighter " SemLF"
  (setq-local fill-paragraph-function
	      (if jrm-semlf-mode
		  (progn
		    (setq-local jrm-old-fill-paragraph-function fill-paragraph-function)
		    #'jrm-fill-semlf)
		jrm-old-fill-paragraph-function)))
