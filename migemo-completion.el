;;; migemo-completion.el --- Completion with migemo -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Hironori OKAMOTO

;; Author: Hironori OKAMOTO <hrnr-okmt@outlook.jp>
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'minibuffer)
(require 'nadvice)
(require 'migemo)

(defun migemo-completion-try-completion (string table pred _point)
  (let ((directory-or-empty-string ""))
    (when minibuffer-completing-file-name
      (setf directory-or-empty-string (file-name-directory string)
	    string (file-name-nondirectory string)))
    (let* ((case-fold-search completion-ignore-case)
	   (pattern (migemo-get-pattern string))
	   (completion-regexp-list (list pattern))
	   (completion (try-completion directory-or-empty-string table pred)))
      (if (stringp completion)
	  (cons completion (length completion))
	completion))))

(defun migemo-completion--hilit-commonality (pattern completions)
  (when completions
    (let ((case-fold-search completion-ignore-case))
      (mapcar
       (lambda (str)
	 (setq str (copy-sequence str))
	 (unless (string-match pattern str)
	   (error "Internal error: %s does not match %s" pattern str))
	 (let ((beg (match-beginning 0))
	       (end  (match-end 0)))
	   (add-face-text-property beg end
				   'completions-common-part
				   nil str)
	   (when (< end (length str))
	     (add-face-text-property end (1+ end)
				     'completions-first-difference
				     nil str)))
	 str)
       completions))))

(defun migemo-completion-all-completions (string table pred _point)
  (let ((directory-or-empty-string ""))
    (when minibuffer-completing-file-name
      (setf directory-or-empty-string (file-name-directory string)
	    string (file-name-nondirectory string)))
    (let* ((case-fold-search completion-ignore-case)
	   (pattern (migemo-get-pattern string))
	   (completion-regexp-list (list pattern))
	   (all (all-completions directory-or-empty-string table pred)))
      (when all
	(nconc (migemo-completion--hilit-commonality pattern all)
	       (length string))))))

(add-to-list 'completion-styles-alist
	     '(migemo migemo-completion-try-completion
		      migemo-completion-all-completions
		      "completion with migemo"))

;;;###autoload
(define-minor-mode migemo-fido-mode
  "fido-modeでもmigemoれるように."
  :global t
  :group 'migemo-completion
  (advice-remove 'icomplete--fido-mode-setup 'migemo-completion)
  (when migemo-fido-mode
    (advice-add 'icomplete--fido-mode-setup :after
		(lambda ()
		  (setq-local completion-styles
			      (append completion-styles '(migemo))))
		'((name . migemo-completion)))))

(provide 'migemo-completion)

;;; migemo-completion.el ends here
