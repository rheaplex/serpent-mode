;;; serpent-mode.el - Ethereum Serpent major mode
;;
;; Copyright (C) 2014 Rob Myers <rob@robmyers.org>
;; Indentation code from python.el
;; Copyright (C) 2003-2013 Free Software Foundation, Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation
;;
;; To install, add this file to your Emacs load path.
;; You can then load it using M-x serpent-mode
;; Alternatively you can have Emacs load it automatically for files with
;; a .strike extension by adding the following to your .emacs file:
;;
;;    (require 'serpent-mode)
;;    (add-to-list 'auto-mode-alist '("\\.se$" . serpent-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst serpent-font-lock-keywords
  '(
    ;; Keywords
    ("\\<\\(?:code\\|el\\(?:if\\|se\\)\\|i\\(?:f\\|nit\\)\\|return\\|s\\(?:top\\|uicide\\)\\|while\\)\\>"
     . font-lock-keyword-face)
    ;; Built-in variables
    ("\\<\\(?:block\\.\\(?:coinbase\\|difficulty\\|gaslimit\\|number\\|prevhash\\|timestamp\\)\\|contract\\.\\(?:\\(?:balanc\\|storag\\)e\\)\\|msg\\.\\(?:data\\(?:size\\)?\\|sender\\|value\\)\\|tx\\.\\(?:gas\\(?:price\\)?\\|origin\\)\\)\\>"
     . font-lock-variable-name-face)
    ;; Built-in-functions
    ("\\<\\(\\(?:array\\|bytes?\\|create\\|getch\\|msg\\|s\\(?:e\\(?:nd\\|tch\\)\\|ha3\\)\\)\\)\\s-*("
     . font-lock-builtin-face)
  )
  "Highlighting for Serpent major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom serpent-indent-offset 4
  "Default indentation offset for Serpent."
  :group 'serpent
  :type 'integer
  :safe 'integerp)

(defcustom serpent-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `serpent-indent-line' call."
  :type '(repeat symbol)
  :group 'serpent)

(defvar serpent-indent-current-level 0
  "Current indentation level `serpent-indent-line-function' is using.")

(defvar serpent-indent-levels '(0)
  "Levels of indentation available for `serpent-indent-line-function'.")

(defconst serpent-block-start
  "^\\s-*\\(code\\|elif\\|else\\|if\\|init\\|while\\)\\>"
  "A regex to match starts of blocks for serpent-indent-calculate-indentation")

;; FIXME: de-indent after functions that end execution
;;(defconst serpent-block-start "^\\s-*\\(return\\|stop\\|suicide\\)"
;;  "A regex to match ends of blocks for serpent-indent-calculate-indentation")

(defun serpent-indent-calculate-indentation ()
  "Calculate correct indentation offset for the current line."
  (interactive)
  (let ((indent 0))
    (save-excursion
      (beginning-of-line)
      ;; At start of buffer? No previous line, so indent is zero
      (unless (bobp)
        ;; Force init:/code: to the left
        (if (looking-at "^\\s-*\\(init\\|code\\)\\>")
            (setf indent 0)
          ;; Otherwise...
          (progn
            ;; Dedent else:/elif:
            (when (looking-at "\\s-*\\(elif\\|else\\)\\>")
              (print "else")
              (decf indent serpent-indent-offset)
              (print indent))
            ;; Then use the indentation of the previous line
            (forward-line -1)
            ;; search-forward updates point, so save excursion again
            (save-excursion
              (incf indent
                    (count-matches "\\s-"
                                   (line-beginning-position)
                                   (re-search-forward "[^[:space:]]"
                                                      (line-end-position)))))
            (when (looking-at serpent-block-start)
              (incf indent serpent-indent-offset))))))
    (print indent)
    indent))

(defun serpent-indent-calculate-levels ()
  "Calculate `serpent-indent-levels' and reset `serpent-indent-current-level'."
  (let* ((indentation (serpent-indent-calculate-indentation))
         (remainder (% indentation serpent-indent-offset))
         (steps (/ (- indentation remainder) serpent-indent-offset)))
    (setq serpent-indent-levels (list 0))
    (dotimes (step steps)
      (push (* serpent-indent-offset (1+ step)) serpent-indent-levels))
    (when (not (eq 0 remainder))
      (push (+ (* serpent-indent-offset steps) remainder) serpent-indent-levels))
    (setq serpent-indent-levels (nreverse serpent-indent-levels))
    (setq serpent-indent-current-level (1- (length serpent-indent-levels)))))

(defun serpent-indent-toggle-levels ()
  "Toggle `serpent-indent-current-level' over `serpent-indent-levels'."
  (setq serpent-indent-current-level (1- serpent-indent-current-level))
  (when (< serpent-indent-current-level 0)
    (setq serpent-indent-current-level (1- (length serpent-indent-levels)))))

(defun serpent-indent-line (&optional force-toggle)
  "Internal implementation of `serpent-indent-line-function'.
Uses the offset calculated in
`serpent-indent-calculate-indentation' and available levels
indicated by the variable `serpent-indent-levels' to set the
current indentation."
  (or
   (and (or (and (memq this-command serpent-indent-trigger-commands)
                 (eq last-command this-command))
            force-toggle)
    (not (equal serpent-indent-levels '(0)))
    (or (serpent-indent-toggle-levels) t))
   (serpent-indent-calculate-levels))
  (let* ((starting-pos (point-marker))
         (indent-ending-position
          (+ (line-beginning-position) (current-indentation)))
         (follow-indentation-p
          (or (bolp)
              (and (<= (line-beginning-position) starting-pos)
                   (>= indent-ending-position starting-pos))))
         (next-indent (nth serpent-indent-current-level serpent-indent-levels)))
    (unless (= next-indent (current-indentation))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to next-indent)
      (goto-char starting-pos))
    (and follow-indentation-p (back-to-indentation)))
  ;;(serpent-info-closing-block-message)
  )

(defun serpent-indent-line-function ()
  "`indent-line-function' for Serpent mode.
See `serpent-indent-line' for details."
  (serpent-indent-line))

(defun serpent-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and ;;(not (serpent-syntax-comment-or-string-p))
             (<= (point-marker) (save-excursion
                                  (back-to-indentation)
                                  (point-marker)))
             (> (current-column) 0))
    (serpent-indent-line t)
    t))

(defun serpent-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is  not in between the indentation."
  (interactive "*p")
  (when (not (serpent-indent-dedent-line))
    (backward-delete-char-untabify arg)))
(put 'serpent-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun serpent-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `serpent-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count serpent-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))
(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun serpent-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `serpent-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (if count
        (setq count (prefix-numeric-value count))
      (setq count serpent-indent-offset))
    (indent-rigidly start end count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom serpent-executable "serpent"
  "The command to call to compile .se files."
  :type 'boolean
  :group 'serpent-mode)

(defun serpent-buffer-file-command (command)
  "Call the serpent executable on the file for the current buffer."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Serpent*"))
        (filename (buffer-file-name)))
    (with-current-buffer output-buffer
      (erase-buffer))
    (cond
     ((not filename)
      (message "Cannot use non-file buffer."))
     ((buffer-modified-p)
      (message "Please save changes before compiling."))
     (t (shell-command
         (concat serpent-executable " " command " \"" (buffer-file-name) "\"")
         output-buffer)))))

(defun serpent-compile-to-assembly-current-buffer ()
  (interactive)
  (serpent-buffer-file-command "compile_to_assembly"))

(defun serpent-compile-current-buffer ()
  (interactive)
  (serpent-buffer-file-command "compile"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Splitting strings into 32 character runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substrings32 (source)
  "Break source up into a list of substrings, each at most 32 characters long."
  (let ((string-length (length string))
	(string-pos 0)
	(substrings '()))
    (while (< string-pos string-length)
      (setf substrings (cons (substring-no-properties source
						      string-pos
						      (min (+ string-pos 32)
							   string-length))
			     substrings)))
    (reverse substrings)))

(defun substring32format (substrings)
  (concat "["
	  (mapconcat 'identity substrings "\", \"")
	  "]"))

(defun stripsubstring32format (source)
  ;; Strip start of list, if any
  (replace-regexp-in-string "^\\s-*[\\s-*" ""
     ;; Strip end of list, if any
     (replace-regexp-in-string "^\\s-*]\\s-*" ""
	;; Strip string separators, if any (and not quoted)
	(replace-regexp-in-string "[^\\]*\"[^\\]*,\\s-*[^\\]*\"" "" source))))

     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar serpent-syntax-table
  (let ((st (make-syntax-table)))
    ;; Dots are valid word constituents
    (modify-syntax-entry ?. "_" st)
    ;; Colons are whitespace
    (modify-syntax-entry ?: "-" st)
    ;; c++-style comments: //
    (modify-syntax-entry ?\/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for Serpent major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst serpent-mode-map (make-keymap)
  "Keymap for Serpent major mode.")
(define-key serpent-mode-map (kbd "C-x C-a") 'serpent-compile-to-assembly-current-buffer)
(define-key serpent-mode-map (kbd "C-x C-e") 'serpent-compile-current-buffer)
;; Indent specific
(define-key serpent-mode-map "\177" 'serpent-indent-dedent-line-backspace)
(define-key serpent-mode-map (kbd "<backtab>") 'serpent-indent-dedent-line)
(define-key serpent-mode-map "\C-c<" 'serpent-indent-shift-left)
(define-key serpent-mode-map "\C-c>" 'serpent-indent-shift-right)
;;(define-key serpent-mode-map ":" 'serpent-indent-electric-colon)
(define-key serpent-mode-map "\C-j" 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode serpent-mode prog-mode "serpent"
  "Major mode for editing Serpent programming language files.
\\{serpent-mode-map}"
  :group 'serpent-mode
  :syntax-table serpent-syntax-table
  :keymap serpent-mode-map
  (set (make-local-variable 'font-lock-defaults)
       '(serpent-font-lock-keywords))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (setq-local indent-line-function 'serpent-indent-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide the mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'serpent-mode)
