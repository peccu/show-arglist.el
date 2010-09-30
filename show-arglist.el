;;; show-arglist.el --- showing arglist of function at point

;; Copyright (C) 2010 Kentarou Shimatani

;; Author: Kentaro Shimatani <kentarou.shimatani@gmail.com>
;; Created: 1 Oct 2010
;; Keywords: function

;;; Commentary:
;; Put this into your ~/.emacs.d/init.d or ~/.emacs:

;;  (require 'show-arglist)
;;  (show-arglist-mode t)

;; It will display arglist of function at point.

;;; Code:

(defgroup arglist-showing nil
  "Showing arglist of function at point."
  :prefix "show-arglist-"
  :group 'paren-matching)

(defcustom show-arglist-delay
  (if (featurep 'lisp-float-type) (/ (float 1) (float 8)) 1)
  "*Time in seconds to delay before showing a arglist of function."
  :type '(number :tag "seconds")
  :group 'arglist-showing)

(defvar show-arglist-idle-timer nil)

;;;###autoload
(define-minor-mode show-arglist-mode
  "Toggle Show Arglist mode.
With prefix ARG, turn Show Arglist mode on if and only if ARG is positive.
Returns the new status of Show Arglist mode (non-nil means on).

When Show Arglist mode is enabled, any arglist of function is shown on header line
after `show-arglist-delay' seconds of Emacs idle time."
  :global t :group 'arglist-showing
  ;; Enable or disable the mechanism.
  ;; First get rid of the old idle timer.
  (if show-arglist-idle-timer
      (cancel-timer show-arglist-idle-timer))
  (setq show-arglist-idle-timer nil)
  ;; If show-arglist-mode is enabled in some buffer now,
  ;; set up a new timer.
  (when (memq t (mapcar (lambda (buffer)
                          (with-current-buffer buffer
                            show-arglist-mode))
                        (buffer-list)))
    (setq show-arglist-idle-timer (run-with-idle-timer
                                   show-arglist-delay t
                                   'show-arglist-function))))

(defun show-arglist-1 (function)
  "引数リストを取り出して，`header-line-format'にほり込む"
  (let* ((def (if (symbolp function)
                  (symbol-function function)
                function))
         file-name string)
    (let* ((arglist (help-function-arglist def)))
      ;; If definition is a keymap, skip arglist note.
      (unless (keymapp def)
        (let* ((use (cond
                     ((listp arglist)
                      (format "%S" (help-make-usage function arglist)))
                     ((stringp arglist) arglist)
                     ;; Maybe the arglist is in the docstring of the alias.
                     ((let ((fun function))
                        (while (and (symbolp fun)
                                    (setq fun (symbol-function fun))
                                    (not (setq usage (help-split-fundoc
                                                      (documentation fun)
                                                      function)))))
                        usage)
                      (car usage))
                     ((or (stringp def)
                          (vectorp def))
                      (format "\nMacro: %s" (format-kbd-macro def)))
                     (t "[Missing arglist.  Please make a bug report.]")))
               (high (help-highlight-arguments use "")))
          (let ((fill-begin (point)))
            (setq header-line-format (car high)))
          (let ((obsolete (and
                           ;; function might be a lambda construct.
                           (symbolp function)
                           (get function 'byte-obsolete-info))))
            (when obsolete
              (setq header-line-format (concat header-line-format " (obsolete"
                                               (when (nth 2 obsolete)
                                                 (format " since %s" (nth 2 obsolete)))
                                               ";"
                                               (if (stringp (car obsolete)) (car obsolete)
                                                 (format "use `%s' instead." (car obsolete)))
                                               ")")))))))))

(defun show-arglist-function ()
  "Display the full documentation of FUNCTION (a symbol)."
  (if show-arglist-mode
      (let ((function (function-called-at-point)))
        (unless (null function)
          (show-arglist-1 function)))
    (progn
      (setq header-line-format nil)
      (message "show done"))))

(provide 'show-arglist)

;;; show-arglist.el ends here
