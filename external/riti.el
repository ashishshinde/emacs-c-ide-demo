;;; riti.el --- apply riti on buffer region

;; Copyright (C) 2010 Gustavo Lima Chaves

;; Author: Gustavo Lima Chaves <com dot gmail at limachaves, in reversed order>
;; Modified: Gordon Read <com dot f2s at gtread, in reversed order>
;;           Added Customisation vars, riti-region, riti-buffer and
;;           riti-on-save hook (15/06/2011)
;; Modified: Gordon read <com dot f2s at gtread, in reversed order>
;;           Added riti-init-hooks and riti-finish-hooks
;;           (12/07/2011)
;; Website: TODO
;; Keywords: riti

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; A simple Emacs interface for the riti source code beautifier.
;; Checks your buffers for improper code indentation. It will follow
;; the indentation rules found in the specified configuration file.

;; Load this file and run:
;;
;;  M-x riti-buffer
;;
;; to indent the whole buffer or select a region and run
;;
;;  M-x riti
;;
;; to indent just the region.
;;
;; See also Customisation group "riti"

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup riti nil
  "Customization group for riti"
  :group 'riti)

(defcustom riti-cfg-file "~/.riti.xml"
  "Path to riti configuration file.\n"
  :type 'string
  :group 'riti)

(defcustom riti-include-folders "/usr/include /usr/local/include"
  "Include folder path, space separated.\n"
  :type 'string
  :group 'riti)

(defcustom riti--on-save nil
  "Whether to riti the buffer when file is saved.\n
  When non-nil, riti will be run when a cc-mode buffer is saved.
  The configuration file will be read from the specification given by
  `riti-cfg-file'."
  :type '(choice (const :tag "off" nil)
                 (const :tag "on" t))
  :group 'riti)

(defcustom riti-init-hooks nil
  "Hooks called prior to running riti."
  :type 'hook
  :group 'riti)

(defcustom riti-finish-hooks nil
  "Hooks called after running riti."
  :type 'hook
  :group 'riti)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vars

(defvar riti-path nil
  "The riti executable in path.\n
  When non-nil return value is the path to local riti")
(unless (bound-and-true-p riti-path)
  (let ((riti-path
         (or (executable-find "aerospike-riti")
             (executable-find "aerospike-riti.bat"))))
    (when riti-path (setq riti-path
                                riti-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private impl fns

(defun riti-impl (point-a point-b)
  ""
  (defvar riti-path-to-use
	(or riti-path (executable-find "aerospike-riti")))
  (run-hooks 'riti-init-hooks)

  (if riti-path-to-use
      (let* ((cmd (format "%s -p %s -s %d -e %d -I %s" riti-path-to-use
                          riti-cfg-file (- point-a 1) (- point-b 1) riti-include-folders)))
        (shell-command-on-region (point-min) (point-max) cmd t t
                                 null-device))
    (message "Riti not found in path - no change"))
  (run-hooks 'riti-finish-hooks)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public functions

(defun riti ()
  "Riti the marked region.
  The configuration file will be read from the specification given by
  `riti-cfg-file'."
  (interactive)
  (let* ((riti-current-line (line-number-at-pos)))
	(save-excursion
    	(riti-impl (region-beginning) (region-end)))
    (goto-char (region-beginning)) (forward-line (1- riti-current-line))))

(defun riti-buffer ()
  "Riti the entire buffer.
  The configuration file will be read from the specification given by
  `riti-cfg-file'. The cursor will attempt to (re)locate
  the current line, which might change as a result of the ritification."
  (interactive)
  (let* ((riti-current-line (line-number-at-pos)))
    (save-excursion
      (riti-impl (point-min) (point-max)))
    (goto-char (point-min)) (forward-line (1- riti-current-line))))

;; If riti-on-save is non nil, riti the whole buffer.
(defun riti-buffer-on-save ()
  (if riti-on-save (riti-buffer)
    (not-modified))
  nil)

;; add a c-mode-common-hook that ritifies the buffer when it is saved,
;; iff riti-on-save is non nil.
(add-hook 'c-mode-common-hook
          '(lambda()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks
                       'riti-buffer-on-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'riti)
;; riti.el ends here
