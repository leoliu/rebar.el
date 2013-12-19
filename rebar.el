;;; rebar.el --- emacs wrapper for the erlang rebar tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.3.7
;; Keywords: erlang, tools, processes
;; Created: 2013-12-19

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Run rebar easily in emacs.
;;
;; Documentation: https://github.com/rebar/rebar/wiki

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'compile))
(autoload 'vc-responsible-backend "vc")

(defgroup rebar nil
  "An Erlang build tool."
  :link '(url-link "https://github.com/rebar/rebar")
  :group 'tools)

(defcustom rebar-config-url
  "https://raw.github.com/rebar/rebar/master/rebar.config.sample"
  "URL to the rebar configuration."
  :type 'string
  :group 'rebar)

(defcustom rebar-use-vc-root '(Git SVN Hg)
  "List of vc backends used to determine rebar project root.
If t use all backends in `vc-handled-backends'."
  :type '(choice (const :tag "All" t) (repeat symbol))
  :group 'rebar)

(defcustom rebar-templates '(("simplesrv" "srvid")
                             ("simplenode" "nodeid")
                             ("simplemod" "modid")
                             ("simplelib" "libid")
                             ("simplefsm" "fsmid")
                             ("simpleapp" "appid")
                             ("ctsuite" "testmod")
                             ("basicnif" "module"))
  "List of templates."
  :type '(repeat (repeat string))
  :group 'rebar)

(defvar rebar-cmds
  '("clean" "compile" "escriptize" "create" "create-app" "create-lib"
    "create-node" "list-templates" "doc" "check-deps" "get-deps" "update-deps"
    "delete-deps" "list-deps" "generate" "overlay" "generate-upgrade"
    "generate-appups" "eunit" "ct" "qc" "xref" "help" "version")
  "List of rebar commands.")

(defun rebar-cmds ()
  (with-temp-buffer
    (call-process "rebar" nil t nil "-c")
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^[-[:alnum:]]+" nil t)
             collect (match-string 0))))

(defun rebar-strip-string (s)
  (let* ((re "[ \t\n\r\f\v]+")
         (from (if (string-match (concat "\\`" re) s) (match-end 0) 0))
         (to (and (string-match (concat re "\\'") s) (match-beginning 0))))
    (substring s from to)))

(defun rebar-read-string (prompt &optional initial-input history default-value
                                 inherit-input-method)
  "Like `read-string' but automatically interpolate DEFAULT in PROMPT."
  (let ((prompt (if (and default-value
                         (not (string-match-p "(default .*?): *\\'" prompt)))
                    (format "%s (default %s): "
                            (if (string-match ": *\\'" prompt)
                                (substring prompt 0 (match-beginning 0))
                              prompt)
                            (or (car-safe default-value) default-value))
                  prompt)))
    (read-string prompt initial-input history default-value
                 inherit-input-method)))

(defun rebar-bin ()
  (if (file-executable-p "rebar") "./rebar" "rebar"))

(defvar rebar-find-project-function #'rebar-find-project-default
  "Function used to find rebar project root.")

(defun rebar-find-project-default ()
  (or (locate-dominating-file default-directory #'rebar-directory-p)
      (rebar-vc-root default-directory)
      (read-directory-name "Directory: " nil nil t)))

;;;###autoload
(defun rebar-directory-p (dir)
  (or (file-exists-p (expand-file-name "rebar.config" dir))
      (file-exists-p (expand-file-name "rebar.config.script" dir))
      (file-regular-p (expand-file-name "rebar" dir))))

(defun rebar-vc-root (&optional file-or-directory)
  (when rebar-use-vc-root
    (ignore-errors
      (let* ((vc-handled-backends (if (listp rebar-use-vc-root)
                                      rebar-use-vc-root
                                    vc-handled-backends))
             (file (or file-or-directory buffer-file-name default-directory))
             (backend (vc-responsible-backend file)))
        (and backend (vc-call-backend backend 'root file))))))

(defvar-local rebar-project-root nil "Internal")

;;;###autoload
(defun rebar-project-root ()
  (or rebar-project-root
      (setq rebar-project-root (funcall rebar-find-project-function))))

(defmacro rebar-ensure-directory (&rest body)
  `(let* ((default-directory (rebar-project-root))
          (process-environment
           (cons (concat "PATH=" (concat default-directory ":" (getenv "PATH")))
                 process-environment)))
     ,@body))

(defvar rebar-compilation-finish-functions nil)

(define-compilation-mode rebar-compilation-mode "Rebar" nil
  (rebar-mode 1))

(defun rebar-start (&rest cmds)
  (rebar-ensure-directory
   (let* ((cmd (concat (rebar-bin) " "
                       (mapconcat 'identity (cl-remove-if #'null cmds) " ")))
          (cmd (if (string-match-p "skip_deps\\|help" cmd)
                   cmd
                 (concat cmd " skip_deps=true"))))
     (compilation-start cmd 'rebar-compilation-mode))))

;;;###autoload
(defun rebar-install-config ()
  (interactive)
  (rebar-ensure-directory
   (and (file-exists-p "rebar.config.sample")
        (error "File rebar.config.sample already exists"))
   (with-temp-buffer
     (url-insert-file-contents rebar-config-url)
     (write-region (point-min) (point-max) "rebar.config.sample"
                   nil nil nil 'ask))))

;;;###autoload
(defun rebar (cmd)
  "Run rebar CMD."
  (interactive (list (completing-read "Cmd: " rebar-cmds nil t)))
  (let ((fn (intern-soft (format "rebar-%s" cmd))))
    (cond
     ((commandp fn) (call-interactively fn))
     ((equal cmd "version")
      (message "%s" (rebar-strip-string
                     (shell-command-to-string "rebar version"))))
     (t (rebar-start cmd)))))

(defun rebar-help (cmd)
  (interactive (list (completing-read "Help on cmd: " rebar-cmds)))
  ;; To avoid asking for project root.
  (let ((rebar-project-root default-directory)
        (compilation-scroll-output nil))
    (rebar-start "help" cmd)))

(defun rebar-create--read-id (prompt)
  (rebar-ensure-directory
   (let ((default (file-name-nondirectory
                    (directory-file-name default-directory))))
     (rebar-read-string prompt nil nil default))))

;;;###autoload
(defun rebar-create (template &rest vars)
  (interactive
   (let* ((template-id (completing-read "Template: " rebar-templates))
          (variables (cdr (assoc template-id rebar-templates)))
          (answers (mapcar
                    (lambda (var)
                      (let ((ans (rebar-create--read-id (capitalize var))))
                        (when (and ans (not (equal ans "")))
                          (format "%s=%s" var ans))))
                    variables)))
     `(,template-id ,@answers)))
  (apply #'rebar-start "create" (concat "template=" template) vars))

;;;###autoload
(defun rebar-create-app (appid)
  (interactive (list (rebar-create--read-id "Appid")))
  (rebar-start "create-app" (concat "appid=" appid)))

;;;###autoload
(defun rebar-create-lib (libid)
  (interactive (list (rebar-create--read-id "Libdir")))
  (rebar-start "create-lib" (concat "libid=" libid)))

;;;###autoload
(defun rebar-create-node (nodeid)
  (interactive (list (rebar-create--read-id "Nodeid")))
  (rebar-start "create-node" (concat "nodeid=" nodeid)))

;;;###autoload
(defun rebar-compile (&optional clean)
  (interactive "P")
  (rebar-start (and clean "clean") "compile"))

(defvar-local rebar-test-suite nil)

(defun rebar-set-test-suite ()
  (let* ((suite (when buffer-file-name
                  (file-name-nondirectory
                   (if (string-match "\\(?:_tests\\|_SUITE\\).erl" buffer-file-name)
                       (substring buffer-file-name 0 (match-beginning 0))
                     (file-name-sans-extension buffer-file-name)))))
         (suites (rebar-read-string "Test suites: " nil nil suite)))
    (setq rebar-test-suite
          (unless (equal suites "")
            (format "suites=%S"
                    (mapconcat 'identity
                               (split-string (substring-no-properties suites)
                                             ",[ \t]*" t)
                               ","))))))

;;;###autoload
(defun rebar-eunit (&optional test-suite)
  (interactive "P")
  (and test-suite (rebar-set-test-suite))
  (rebar-start "eunit" rebar-test-suite))

;;;###autoload
(defun rebar-ct (&optional test-suite)
  (interactive "P")
  (and test-suite (rebar-set-test-suite))
  (rebar-start "ct" rebar-test-suite))

(defvar rebar-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-K" 'rebar-compile)
    (define-key m "\M-N" 'rebar-create)
    (define-key m "\M-R" 'rebar)
    (define-key m "\M-T" 'rebar-ct)
    (define-key m "\M-U" 'rebar-eunit)
    m))

;;;###autoload
(define-minor-mode rebar-mode nil
  :lighter (:eval (if global-rebar-mode "" " RB")))
;;;###autoload
(define-minor-mode global-rebar-mode nil
  :lighter " GRB" :global t :keymap rebar-mode-map)

(defvar rebar-menu-items '(("Rebar"
                            (("Run Command" rebar)
                             ("Command help" rebar-help)
                             ("Compile" rebar-compile)
                             ("Create Application" rebar-create-app)
                             ("Create" rebar-create)
                             ("EUnit" rebar-eunit)
                             ("Common Test" rebar-ct))))
  "See `erlang-menu-base-items' for documentation.")

(defun rebar-install-erlang-menu ()
  (with-no-warnings
    (setq erlang-menu-items
          (erlang-menu-add-above 'rebar-menu-items
                                 'erlang-menu-compile-items
                                 erlang-menu-items))
    (erlang-menu-init))
  (remove-hook 'erlang-load-hook 'rebar-install-erlang-menu))

(if (featurep 'erlang)
    (rebar-install-erlang-menu)
  (add-hook 'erlang-load-hook 'rebar-install-erlang-menu))

(provide 'rebar)
;;; rebar.el ends here