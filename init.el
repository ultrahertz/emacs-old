
(setq user-full-name "James Fletcher")
(setq user-mail-address "jamesfbsd@gmail.com")

(require 'cl)

;;; Load path
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-web-server")
(add-to-list 'load-path "~/.emacs.d/elisp/better-defaults/")
(add-to-list 'load-path "~/.emacs.d/elisp/slime/")

;;; SLIME
(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy)) 

(require 'better-defaults)

(load-theme 'wombat t)

(setq inhibit-slash-screen t
      initial-scratch-message nil
      inhibit-startup-message t)

(setq package-enable-at-startup nil)

(load "package")
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar jamesf/packages '(ac-slime
                          auto-complete
                          autopair
                          clojure-mode
                          cider
                          deft
                          erlang
                          go-mode
                          haskell-mode
                          htmlize
                          magit
                          markdown-mode
                          nodejs-repl
                          org
                          php-mode
                          restclient
                          smex
                          web-mode
                          writegood-mode
                          yaml-mode)
  "James F's default packages")

(defun jamesf/packages-installed-p ()
  (loop for pkg in jamesf/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (jamesf/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg jamesf/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq tab-width 2
      indent-tabs-mode nil)

(setq make-backup-files nil)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;;; Haskell
;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;; Smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(setq column-number-mode t)

;;; Backups
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(require 'autopair)

(require 'auto-complete-config)
(ac-config-default)

;;; ERC
(setq erc-server "irc.freenode.net" 
      erc-port 6667 
      erc-nick "jamesf"
      erc-user-full-name user-full-name
      erc-email-userid "jamesf"
      erc-prompt-for-password t)

;;; Org-mode
(require 'org)

;; activate debugging
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;; (add-to-list 'org-modules "org-habit")

(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

(setq org-log-done t
      org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-agenda-custom-commands
      '(("f" occur-tree "FIXME")))

(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t
      org-hide-leading-stars t
      org-indent t)

(setq org-agenda-files (list "~/org/todo.org"
                             "~/org/work.org"))

(add-hook 'org-mode-hook 'turn-on-font-lock)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-c i") 'org-index)

;;; Deft
(setq deft-directory "~/org/deft")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

;;; Web-server
(require 'web-server)
