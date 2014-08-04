;;; init.el --- -*- mode: emacs-lisp -*-
;; 
;; Filename: init.el
;; Description: my emacs config
;; Author: James Fletcher
;; Maintainer: James Fletcher
;; Created: Mon Aug  4 17:59:39 2014 (+0100)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: Mon Aug  4 18:02:30 2014 (+0100)
;;           By: James Fletcher
;;     Update #: 3
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; My ever improving emacs config. :)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
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
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar jamesf/packages '(ac-slime
                          auto-complete
                          autopair
                          bat-mode
                          csharp-mode
                          clojure-mode
                          cider
                          cmake-mode
                          conf-mode
                          deft
                          erlang
                          go-mode
                          haskell-mode
                          htmlize
                          js3-mode
                          lua-mode
                          magit
                          markdown-mode
                          nginx-mode
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

(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer
(defalias 'perl-mode 'cperl-mode) ; always use cperl-mode

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

;;; header2
(require 'header2)
(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)

(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook 'auto-make-header)
(add-hook 'php-mode-hook 'auto-make-header)
(add-hook 'js3-mode-hook 'auto-make-header)
(add-hook 'java-mode-hook 'auto-make-header)
(add-hook 'web-mode-hook 'auto-make-header)
(add-hook 'conf-mode-hook 'auto-make-header)

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

;;; auto-mode-alist
(add-to-list 'auto-mode-alist '("/crontab.*$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(add-to-list 'auto-mode-alist '("\\.[bB][aA][tT]$" . bat-mode))

;; Magically go into c++ mode (for windriver headers):
(add-to-list 'magic-fallback-mode-alist '("^// " . c++-mode))

(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.x$" . c++-mode))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))

(add-to-list 'auto-mode-alist '("/git-rebase-todo$" . conf-mode))

(add-to-list 'auto-mode-alist '("_defconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("/Kbuild$" . makefile-mode))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl" . web-mode))
(add-to-list 'auto-mode-alist '("/.gitconfig". conf-mode))
(add-to-list 'auto-mode-alist '("\.zsh" . sh-mode))
(add-to-list 'auto-mode-alist '("\.install" . sh-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
