;;; init.el --- Emacs initialization

;;; Commentary:
;;; https://github.com/terjesannum/setup

;;; Code:

(when window-system
  (tool-bar-mode -1)
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "/github.com/cyberpunk-theme.el"))
  (load-theme 'cyberpunk t))

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq save-abbrevs 'silently)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(let ((local-settings (expand-file-name (concat user-emacs-directory "/local.el"))))
  (when (file-exists-p local-settings)
    (load-library local-settings)))

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "/github.com/use-package"))
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package time
  :init (setq display-time-format "%a/%V %Y-%m-%d %H:%M"
              display-time-mail-file 'none)
  :config (display-time-mode 1))
(use-package calendar
  :init (setq calendar-week-start-day 1))
(use-package paren
  :init (setq show-paren-delay 0
              show-paren-style 'expression)
  :config (show-paren-mode 1))
(use-package hl-line
  :config (global-hl-line-mode 1))
(use-package savehist
  :config (savehist-mode 1))
(use-package winner
  :demand t
  :init (setq winner-dont-bind-my-keys t)
  :bind (("C-c u" . winner-undo)
         ("C-c r" . winner-redo))
  :config (winner-mode 1))
(use-package ido
  :init (setq ido-ignore-buffers '("\\` " "\\`\\*tramp" "Completions\\*\\'"))
  :config (ido-mode 'buffers))
(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))
(use-package window-number
  :config (window-number-mode 1))
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))
(use-package go-mode)
(use-package markdown-mode
  :mode (("github.*\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
(use-package dockerfile-mode)
(use-package logstash-conf
  :mode (("logstash.*\\.conf\\'" . logstash-conf-mode)))
(use-package puppet-mode)
(use-package yaml-mode)
(use-package smart-shift
  :hook (yaml-mode . smart-shift-mode))
(use-package terraform-mode)
(use-package highlight-indentation)
(use-package copy-as-format
  :bind (("C-c w s" . copy-as-format-slack)
         ("C-c w g" . copy-as-format-github)))
(use-package "js2-mode"
  :mode (("\\.\\(js\\|json\\)\\'" . js2-mode)))
(use-package scala-mode
  :interpreter ("scala" . scala-mode))
(use-package google-this
  :init (setq google-this-modeline-indicator nil)
  :config (google-this-mode 1))
(use-package browse-url
  :bind (("C-c / ." . browse-url)))
(use-package flycheck
  :init (setq flycheck-emacs-lisp-load-path 'inherit)
  :config (global-flycheck-mode))
(use-package flycheck-package
  :after (flycheck)
  :config (flycheck-package-setup))
(use-package broadcast)
(use-package bash-completion
  :commands 'bash-completion-dynamic-complete
  :init (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete))
(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-check-startup-files nil
              exec-path-from-shell-variables '("PATH" "MANPATH" "KUBECONFIG"
                                               "PERL5LIB" "PERL_LOCAL_LIB_ROOT" "PERL_MB_OPT" "PERL_MM_OPT"))
  :config (exec-path-from-shell-initialize))
(use-package password-cache
  :init (setq password-cache-expiry 3600))
(use-package auth-source
  :init (setq auth-source-save-behavior nil))
(defvar explicit-sh-args)
(use-package tramp
  :init (setq tramp-histfile-override t
              tramp-default-method "ssh"
              explicit-shell-file-name "sh"
              explicit-sh-args '("-l")))
(use-package docker-tramp)
(use-package kubernetes-tramp
  :init (setq tramp-remote-shell-executable "sh")
  :config (kubernetes-tramp-add-method))
(use-package flycheck-elm
  :after (flycheck)
  :config (flycheck-elm-setup))
(use-package elm-mode)
(use-package nginx-mode)
(use-package org
  :init (setq org-agenda-files '("~/org/"))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . 'org-capture))
  :config (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers))
(use-package org-agenda
    :ensure nil
    :after org
    :init (setq org-agenda-include-diary t
                org-agenda-custom-commands '(("p" "Private todos" todo "" ((org-agenda-files '("~/org/todo-priv.org"))))
                                             ("w" "Work todos" todo "" ((org-agenda-files '("~/org/todo-work.org")))))))
(use-package org-capture
    :ensure nil
    :after org
    :init (setq org-capture-templates '(("p" "Private todo" entry (file+headline "~/org/todo-priv.org" "Tasks") "* TODO %?\n  %i\n  %a")
                                        ("w" "Work todo" entry (file+headline "~/org/todo-work.org" "Tasks") "* TODO %?\n  %i\n  %a"))))

(add-to-list 'load-path (concat user-emacs-directory "/github.com/emacs-gcloud-mode"))
(require 'gcloud-mode)
(setq gcloud-tunnel-through-iap t)
(setq gcloud-mode-line-update-interval 10)
(gcloud-mode 1)

(add-to-list 'load-path (concat user-emacs-directory "/github.com/emacs-kubectx-mode"))
(require 'kubectx-mode)
(setq kubectx-mode-line-update-interval 1)
(kubectx-mode 1)

(add-to-list 'load-path (concat user-emacs-directory "/github.com/emacs-influxdb-mode"))
(require 'influxdb-mode)

(add-to-list 'load-path (concat user-emacs-directory "/github.com/emacs-shell"))
(require 'emacs-shell)

(setq-default c-basic-offset 2
              js-indent-level 2
              indent-tabs-mode nil)

(prefer-coding-system 'utf-8)

(defvar user-temporary-file-directory
  (expand-file-name (concat user-emacs-directory "/" "autosaves" "/")))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))


(cond ((eq window-system 'x) (when (getenv "VDI")
                               (defvar x-super-keysym) ; defined on emacs with x support
                               (setq x-super-keysym 'meta)))
      ((eq window-system 'ns) (progn
                                (defvar mac-option-modifier) ; defined on mac emacs
                                (defvar mac-command-modifier)
                                (setq mac-option-modifier nil)
                                (setq mac-command-modifier 'meta)
                                (when (string= default-directory "/")
                                  (setq default-directory (concat (getenv "HOME") "/"))))))

(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c :") 'uncomment-region)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

(setq visible-bell t)

(line-number-mode 1)
(column-number-mode 1)

(global-font-lock-mode t)

(defun spook-string ()
  "Generate string test data."
  (mapconcat 'identity
   (delete "" (split-string (with-temp-buffer (spook) (buffer-string)) "\n"))
   " "))

(defun url-decode-region (begin end)
  "URL decode text between BEGIN and END."
  (interactive "r")
  (save-excursion
    (let ((text (delete-and-extract-region begin end)))
      (insert (replace-regexp-in-string "\\+" " " (url-unhex-string text))))))

(defun shift-region (distance)
  "Shift region DISTANCE characters."
  (interactive "nDistance: ")
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  "Shift region one character to the right."
  (interactive)
  (shift-region 1))

(defun shift-left ()
  "Shift region one character to the left."
  (interactive)
  (shift-region -1))

(defun set-selective-display-at-column ()
  "Set selective display at current column."
  (interactive)
  (set-selective-display (+ 1 (current-column))))
(global-set-key (kbd "C-c $") 'set-selective-display-at-column)

(server-start)

(defvar ts-emacs-geometry nil "Emacs frame geometry: (width height x y).")

(defun ts-emacs-set-frame-geometry (&optional geometry)
  "Set Emacs frame geometry to defaults in `ts-emacs-geometry' or GEOMETRY if supplied."
  (interactive)
  (let ((geometry-list (or geometry (and (functionp ts-emacs-geometry) (funcall ts-emacs-geometry)) ts-emacs-geometry)))
    (when (and window-system geometry-list)
      (set-frame-position (selected-frame) (nth 2 geometry-list) (nth 3 geometry-list))
      (set-frame-size (selected-frame) (nth 0 geometry-list) (nth 1 geometry-list)))))

(global-set-key (kbd "C-S-l") 'ts-emacs-set-frame-geometry)

(ts-emacs-set-frame-geometry)

(add-hook 'kill-emacs-hook
          (lambda ()
            (with-current-buffer (get-buffer "*Messages*")
              (write-file (concat user-emacs-directory "/messages")))))

(setq custom-file (concat user-emacs-directory "/emacs-custom.el"))
(load custom-file :noerror)

;;; init.el ends here
