(add-to-list 'load-path ts-emacs-dir)

(when window-system
  (tool-bar-mode -1)
  (load-file (concat ts-emacs-dir "/github.com/cyberpunk-theme.el/cyberpunk-theme.el"))
  )

(defvar local-domains nil "List of local domains")

(let ((local-settings (concat ts-emacs-dir "/local.el")))
  (when (file-exists-p local-settings)
    (load-library local-settings)))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/dash.el"))
(eval-after-load 'dash '(dash-enable-font-lock))
(require 'dash)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/restclient.el"))
(require 'restclient)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/window-number"))
(require 'window-number)
(setq window-number-inactive-foreground (face-attribute 'mode-line :foreground)
      window-number-inactive-background (face-attribute 'mode-line :background))
(window-number-mode t)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/go-mode.el"))
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/markdown-mode"))
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/dockerfile-mode"))
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/logstash-conf.el"))
(autoload 'logstash-conf-mode "logstash-conf" nil t)
(add-to-list 'auto-mode-alist '("logstash.*\\.conf\\'" . logstash-conf-mode))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/puppet-syntax-emacs"))
(autoload 'puppet-mode "puppet-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/yaml-mode"))
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/js2-mode"))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)\\'" . js2-mode))

(when (memq window-system '(mac ns x))
  (load-file (concat ts-emacs-dir "/github.com/exec-path-from-shell/exec-path-from-shell.el"))
  (exec-path-from-shell-initialize))

(setq-default buffer-file-coding-system 'iso-8859-1-unix
              c-basic-offset 2
              js-indent-level 2
              indent-tabs-mode nil)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq save-abbrevs 'silently)

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

(when (getenv "VDI")
  (setq x-super-keysym 'meta))
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c :") 'uncomment-region)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "S-C-n") (lambda ()
                                (interactive)
                                (let ((default-directory "~"))
                                  (shell (generate-new-buffer-name "*shell*")))))

(setq visible-bell t)

(setq display-time-format "%a/%W %Y-%m-%d %H:%M")
(display-time)

(global-hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(global-font-lock-mode t)

(show-paren-mode)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

(defun spook-string ()
  (replace-regexp-in-string 
   "\n" " " 
   (replace-regexp-in-string 
    "^\n" "" 
    (replace-regexp-in-string 
     "\n$" "" 
     (with-temp-buffer (spook) (buffer-string))))))

(defun url-decode-region (start end)
  "url decode region"
  (interactive "r")
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (replace-regexp-in-string "\\+" " " (url-unhex-string text))))))

(defun shift-region (distance)
  (interactive "nDistance: ")
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

(setq calendar-week-start-day 1)

(setq mac-option-modifier nil
      mac-command-modifier 'meta)

(require 'shell)
(define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
(define-key shell-mode-map (kbd "C-n") 'comint-next-input)

(require 'tramp)
(setq tramp-default-method "ssh")
(dolist (domain local-domains)
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote domain) "\\`root\\'" "/ssh:%h:")))

(defvar user-remote-shell-history-directory
  (expand-file-name (concat user-emacs-directory "/" "shell-history" "/")))
(make-directory user-remote-shell-history-directory t)

(defun tramp-shell (method host)
  (interactive "sMethod: \nsHost: ")
  (find-file (format "/%s:%s:" method host))
  (let ((buffer (current-buffer)))
    (shell (concat method "-" host))
    (setq comint-input-ring-file-name (concat user-remote-shell-history-directory "/" host "." method))
    (comint-read-input-ring 'silent)
    (kill-buffer buffer)))

(defun ssh-shell (host)
  (interactive "sHost: ")
  (tramp-shell "ssh" host))

(defun sudo-shell (host)
  (interactive "sHost: ")
  (tramp-shell "sudo" host))

(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-sentinel (get-buffer-process (current-buffer))
                                  'shell-process-kill-buffer-sentinel)))

(defun shell-process-kill-buffer-sentinel (process state)
  (message "shell(%s): %s" (buffer-name) state)
  (if (string-match "finished" state)
      (kill-buffer (current-buffer))))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (derived-mode-p 'comint-mode)
              (comint-write-input-ring))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (loop for buffer in (buffer-list)
                  do (progn
                       (set-buffer buffer)
                       (when (derived-mode-p 'comint-mode)
                         (comint-write-input-ring))))))


(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-bash-completion"))
(require 'bash-completion)
(bash-completion-setup)

(require 'influx)

(setq shell-font-lock-keywords nil)
(setq comint-input-ring-size 50000)
(setq comint-buffer-maximum-size 100000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|^Password (will be hidden):\\s *\\'"
              "\\|^Password for [^:]+:\\s*\\'"))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-kubectx-mode-line"))
(require 'kubectx)
(add-hook 'comint-output-filter-functions (lambda (string) (kubectx-update)))
(kubectx-mode 1)

(savehist-mode 1)
(winner-mode 1)

(setq ido-ignore-buffers '("\\` " "\\`\\*tramp" "Completions\\*\\'"))
(ido-mode 'buffers)

(server-start)
