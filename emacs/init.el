(add-to-list 'load-path ts-emacs-dir)

(when window-system
  (tool-bar-mode -1)
  (load-file (concat ts-emacs-dir "/github.com/cyberpunk-theme.el/cyberpunk-theme.el"))
  )
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq save-abbrevs 'silently)

(let ((local-settings (concat ts-emacs-dir "/local.el")))
  (when (file-exists-p local-settings)
    (load-library local-settings)))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-bash-completion"))
(add-to-list 'load-path (concat ts-emacs-dir "/github.com/exec-path-from-shell"))
(add-to-list 'load-path (concat ts-emacs-dir "/github.com/docker-tramp.el"))
(add-to-list 'load-path (concat ts-emacs-dir "/github.com/kubernetes-tramp"))
(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-shell"))
(require 'emacs-shell)

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
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/smart-shift"))
(require 'smart-shift)
(add-hook 'yaml-mode-hook 'smart-shift-mode)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-hcl-mode"))
(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-terraform-mode"))
(require 'terraform-mode)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/Highlight-Indentation-for-Emacs"))
(require 'highlight-indentation)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/copy-as-format"))
(require 'copy-as-format)
(global-set-key (kbd "C-c w s") 'copy-as-format-slack)
(global-set-key (kbd "C-c w g") 'copy-as-format-github)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/js2-mode"))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)\\'" . js2-mode))

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-scala-mode"))
(autoload 'scala-mode "scala-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.s\\(cala\\|bt\\)\\'" . scala-mode))
(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-sbt-mode"))
(require 'sbt-mode)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-google-this"))
(setq google-this-modeline-indicator nil)
(require 'google-this)
(google-this-mode 1)
(global-set-key (kbd "C-c / .") 'browse-url)

(setq-default buffer-file-coding-system 'iso-8859-1-unix
              c-basic-offset 2
              js-indent-level 2
              indent-tabs-mode nil)

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

(setq visible-bell t)

(setq display-time-format "%a/%V %Y-%m-%d %H:%M")
(display-time)

(global-hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(global-font-lock-mode t)

(show-paren-mode)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

(defun spook-string ()
  (mapconcat 'identity
   (delete "" (split-string (with-temp-buffer (spook) (buffer-string)) "\n"))
   " "))

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

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-influxdb-mode"))
(require 'influxdb-mode)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-gcloud-mode-line"))
(require 'gcloud-mode-line)
(setq gcloud-update-interval 10)
(gcloud-mode 1)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-kubectx-mode-line"))
(require 'kubectx)
(setq kubectx-update-interval 1)
(kubectx-mode 1)

(savehist-mode 1)

(setq winner-dont-bind-my-keys t)
(global-set-key (kbd "C-c u") 'winner-undo)
(global-set-key (kbd "C-c r") 'winner-redo)
(winner-mode 1)

(defun set-selective-display-at-indent ()
  (interactive)
  (set-selective-display (+ 1 (current-column))))
(global-set-key (kbd "C-c $") 'set-selective-display-at-indent)

(setq ido-ignore-buffers '("\\` " "\\`\\*tramp" "Completions\\*\\'"))
(ido-mode 'buffers)

(server-start)

(defvar ts-emacs-geometry nil "Emacs frame geometry: (width height x y)")

(defun ts-emacs-set-frame-geometry (&optional geometry)
  "Set emacs frame geometry"
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
