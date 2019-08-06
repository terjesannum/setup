(defvar local-domains nil "List of local domains")

(defvar user-remote-shell-history-directory
  (expand-file-name (concat user-emacs-directory "/" "shell-history" "/")))
(make-directory user-remote-shell-history-directory t)

(when (memq window-system '(mac ns x))
  (load-file (concat ts-emacs-dir "/github.com/exec-path-from-shell/exec-path-from-shell.el"))
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "KUBECONFIG"))
  (exec-path-from-shell-initialize))

(require 'tramp)
(setq tramp-default-method "ssh")
(setq explicit-shell-file-name "sh")
(setq explicit-sh-args '("-l"))
(dolist (domain local-domains)
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote domain) "\\`root\\'" "/ssh:%h:")))

(defvar tramp-shell-hook nil "Hook called before starting a tramp shell")

(defun tramp-shell (method host &optional history-name)
  (interactive "sMethod: \nsHost: ")
  (run-hook-with-args 'tramp-shell-hook method host)
  (find-file (format "/%s:%s:" method host))
  (let ((buffer (current-buffer)))
    (shell (generate-new-buffer-name (concat method "-" host)))
    (setq comint-input-ring-file-name (concat user-remote-shell-history-directory "/" (or history-name host) "." method))
    (comint-read-input-ring 'silent)
    (kill-buffer buffer)))

(defun ssh-shell (host)
  (interactive "sHost: ")
  (tramp-shell "ssh" host))

(defun sudo-shell (host)
  (interactive "sHost: ")
  (tramp-shell "sudo" host))

(load-file (concat ts-emacs-dir "/github.com/docker-tramp.el/docker-tramp.el"))
(defun docker-shell (container)
  (interactive
   (list
    (completing-read "Container: " (docker-tramp--running-containers))))
  (tramp-shell "docker" container))

(let ((tramp-remote-shell-executable "sh"))
  (load-file (concat ts-emacs-dir "/github.com/kubernetes-tramp/kubernetes-tramp.el")))

(defun pod-owner-name (pod)
  (let ((owner (car (apply #'process-lines kubernetes-tramp-kubectl-executable (list "get" "pod" pod "-o" "jsonpath={.metadata.ownerReferences[].kind}")))))
    (cond ((string= owner "ReplicaSet") (replace-regexp-in-string "-[0-9a-f]\\{8,10\\}-[0-9a-z]\\{5\\}$" "" pod))
          ((string= owner "DaemonSet") (replace-regexp-in-string "-[0-9a-z]\\{5\\}$" "" pod))
          ((string= owner "StatefulSet") (replace-regexp-in-string "-[0-9]+$" "" pod))
          (t pod))))

(defun pod-shell (pod)
  (interactive
   (list
    (completing-read "Pod: " (kubernetes-tramp--running-containers))))
  (tramp-shell "kubectl" pod (pod-owner-name pod)))

(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-sentinel (get-buffer-process (current-buffer))
                                  'shell-process-kill-buffer-sentinel)))

(defun shell-process-kill-buffer-sentinel (process state)
  (message "shell(%s): %s" (buffer-name) state)
  (kill-buffer (current-buffer)))

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

(global-set-key (kbd "S-C-n") (lambda ()
                                (interactive)
                                (let ((default-directory "~"))
                                  (shell (generate-new-buffer-name "*shell*"))
                                  (setq comint-input-ring-file-name (concat user-remote-shell-history-directory "/localhost"))
                                  (comint-read-input-ring 'silent))))

(require 'shell)
(define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
(define-key shell-mode-map (kbd "C-n") 'comint-next-input)

(add-to-list 'load-path (concat ts-emacs-dir "/github.com/emacs-bash-completion"))
(require 'bash-completion)
(bash-completion-setup)

(setq shell-font-lock-keywords nil)
(setq comint-input-ring-size 50000)
(setq comint-buffer-maximum-size 100000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-mode-hook
          '(lambda () (setq comint-input-ignoredups t)))

(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\| (will be hidden): *\\'"
              "\\|^Password for [^:]+: *\\'"
              "\\|^Enter .*password[^:]*: *\\'"))

(setq ido-ignore-buffers '("\\` " "\\`\\*tramp" "Completions\\*\\'"))

(provide 'ts-shell)
