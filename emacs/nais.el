(defvar nais-jumphost (getenv "JUMPHOST") "Jumphost to use to reach NAIS nodes")
(defvar nais-jumphost-user (getenv "USER") "User on jumphost")
(defvar nais-remote-user nil "Remote user to use on NAIS nodes")
(defvar nais-remote-user-identity-file nil "Private key to use on NAIS ssh sessions")

(require 'tramp)

(defun nais-add-tramp-method ()
  "Add nais method to tramp"
  (add-to-list 
   'tramp-methods
   (list
    "nais"
    '(tramp-login-program "ssh")
    (list 'tramp-login-args
          (list 
           '("-t")
           (list "-l" nais-remote-user)
           (list "-i" nais-remote-user-identity-file)
           '("-e" "none")
           (list "-o" (concat "ProxyCommand='ssh -l " nais-jumphost-user " " nais-jumphost " nc %h 22'"))
           '("%h")
           '("sudo" "su" "-")))
    '(tramp-async-args
      (("-q")))
    '(tramp-remote-shell "/bin/sh")
    '(tramp-remote-shell-args
      ("-c"))
    '(tramp-gw-args
      (("-o" "GlobalKnownHostsFile=/dev/null")
       ("-o" "UserKnownHostsFile=/dev/null")
       ("-o" "StrictHostKeyChecking=no")))
    '(tramp-default-port 22))))

(provide 'nais)

