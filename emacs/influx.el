(defvar influx-host (or (getenv "INFLUX_HOST") "localhost"))
(defvar influx-database (or (getenv "INFLUX_DATABASE") "_internal"))
(defvar influx-precision "rfc3339")
(defvar influx-cli "/usr/bin/influx")

(defun influx ()
  (interactive)
  (set-window-buffer nil (get-buffer-create "*Influx*"))
  (unless (comint-check-proc "*Influx*")
    (apply 'make-comint-in-buffer "Influx" nil
           influx-cli nil (list "-host" influx-host "-database" influx-database "-precision" influx-precision)))
  (setq comint-input-ring-file-name "~/.influx_history")
  (comint-read-input-ring))

(provide 'influx)
