(defvar influx-host (or (getenv "INFLUX_HOST") "localhost"))
(defvar influx-database (or (getenv "INFLUX_DATABASE") "_internal"))
(defvar influx-precision "rfc3339")
(defvar influx-cli "/usr/bin/influx")

(defun influx ()
  (interactive)
  (let ((buffer (comint-check-proc "Influx")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'comint-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Influx*"))
       (current-buffer)))
    (unless buffer
      (apply 'make-comint-in-buffer "Influx" buffer
             influx-cli nil (list "-host" influx-host "-database" influx-database "-precision" influx-precision)))))

(provide 'influx)
