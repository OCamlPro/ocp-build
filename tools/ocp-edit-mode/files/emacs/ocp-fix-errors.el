(message "Loading ocp-fix-errors.el...")

(defconst ocp-fix-errors-program "ocp-fix-errors.sh"
  "command for invoking ocp-fix-errors")

(defun ocp-fix-errors ()
  "automatically fix errors in compilation buffer"
  (interactive)
;;  (setq debug-on-error t)
  (set-buffer (get-buffer-create "*compilation*"))
  (let ((line-num (number-to-string (line-number-at-pos (point)))))
    (let ((temp-file (make-temp-file "ocpfix")))
      (write-region nil nil temp-file nil 'x nil)
      (with-temp-buffer
        (insert (shell-command-to-string
                 (concat ocp-fix-errors-program " " temp-file " " line-num)))
      (eval-buffer))
    (delete-file temp-file)
    ))
)

(defun ocp-bytes-of-point(p)
  (length (encode-coding-string
	   (buffer-substring-no-properties 1 p) buffer-file-coding-system)))

(defun ocp-byte-pos-to-pos (byte-pos)
  (let* ((bytes (1- byte-pos))
	 (pos (max 1 (/ byte-pos 6)))
	 (iter 0)
	 (b (ocp-bytes-of-point pos)))
    (while (and (< b bytes) (<= pos (point-max)))
      (setq iter (1+ iter))
      (setq pos (+ pos (max (/ (- bytes b) 6) 1)))
      (setq b (ocp-bytes-of-point pos)))
    pos))

(defun ocp-goto-char (pos)
  (goto-char (ocp-byte-pos-to-pos pos)))

(defun ocp-delete-region (s e)
  (delete-region (ocp-byte-pos-to-pos s) (ocp-byte-pos-to-pos e)))

(message "Loaded ocp-fix-errors-program")

