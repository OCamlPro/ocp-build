;; ocp-annot-print-info-at-point : print type at point
;; ocp-annot-jump-to-definition-at-point : jump to ident definition
;; ocp-annot-jump-to-definition-at-point-other-window : jump to ident definition

(provide 'ocp-annot)
(require 'cl)

;; Customize defs

(defgroup ocp-annot nil
  "ocp-annot OCaml completion/doc tool binding configuration"
  :group 'languages)

(defcustom ocp-annot-path "ocp-annot --emacs"
  "*Path to access the ocp-annot command"
  :group 'ocp-annot :type '(file))

(defvar ocp-annot-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'ocp-annot-print-info-at-point)
    (define-key map (kbd "C-c ;") 'ocp-annot-jump-to-definition-at-point)
    (define-key map (kbd "C-c C-a") 'ocp-annot-find-alternate-file)
;    (define-key map (kbd "C-c ;") 'ocp-annot-jump-to-definition-at-point-other-window)
;    (define-key map (kbd "C-c :") 'ocp-annot-jump-to-sig-at-point-other-window)
;    (define-key map (kbd "C-c C-;") 'ocp-annot-jump-to-definition-at-point)
;    (define-key map (kbd "C-c C-:") 'ocp-annot-jump-to-sig-at-point)
    map))

(define-minor-mode ocp-annot-mode
  "OCaml auto-completion, documentation and source browsing using ocp-annot"
  :group 'ocp-annot
  :keymap ocp-annot-keymap
  )


;; ‘bufferpos-to-filepos’ and ‘filepos-to-bufferpos’ are new in Emacs 25.1.
;; Provide fallbacks for older versions.
(defalias 'ocp-annot-bufferpos-to-filepos
  (if (fboundp 'bufferpos-to-filepos)
      'bufferpos-to-filepos
    (lambda (position &optional _quality _coding-system)
      (1- (position-bytes position)))))

(defalias 'ocp-annot-filepos-to-bufferpos
  (if (fboundp 'filepos-to-bufferpos)
      'filepos-to-bufferpos
    (lambda (byte &optional _quality _coding-system)
      (byte-to-position (1+ byte)))))


;;
;;
;;
;;     Formatting positions as FILE:POS
;;
;;
;;

(defun ocp-annot-location-at-point ()
  "Location in FILE:POS format of point"
  (format "%s:%d" (buffer-file-name)
          (ocp-annot-bufferpos-to-filepos (point))))

(defun ocp-annot-bufferpos-of-location(pos)
  (ocp-annot-filepos-to-bufferpos pos))

;;
;;
;;
;;     Formatting positions as FILE:LINE:LINEPOS
;;
;;
;;

;; ocamlspot.el uses a different computation, maybe we should try the same one
;; if we find a performance problem
(defun ocp-annot-location2-at-point ()
  "Location in FILE:LINE:LINEPOS format of point"
  (let*(
        (line (count-lines (point-min) (min (1+ (point)) (point-max))))
        (line-pos
         (- (ocp-annot-bufferpos-to-filepos (point))
            (ocp-annot-bufferpos-to-filepos (line-beginning-position))))
      )
    (format "%s:%d:%d"
            (buffer-file-name) line line-pos)))

;; Some code taken from ocamlspot.el (by Jun Furuse)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; column bytes => column chars

;; This looks complicated, but we need this conversion for multi-byte characters

;; Same as (goto-line), but without unwanted side-effects.
(defun ocp-annot-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; returns the string at line
(defun ocp-annot-buffer-substring-at-line (line)
  ; no need of save-excursion
  (ocp-annot-goto-line line)
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

;; returns the first [bytes] of [str] as chars, not as bytes
(defun ocp-annot-chars-of-bytes-of-string (str bytes)
  (length
   (decode-coding-string
    (substring (encode-coding-string str buffer-file-coding-system)
               0 bytes)
    buffer-file-coding-system)))

;; returns the buffer position of [bytes] at [line]
(defun ocp-annot-pos-of-bytes-at-line (line bytes)
  ; no need of save-excursion
  (ocp-annot-goto-line line)
  (let ((pos-at-beginning-of-line (line-beginning-position))
        (chars-from-beginning-of-line
         (ocp-annot-chars-of-bytes-of-string
          (ocp-annot-buffer-substring-at-line line) bytes)))
    (+ pos-at-beginning-of-line chars-from-beginning-of-line)))

;; pos is a list : (list LINE LINEPOS)
(defun ocp-annot-bufferpos-of-location2(pos)
  (let*(
        (line (car pos))
        (line-pos (car (cdr pos)))
        )
    (save-excursion
      (ocp-annot-pos-of-bytes-at-line line line-pos))))

;;
;;
;;
;;     Calling ocp-annot
;;
;;
;;

(defvar ocp-annot-debug t)

(defun ocp-annot-debug-mode ()
  "Display command sent to ocp-annot in the *Message* buffer"
  (interactive nil)
  (if ocp-annot-debug
      (progn (message "ocp-annot debug mode disabled")
	     (setq ocp-annot-debug nil))
    (message "ocp-annot debug mode enabled")
    (setq ocp-annot-debug t)))

(defun ocp-annot-args (cmd &rest args)
  (let*(
        (cmd (list* cmd args))
        )
    (when ocp-annot-debug
      (message "%s" (mapconcat
                     (lambda (s) (format "%S" s))
                     (list* ocp-annot-path cmd) " ")))
    cmd))

(defun ocp-annot-run (cmd &rest args)
  (let* ((args (apply 'ocp-annot-args cmd args))
         (shell-command (format "exec %s \"$@\"" ocp-annot-path)))
    (with-output-to-string
      (let ((ret
             (apply 'call-process-region (point-min) (point)
                    shell-file-name
                    nil (list standard-output nil) nil
                    (list* shell-command-switch shell-command
                           ocp-annot-path args))))
        (when (= 127 ret)
          (error "Could not find the ocp-annot program %S" ocp-annot-path))))))

;;
;;
;;
;;     Printing the type of an expression
;;
;;
;;

(defun ocp-annot-get-info (location)
  (let*(
       (output (ocp-annot-run "--query-info-file-pos" location))
       (res (read-from-string output)))
    (car-safe res)))

(defvar ocp-annot-get-info-cached-location "")
(defvar ocp-annot-get-info-cached-parent ())
(defun ocp-annot-get-info-cached (location)
  (if (string= location ocp-annot-get-info-cached-location)
      (let*(
           (infos ocp-annot-get-info-cached-parent)
           (parent (cdr (assoc :parent infos)))
           )
        (if (not parent)
            (setq ocp-annot-get-info-cached-location "loc"))
        (setq ocp-annot-get-info-cached-parent parent)
          infos)
    (let*(
          (infos (ocp-annot-get-info location))
          (parent (cdr (assoc :parent infos)))
          )
      (setq ocp-annot-get-info-cached-location location)
      (setq ocp-annot-get-info-cached-parent parent)
      infos)
    ))

(defvar ocp-annot-expr-ovl (make-overlay 1 1))
(make-face 'ocp-annot-expr-face)
(set-face-doc-string 'ocp-annot-expr-face
                     "face for hilighting expressions and types")
(if (not (face-differs-from-default-p 'ocp-annot-expr-face))
    (set-face-background 'ocp-annot-expr-face "#88FF44"))
(overlay-put ocp-annot-expr-ovl 'face 'ocp-annot-expr-face)

(defun ocp-annot-print-info (location &optional to-kill)
  "Display the type of an expression in the minibuffer. If called several times, display the types of enclosing expressions."
  (let*
      (
       (target-buf (current-buffer))
       (infos (ocp-annot-get-info-cached location))
       (exception (assoc :error infos))
       )
    (if exception
        (display-message-or-buffer (cdr exception) "*ocp-annot*")
      (let*
          (
          (type (cdr (assoc :type infos)))
          (left (cdr (assoc :left infos)))
          (right (cdr (assoc :right infos)))
          )
        (move-overlay ocp-annot-expr-ovl
                      (ocp-annot-bufferpos-of-location  left)
                      (ocp-annot-bufferpos-of-location  right) target-buf)
        (display-message-or-buffer type "*ocp-annot*")
        (if to-kill (kill-new type))
        (unwind-protect (sit-for 60)
          (delete-overlay ocp-annot-expr-ovl))))))

(defun ocp-annot-print-info-at-point ()
  (interactive nil)
  (ocp-annot-print-info (ocp-annot-location-at-point))
)

(defun ocp-annot-print-info-at-point-and-copy ()
  (interactive nil)
  (ocp-annot-print-info (ocp-annot-location-at-point) t)
)

;;
;;
;;
;;     Jumping to definition
;;
;;
;;

(defun ocp-annot-query-jump (location)
  (let*(
       (output (ocp-annot-run "--query-jump-file-pos" location))
       (res (read-from-string output)))
    (car-safe res)))

(defun ocp-annot-jump (file pos)
  "Jump to a file and a position"
  (if file
      (if other-window (find-file-other-window file) (find-file file))
    )
  (goto-char (ocp-annot-bufferpos-of-location pos))
  )

(defvar ocp-annot-jump-to-definition-target-location "")
(defvar ocp-annot-jump-to-definition-source-file "")
(defvar ocp-annot-jump-to-definition-source-pos "")
(defun ocp-annot-jump-to-definition-really (location &optional other-window)
  "Jump to defintion"
  (let*
      (
       (target-buf (current-buffer))
       (source-file (buffer-file-name))
       (source-pos (point))
       (infos (ocp-annot-query-jump location))
       (exception (assoc :error infos))
       )
    (if exception
        (display-message-or-buffer (cdr exception) "*ocp-annot*")
      (let*
          (
          (ident (cdr (assoc :ident infos)))
          (pos (cdr (assoc :pos infos)))
          (file (assoc :file infos))
          )
        (ocp-annot-jump (if file (cdr file) file) pos)
        (let* (
               (target-location (ocp-annot-location-at-point))
               )
          (setq ocp-annot-jump-to-definition-target-location target-location)
          (setq ocp-annot-jump-to-definition-source-file source-file)
          (setq ocp-annot-jump-to-definition-source-pos source-pos)
          )
        )))
  )

(defun ocp-annot-jump-backward ()
  (interactive nil)
  (let* (
         (from-source-file (buffer-file-name))
         (from-source-pos (point))
         (to-source-file ocp-annot-jump-to-definition-source-file)
         (to-source-pos ocp-annot-jump-to-definition-source-pos)
         )
    (setq ocp-annot-jump-to-definition-source-file from-source-file)
    (setq ocp-annot-jump-to-definition-source-pos from-source-pos)
    (ocp-annot-jump to-source-file to-source-pos)
  ))

(defun ocp-annot-jump-to-definition (location &optional other-window)
  "Jump to the definition of an identifier. If called from the target of the jump, jump back to the origin."
  (if
      (string= location ocp-annot-jump-to-definition-target-location)
      (ocp-annot-jump-backward)
    (ocp-annot-jump-to-definition-really location other-window)
    ))

(defun ocp-annot-jump-to-definition-at-point ()
  (interactive nil)
  (ocp-annot-jump-to-definition (ocp-annot-location-at-point))
)

(defun ocp-annot-jump-to-definition-at-point-other-window ()
  (interactive nil)
  (ocp-annot-jump-to-definition (ocp-annot-location-at-point) t)
)

;;
;;
;;
;;     Jumping between interface/implementation
;;
;;
;;

(defun ocp-annot-find-alternate-file ()
  "Switch Implementation/Interface."
  (interactive)
  (let* (
         (name buffer-file-name)
         (output (ocp-annot-run "--query-alternate-file" name))
         (m0 (message (format "[%s]" output)))
         (res (read-from-string output))
         (infos (car-safe res))
         (file (cdr (assoc :file infos)))
         (kind (cdr (assoc :kind infos)))
         (should-create (assoc :create infos))
         )
    (if should-create
        (if (y-or-n-p
             (format "Create %s file %s " kind 
                     (file-name-nondirectory file)))
            (find-file file))
      (if file (find-file file)))
    )
  )
      
