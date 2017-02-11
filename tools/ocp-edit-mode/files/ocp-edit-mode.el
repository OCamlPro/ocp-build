;; auto-completion for ocaml using auto-complete
;; (https://github.com/auto-complete/auto-complete) and ocp-edit-mode

(provide 'ocp-edit-mode)
(require 'cl)

;; Customize defs

(defgroup ocp-edit-mode nil
  "ocp-edit-mode OCaml completion/doc tool binding configuration"
  :group 'languages)

(defcustom ocp-edit-mode-path "ocp-edit-mode query --"
  "*Path to access the ocp-edit-mode command"
  :group 'ocp-edit-mode :type '(file))

(defcustom ocp-grep-path "ocp-grep"
  "*Path to access the ocp-grep command"
  :group 'ocp-edit-mode :type '(file))

(defcustom ocp-edit-mode-options "--show=types"
  "*Command-line parameters to add to ocp-edit-mode invocations (ex. --show=sigs)"
  :group 'ocp-edit-mode :type 'string)

(defcustom ocp-edit-mode-override-auto-complete-defaults t
  "*If set, auto-complete defaults will be reset to a sane setting in ocaml
   buffers. Disable if you prefer to configure auto-complete yourself."
  :group 'ocp-edit-mode :type 'boolean)

(defcustom ocp-edit-mode-auto-complete-workaround t
  "*Fix a bug in auto-complete whith quick-help at EOF in text mode."
  :group 'ocp-edit-mode :type 'boolean)

(defcustom ocp-edit-mode-extra-completion-sources
  (list 'ac-source-words-in-same-mode-buffers)
  "*Completion sources to enable besides ocp-edit-mode completion"
  :group 'ocp-edit-mode :type '(repeat symbol))

(defcustom ocp-edit-mode-show-help t
  "*If set, show the documentation bubble after completion (otherwise,
   the type is printed in the echo area)."
  :group 'ocp-edit-mode :type 'boolean)

(defvar ocp-edit-mode-has-auto-complete
  (require 'auto-complete nil t))

(defcustom ocp-edit-mode-use-auto-complete ocp-edit-mode-has-auto-complete
  "*If set, use `auto-complete' for completion."
  :group 'ocp-edit-mode :type 'boolean)

;; auto-complete bug workaround (complete at EOF in text mode)
(defun ocp-edit-mode-enable-ac-workaround ()
  (defun ac-menu-delete ()
    (ac-remove-quick-help)
    (when ac-menu
      (popup-delete ac-menu)
      (setq ac-menu))))

;; Completion aux functions

(defvar ac-ocp-edit-mode-current-doc nil)

(defun ocp-edit-mode-bounds-of-symbol-at-point ()
  "Matches the fully qualified identifier at point, eg [M1.M2.someval] but
   also somerecord.[M1.M2.somefield]"
  (let ((case-fold-search nil))
    (save-excursion
      (while (looking-back "\\<\\([A-Z][a-zA-Z0-9_']*\.\\)*[a-zA-Z0-9_']*"
                           (line-beginning-position) nil)
        (goto-char (match-beginning 0)))
      (when (looking-at "[a-zA-Z0-9_'.]*[a-zA-Z0-9_']")
        (cons (match-beginning 0) (match-end 0))))))

(defun ocp-edit-mode-completion-prefix-start ()
  (car-safe (ocp-edit-mode-bounds-of-symbol-at-point)))

(defun ocp-edit-mode-symbol-at-point ()
  (let ((bounds (ocp-edit-mode-bounds-of-symbol-at-point)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

;; override default prefix definition
(defun ac-prefix-symbol ()
  (ocp-edit-mode-completion-prefix-start))

(defun ocp-edit-mode-column-offset ()
  (save-excursion (let ((pt (point))) (forward-line 0) (- pt (point)))))

(defvar ocp-edit-mode-debug nil)

(defun ocp-edit-mode-debug-mode ()
  "Display command sent to ocp-edit-mode in the *Message* buffer"
  (interactive nil)
  (if ocp-edit-mode-debug
      (progn (message "ocp-edit-mode debug mode disabled")
	     (setq ocp-edit-mode-debug nil))
    (message "ocp-edit-mode debug mode enabled")
    (setq ocp-edit-mode-debug t)))

(defun ocp-edit-mode-args (cmd &rest args)
  (let*
      ((current-module (upcase-initials
                        (file-name-nondirectory
                         (file-name-sans-extension (buffer-file-name)))))
       (cmd (list* cmd ocp-edit-mode-options
                   "--full-open" current-module
                   "--context" ":"
                   args)))
    (when ocp-edit-mode-debug
      (message "%s" (mapconcat
                     (lambda (s) (format "%S" s))
                     (list* ocp-edit-mode-path cmd) " ")))
    cmd))

(defun ocp-edit-mode-run (cmd &rest args)
  (let* ((args (apply 'ocp-edit-mode-args cmd args))
         (shell-command (format "exec %s \"$@\"" ocp-edit-mode-path)))
    (with-output-to-string
      (let ((ret
             (apply 'call-process-region (point-min) (point)
                    shell-file-name
                    nil (list standard-output nil) nil
                    (list* shell-command-switch shell-command
                           ocp-edit-mode-path args))))
        (when (= 127 ret)
          (error "Could not find the ocp-edit-mode program %S" ocp-edit-mode-path))))))

(defun ac-ocp-edit-mode-candidates ()
  (let* ((output (ocp-edit-mode-run "complete" "--sexp" ac-prefix))
         (defs   (car-safe (read-from-string output))))
    (setq ac-ocp-edit-mode-current-doc defs)
    (mapcar 'car-safe defs)))

(defun ac-ocp-edit-mode-documentation (symbol)
  (let* ((info (cdr (assoc symbol ac-ocp-edit-mode-current-doc)))
         (path (cdr (assoc :path info)))
         (kind (cdr (assoc :kind info)))
         (type (cdr (assoc :type info)))
         (doc  (cdr (assoc :doc info))))
    (if doc
        (format "%s %s: %s\n---\n%s" kind path type doc)
      (format "%s %s: %s" kind path type))))

(defun ac-ocp-edit-mode-action ()
  (if ocp-edit-mode-show-help
      (ac-last-quick-help)
    (let* ((symbol (buffer-substring (ocp-edit-mode-completion-prefix-start) (point)))
           (info   (cdr (assoc symbol ac-ocp-edit-mode-current-doc)))
           (path   (cdr (assoc :path info)))
           (kind   (cdr (assoc :kind info)))
           (type   (cdr (assoc :type info))))
      (message "%s %s: %s" kind path type))))

(defun ac-ocp-edit-mode-init ()
  (setq ac-ocp-edit-mode-current-doc nil))

(defvar ac-source-ocp-edit-mode
  '((init . ac-ocp-edit-mode-init)
    (candidates . ac-ocp-edit-mode-candidates)
    (symbol . "o")
    (document . ac-ocp-edit-mode-documentation)
    (action . ac-ocp-edit-mode-action)
    ))

(defun ocp-edit-mode-setup-auto-complete ()
  (require 'auto-complete)
  (auto-complete-mode t)
  (setq ac-sources
        (cons 'ac-source-ocp-edit-mode
              ocp-edit-mode-extra-completion-sources))
  (when ocp-edit-mode-override-auto-complete-defaults
    (set (make-local-variable 'ac-auto-show-menu) t)
    (set (make-local-variable 'ac-auto-start) nil)
    (set (make-local-variable 'ac-delay) 0.0)
    (set (make-local-variable 'ac-expand-on-auto-complete) nil)
    (set (make-local-variable 'ac-ignore-case) nil)
    (set (make-local-variable 'ac-quick-help-delay) 0.2)
    (set (make-local-variable 'ac-trigger-commands) nil))
  (when ocp-edit-mode-auto-complete-workaround
    (ocp-edit-mode-enable-ac-workaround))
  (add-to-list 'ac-modes 'tuareg-mode)
  (add-to-list 'ac-modes 'caml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get and print indent details ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ocp-edit-mode-get-info (path)
  (let*
      ((format "((:kind . \"%k\")(:path . \"%p\")(:type . \"%t\")(:doc . \"%D\")(:parent . \"%e\"))")
       (output (ocp-edit-mode-run "print" path format "--separate"))
       (els (concat "(" output ")"))
       (res (read-from-string els)))
    (car-safe res)))

(defun ocp-edit-mode-format-info (i)
  "Extracts a formatted info string from an element of the
   structure read from ocp-edit-mode (e.g. through
   ocp-edit-mode-get-info, or a run with --sexp)"
  (let*
      ((kind (cdr (assoc :kind i)))
       (name (cdr (assoc :path i)))
       (type (cdr (assoc :type i)))
       (doc (cdr (assoc :doc i)))
       (propertize (lambda (a b c) a)))
    (format
     "%s %s: %s%s"
     (propertize kind 'face 'font-lock-keyword-face)
     (propertize name 'face 'font-lock-variable-name-face)
     (propertize type 'face 'font-lock-type-face)
     (if (string= doc "") ""
       (propertize (concat "\n> " doc)
                   'face 'font-lock-doc-face)))))

(defun ocp-edit-mode-print-info (ident)
  "Display the type and doc of an ocaml identifier in the echo area using
   ocp-edit-mode.
   Call twice to show the enclosing type of field records, variants and methods"
  (interactive (let ((default (ocp-edit-mode-symbol-at-point)))
                 (list
                  (read-string
                   (format "type ident (%s): " default) nil nil default))))
  (let*
      ((infos (ocp-edit-mode-get-info ident))
       (parents (cl-remove-if (lambda (i) (string= "" (cdr (assoc :parent i))))
                              infos))
       (msg
        (if (not infos) "No definition found"
          (if (and parents (equal last-command this-command))
              (mapconcat (lambda (i) (cdr (assoc :parent i))) parents "\n")
            (mapconcat 'ocp-edit-mode-format-info infos "\n")))))
    (display-message-or-buffer msg "*ocp-edit-mode*")))

(defun ocp-edit-mode-print-info-at-point ()
  (interactive nil)
  (ocp-edit-mode-print-info (ocp-edit-mode-symbol-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion-at-point support ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ocp-edit-mode-completion-data nil
  "The completion data for the last completed prefix.")

(defun ocp-edit-mode-completion-candidates (prefix)
  "Sets the data for completion of PREFIX in variable ocp-edit-mode-completion-data."
  (let* ((format "(\"%q\" (:path . \"%p\")(:type . \"%t\")(:kind . \"%k\")(:doc . \"%D\")(:loc . \"%l\"))")
         (output (ocp-edit-mode-run "complete" "--separate" "--format" format prefix))
         (data (car-safe (read-from-string (concat "(" output ")")))))
    (setq ocp-edit-mode-completion-data data)))

(defun ocp-edit-mode-completion-exit-function (candidate state)
  "Print the type of CANDIDATE in the echo area."
  (let ((info (cdr-safe (assoc candidate ocp-edit-mode-completion-data))))
    (when info (message "%s" (ocp-edit-mode-format-info info)))))

(defun ocp-edit-mode-completion-company-doc-buffer (candidate)
  (let ((info (cdr-safe (assoc candidate ocp-edit-mode-completion-data))))
    (when info
      (company-doc-buffer (ocp-edit-mode-format-info info)))))

(defun ocp-edit-mode-completion-company-docsig (candidate)
  (let ((info (cdr (assoc candidate ocp-edit-mode-completion-data))))
    (when info
      (message "%s" (ocp-edit-mode-format-info info)))))

(defun ocp-edit-mode-completion-annotation-function (candidate)
  (concat " " (cdr (assoc :kind (cdr (assoc candidate ocp-edit-mode-completion-data))))))

(defun ocp-edit-mode-completion-company-location (candidate)
  "Return the location of the definition of CANDIDATE as (FILE . LINE)."
  (let ((loc
         (cdr (assoc :loc (cdr (assoc candidate ocp-edit-mode-completion-data))))))
    (when (and loc (string-match "^\\(.*\\):\\([0-9]\+\\):\\([0-9]\+\\)$" loc))
      (let ((file (match-string 1 loc))
            (line (string-to-number (match-string 2 loc))))
        (cons file line)))))

(defun ocp-edit-mode-completion-table (fun)
  (if (fboundp 'completion-table-with-cache)
      (completion-table-with-cache fun)
    (completion-table-dynamic fun)))

(defun ocp-edit-mode-completion-at-point ()
  "Function used for `completion-at-point-functions' in `tuareg-mode' or `caml-mode'.

If `company-mode' has `company-capf' backend enabled (this is the
default in Emacs 24.4) then `company-mode' will automatically use
this function to present completions to the user."
  (let ((bounds (ocp-edit-mode-bounds-of-symbol-at-point)))
    (when bounds
      (let* ((start (car bounds))
             (end (point))
             (prefix (buffer-substring start end)))
        (list start end (ocp-edit-mode-completion-table 'ocp-edit-mode-completion-candidates)
              :exit-function 'ocp-edit-mode-completion-exit-function
              :annotation-function 'ocp-edit-mode-completion-annotation-function
              :company-doc-buffer 'ocp-edit-mode-completion-company-doc-buffer
              :company-location 'ocp-edit-mode-completion-company-location
              :company-docsig 'ocp-edit-mode-completion-company-docsig)))))

(defun ocp-edit-mode-setup-completion-at-point ()
  (add-hook 'completion-at-point-functions
            'ocp-edit-mode-completion-at-point nil 'local))

;;;;;;;;;;;;;;
;; grepping ;;
;;;;;;;;;;;;;;

(defun ocp-edit-mode-try-expand-symbol-at-point ()
  (interactive nil)
  (let ((ident (ocp-edit-mode-symbol-at-point)))
    (when ident
      (let* ((path (ocp-edit-mode-run "print" ident "\"%p\""))
             (path (car-safe (car (read-from-string (concat "(" path ")"))))))
        (if (string= path "") ident path)))))

(defun ocp-edit-mode-grep (query)
  "Search for an OCaml ident or string using ocp-grep.
Calls ocp-grep to find uses of a qualified ident in the current project.
The default query is extracted from the ident under point, qualified using ocp-edit-mode.
If the query is enclosed in double-quotes, it is understood as a POSIX regexp to
be searched within string literals.

The set of files to search in are determined by ocp-grep: it guesses the project root
and greps in any OCaml source files from there. "
  (interactive
   (let ((default (ocp-edit-mode-try-expand-symbol-at-point)))
     (list
      (read-string
       (format "grep OCaml code for (%s): " default) nil nil default))))
  (require 'grep)
  (let ((grep-use-null-device nil))
    (if (string-match-p "\".*\"" query)
        (grep (format "%s -e %s" ocp-grep-path query))
      (grep (format "%s %s" ocp-grep-path query)))))

;;;;;;;;;;;;;
;; jumping ;;
;;;;;;;;;;;;;

(defun ocp-edit-mode-jump-to-loc (loc other-window)
  (if (string-match "^\\(.*\\):\\([0-9-]\+\\):\\([0-9-]\+\\)$" loc)
      (let ((file   (match-string 1 loc))
            (line   (string-to-number (match-string 2 loc)))
            (column (string-to-number (match-string 3 loc)))
            (last-buffer (current-buffer)))
        (if (not (file-exists-p file))
            (message "Could not find source file %s" file)
          (if other-window (find-file-other-window file) (find-file file))
          (goto-char (point-min))
          (when (>= line 0) (forward-line (1- line)))
          (when (>= column 0) (forward-char column))
          (when other-window (switch-to-buffer-other-window last-buffer))))
    (message "%s" (replace-regexp-in-string "\n\+$" "" loc))))

(defun ocp-edit-mode-jump-to-definition (ident sig other-window)
  "Jump to the definition of an ocaml identifier using ocp-edit-mode"
  (interactive (let ((default (ocp-edit-mode-symbol-at-point)))
                 (list
                  (read-string
                   (format "lookup ident (%s): " default) nil nil default)
                  nil t)))
  (let* ((output (if sig (ocp-edit-mode-run "locate" "-i" ident)
                   (ocp-edit-mode-run "locate" ident)))
         (locs (split-string output "\n" t)))
    (if locs
        (progn
          (ocp-edit-mode-jump-to-loc (car locs) other-window)
          (cdr locs))
      (message "No definition found")
      nil)))

(defun ocp-edit-mode-jump (name sig other-window)
  (if (and (eq (car-safe last-command) name)
           (cdr last-command))
      (let* ((locs (cdr last-command)))
        (if locs
            (progn
              (ocp-edit-mode-jump-to-loc (car locs) other-window)
              (cdr locs))))
    (let ((next (ocp-edit-mode-jump-to-definition (ocp-edit-mode-symbol-at-point) sig other-window)))
      (setq this-command (list* name next)))))

(defun ocp-edit-mode-jump-to-definition-at-point ()
  (interactive nil) (ocp-edit-mode-jump 'ocp-edit-mode-jump-to-definition-at-point nil nil))
(defun ocp-edit-mode-jump-to-definition-at-point-other-window ()
  (interactive nil) (ocp-edit-mode-jump 'ocp-edit-mode-jump-to-definition-at-point nil t))
(defun ocp-edit-mode-jump-to-sig-at-point ()
  (interactive nil) (ocp-edit-mode-jump 'ocp-edit-mode-jump-to-sig-at-point t nil))
(defun ocp-edit-mode-jump-to-sig-at-point-other-window ()
  (interactive nil) (ocp-edit-mode-jump 'ocp-edit-mode-jump-to-sig-at-point t t))

(defun ocp-edit-mode-complete ()
  (interactive)
  (if ocp-edit-mode-use-auto-complete (auto-complete) (completion-at-point)))

(defvar ocp-edit-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'ocp-edit-mode-print-info-at-point)
    (define-key map (kbd "C-c ;") 'ocp-edit-mode-jump-to-definition-at-point-other-window)
    (define-key map (kbd "C-c :") 'ocp-edit-mode-jump-to-sig-at-point-other-window)
    (define-key map (kbd "C-c C-;") 'ocp-edit-mode-jump-to-definition-at-point)
    (define-key map (kbd "C-c C-:") 'ocp-edit-mode-jump-to-sig-at-point)
    (define-key map (kbd "C-c /") 'ocp-edit-mode-grep)
    (define-key map (kbd "C-c TAB") 'ocp-edit-mode-complete)
    map))

(defun ocp-edit-mode-setup-completion ()
  (if ocp-edit-mode-use-auto-complete (ocp-edit-mode-setup-auto-complete))
  (ocp-edit-mode-setup-completion-at-point))

(define-minor-mode ocp-edit-mode-mode
  "OCaml auto-completion, documentation and source browsing using ocp-edit-mode"
  :group 'ocp-edit-mode
  :keymap ocp-edit-mode-keymap
  (if ocp-edit-mode-mode
      (ocp-edit-mode-setup-completion)
    (when ocp-edit-mode-use-auto-complete (auto-complete-mode -1)))
  )

(add-hook 'tuareg-mode-hook 'ocp-edit-mode-mode t)
(add-hook 'caml-mode-hook 'ocp-edit-mode-mode t)
