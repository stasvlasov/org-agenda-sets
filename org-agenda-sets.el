(defvar org-agenda-sets-file (concat user-emacs-directory "org-agenda-sets.el")
  "Where to store agenda sets (lists of agenda files).")
(defvar org-agenda-sets nil "List of (SET-NAME AGENDA-FILES).")
(defvar org-agenda-sets-definitions nil "List of (SET-NAME SET-RECIPE).")

(require 'dash)
(require 'f)

(defmacro org-agenda-sets-define (name &rest recipe)
  "Adds or updates agenda set's definition (NAME (RECIPE)) to `org-agenda-sets-definitions'"
  `(setf (alist-get (quote ,name) org-agenda-sets-definitions) (quote (,recipe))))

(defun org-agenda-sets--:arg (name args &optional to-list)
  "Gets value from ARGS list that follows NAME element. TO-LIST option ensures that the value wrapped as a list."
      (let ((i (-elem-index name args)))
        (when-let ((i i)
                   (arg (nth (1+ i) args)))
          (if (and to-list
                   (not (listp arg)))
              (list arg)
            arg))))

(defun org-agenda-sets-make-regexp (args)
  "Makes regexp form a ARGS list (:exact VALS :regex VALS :fixed VALS :ends VALS :begins VALS) in any order. VALS are either string or list of strings. If only (VALS) are provided as ARG then it treats it as (:exact VALS)"
  (when args 
    (let* ((args (if (and (listp args)
                          (not (-all? 'stringp args)))
                     args
                   (list :exact args)))
           (exact (->> (org-agenda-sets--:arg :exact args 'to-list)
                       (-map 'regexp-quote)
                       (--map (concat "^" it "$"))))
           (regex (org-agenda-sets--:arg :regex args 'to-list))
           (fixed (->> (org-agenda-sets--:arg :fixed args 'to-list)
                       (-map 'regexp-quote)))
           (ends (->> (org-agenda-sets--:arg :ends args 'to-list)
                      (-map 'regexp-quote)
                      (--map (concat it "$"))))
           (ext (->> (org-agenda-sets--:arg :ext args 'to-list)
                     (-map 'regexp-quote)
                     (--map (concat "\\." it "$"))))
           (begins (->> (org-agenda-sets--:arg :begins args 'to-list)
                        (-map 'regexp-quote)
                        (--map (concat "^" it)))))
      ;; return a single string
      (-> (-concat exact regex fixed ends begins ext)
          (string-join "\\|")))))

;; (org-agenda-sets-make-regexp '(:ends ("lola" "bar") :exact "la.la"))
;; (org-agenda-sets-make-regexp '(:ends ("lola" "bar")))
;; (org-agenda-sets-make-regexp "lola")
;; (org-agenda-sets-make-regexp '("lola" "bar"))
;; (org-agenda-sets-make-regexp nil)
;; (org-agenda-sets-make-regexp '(:ext "org"))
;; (org-agenda-sets-make-regexp '(:ext ("org" "orgz")))

(defun org-agenda-sets-get (names)
  "Return set of agenda-files by name from `org-agenda-sets'. List of files is expected to be third element in each named (first element is a name) list in `org-agenda-sets'."
  (-flatten (--map (alist-get it org-agenda-sets) (-flatten names))))

;; (org-agenda-sets-get '(name2 name3))
;; (org-agenda-sets-get '(name2 name1))
;; (org-agenda-sets-get 'name1)
;; (org-agenda-sets-get nil)

;; (setq org-agenda-sets
      ;; '((name1
         ;; (1 2 3 4))
        ;; (name2
         ;; (4 5 6 7))))

(defun org-agenda-sets-match-p (regex str &optional regex-nil)
  "Matches file or dir (STR) with REGEX. Returns REGEX-NIL if REGEX or STR is nil."
    (if-let ((regex-p (stringp regex))
             (str (f-filename str)))
          (string-match-p regex str)
      regex-nil))

(defun org-agenda-sets-make (recipe &optional quite)
  "Define `agenda-files' set. Checks if the variable."
  (interactive)
  (let* ((dir
          (-filter 'file-exists-p (org-agenda-sets--:arg :dir recipe 'to-list)))
         (dir-remove
          (org-agenda-sets-make-regexp (org-agenda-sets--:arg :dir-remove recipe)))
         (dir-filter
          (org-agenda-sets-make-regexp (org-agenda-sets--:arg :dir-filter recipe)))
         (files
          (org-agenda-sets--:arg :files recipe))
         (files-remove
          (org-agenda-sets-make-regexp (org-agenda-sets--:arg :files-remove recipe)))
         (files-filter
          (org-agenda-sets-make-regexp (org-agenda-sets--:arg :files-filter recipe)))
         (sets
          (org-agenda-sets-get (org-agenda-sets--:arg :sets recipe)))
         (sets-remove
          (org-agenda-sets-get (org-agenda-sets--:arg :sets-remove recipe)))
         (sets-filter
          (org-agenda-sets-get (org-agenda-sets--:arg :sets-filter recipe))))
    ;; get dirs
    (--> (when dir
          (->> dir
              (--map (f-directories it nil t))
              (-flatten)
              (-concat dir)
              (--remove (org-agenda-sets-match-p dir-remove it))
              (--filter (org-agenda-sets-match-p dir-filter it t))
              (--map (f-files it))
              (-flatten)))
        ;; add sets
        (-concat sets it)
        (-difference it sets-remove)
        (if sets-filter (-intersection it sets-filter) it)
        (--remove (org-agenda-sets-match-p files-remove it) it)
        (--filter (org-agenda-sets-match-p files-filter it t) it)
        ;; insert files directly specified
        (-concat files it)
        ;; message
        (prog1 it
          (unless quite
            (message "%s - %s files found" (current-message) (length it)))))))

;; (org-agenda-sets-make '(:dir ("~/org/music" "~/org/people")
                         ;; :files-filter (:ext "org")))

(defun org-agenda-sets-scan (&optional sets quite)
  "Scans SETS using recipies. SETS should be in form of list of (NAME RECIPE). If SETS is not provided use `org-agenda-sets-definitions'."
  (interactive)
  (let ((sets (if sets sets
                ;; reverse because set definition adds set in front
                (seq-reverse org-agenda-sets-definitions))))
    (dolist (s sets)
      (unless quite
        (message "Scanning set: %s" (car s)))
      ;; modifies org-agenda-sets
      (setf (alist-get (car s) org-agenda-sets nil 'remove)
            (org-agenda-sets-make (cadr s))))))

;; (org-agenda-sets-scan)

;; adds
;; (org-agenda-sets-scan '((lala (:dir "~/org/music"
                           ;; :files-filter (:ext "org")))))

;; removes
;; (setf (alist-get 'lala org-agenda-sets nil 'remove) nil)


(defun org-agenda-sets-save (&optional quite)
  "Saves `org-agenda-sets' to `org-agenda-sets-file' file."
  (interactive)
  (with-temp-file org-agenda-sets-file
      (insert "(setq org-agenda-sets")
      (newline)
      (insert "'" (pp org-agenda-sets) ")"))
  (unless quite
    (message "Wrote org-agenda-sets to %s" org-agenda-sets-file)))

(defun org-agenda-sets (&optional reload rescan sets)
  "Returns `org-agenda-sets' list if it is not nil. If it is nil or RELOAD is set then attempt to load from `org-agenda-sets-file'. If load fails or RESCAN is set then attempt to rescan files with `org-agenda-sets-scan' (optional SETS is passed there) and rewrite `org-agenda-sets-file' with new value of `org-agenda-sets'."
  (when (or rescan
            (and (or reload (not org-agenda-sets))
                 (not (load org-agenda-sets-file 'no-error 'no-message))
                 (y-or-n-p "Build org-agenda-sets-file?")))
    ;; reset
    (setq org-agenda-sets nil)
    ;; scan and fill org-agenda-sets
    (org-agenda-sets-scan sets)
    ;; write results to file
    (org-agenda-sets-save))
  ;; return
  org-agenda-sets)

(defun org-agenda-sets-reload ()
  "Sets `org-agenda-sets' from `org-agenda-sets-file'. Rebuild if no file was found."
  (interactive)
  (org-agenda-sets 'reload)
  (message "Agenda sets reloaded from disk."))

(defun org-agenda-sets-rebuild ()
  "Rebuilds `org-agenda-sets' and saves it to `org-agenda-sets-file'."
  (interactive)
  (org-agenda-sets nil 'rescan)
  (message "Agenda sets rebuild."))

(defun org-agenda-sets-eq (symb name)
  (string= (symbol-name symb) name))

(defun org-agenda-sets-use (s)
  "Select agenda set from `org-agenda-sets' (rebuild if needed) and assign it to `org-agenda-files'."
  (interactive
   (list (completing-read
          "Chose agenda files set to use"
          (mapcar 'car (org-agenda-sets)))))
  (setq org-agenda-files
        (alist-get s org-agenda-sets nil nil 'org-agenda-sets-eq))
  (message "%s files are used for Org Agenda" (length org-agenda-files)))

(defun org-agenda-sets-add (s)
  "Select agenda set from `org-agenda-sets' (rebuild if needed) and add it to `org-agenda-files'."
  (interactive
   (list (completing-read
          "Chose agenda files set to add"
          (mapcar 'car (org-agenda-sets)))))
  (setq org-agenda-files
        (append org-agenda-files
                (alist-get s org-agenda-sets nil nil 'org-agenda-sets-eq)))
  (message "%s files are used for Org Agenda" (length org-agenda-files)))


(defun org-agenda-sets-remove (s)
  "Select agenda set from `org-agenda-sets' (rebuild if needed) and subtract it from `org-agenda-files'."
  (interactive
   (list (completing-read
          "Chose agenda files set to remove"
          (mapcar 'car (org-agenda-sets)))))
  (setq org-agenda-files
        (-difference org-agenda-files
                     (alist-get s org-agenda-sets nil nil 'org-agenda-sets-eq)))
  (message "%s files are used for Org Agenda" (length org-agenda-files)))


(defun org-agenda-sets-overlap (s)
  "Select agenda set from `org-agenda-sets' (rebuild if needed) and overlap it with `org-agenda-files'."
  (interactive
   (list (completing-read
          "Chose agenda files set to overlap with"
          (mapcar 'car (org-agenda-sets)))))
  (setq org-agenda-files
        (-intersection org-agenda-files
                     (alist-get s org-agenda-sets nil nil 'org-agenda-sets-eq)))
  (message "%s files are used for Org Agenda" (length org-agenda-files)))

(provide 'org-agenda-sets)
