(defvar agenda-sets-file (concat org-directory "/agenda-sets.el")
  "Where to store agenda sets (lists of agenda files).")
(defvar agenda-sets nil "List of (SET-NAME SET-FILES).")
(defvar agenda-sets-definitions nil "List of (SET-NAME SET-RECIPE).")

(require 'dash)
(require 'f)

(defmacro agenda-sets-define (name &rest recipe)
  "Adds or updates agenda set's definition (NAME (RECIPE)) to `agenda-sets-definitions'"
  `(setf (alist-get (quote ,name) agenda-sets-definitions) (quote (,recipe))))

(defun agenda-sets-get:arg (name args &optional to-list)
  "Gets value from ARGS list that follows NAME element. TO-LIST option ensures that the value wrapped as a list."
      (let ((i (-elem-index name args)))
        (when-let ((i i)
                   (arg (nth (1+ i) args)))
          (if (and to-list
                   (not (listp arg)))
              (list arg)
            arg))))

(defun agenda-sets-make-regexp (args)
  "Makes regexp form a ARGS list (:exact VALS :regex VALS :fixed VALS :ends VALS :begins VALS) in any order. VALS are either string or list of strings. If only (VALS) are provided as ARG then it treats it as (:exact VALS)"
  (when args 
    (let* ((args (if (and (listp args)
                          (not (-all? 'stringp args)))
                     args
                   (list :exact args)))
           (exact (->> (agenda-sets-get:arg :exact args 'to-list)
                       (-map 'regexp-quote)
                       (--map (concat "^" it "$"))))
           (regex (agenda-sets-get:arg :regex args 'to-list))
           (fixed (->> (agenda-sets-get:arg :fixed args 'to-list)
                       (-map 'regexp-quote)))
           (ends (->> (agenda-sets-get:arg :ends args 'to-list)
                      (-map 'regexp-quote)
                      (--map (concat it "$"))))
           (ext (->> (agenda-sets-get:arg :ext args 'to-list)
                     (-map 'regexp-quote)
                     (--map (concat "\\." it "$"))))
           (begins (->> (agenda-sets-get:arg :begins args 'to-list)
                        (-map 'regexp-quote)
                        (--map (concat "^" it)))))
      ;; return a single string
      (-> (-concat exact regex fixed ends begins ext)
          (string-join "\\|")))))

;; (agenda-sets-make-regexp '(:ends ("lola" "bar") :exact "la.la"))
;; (agenda-sets-make-regexp '(:ends ("lola" "bar")))
;; (agenda-sets-make-regexp "lola")
;; (agenda-sets-make-regexp '("lola" "bar"))
;; (agenda-sets-make-regexp nil)
;; (agenda-sets-make-regexp '(:ext "org"))
;; (agenda-sets-make-regexp '(:ext ("org" "orgz")))

(defun agenda-sets-get (names)
  "Return set of agenda-files by name from `agenda-sets'. List of files is expected to be third element in each named (first element is a name) list in `agenda-sets'."
  (interactive)
  (when (boundp 'agenda-sets)
    (-flatten (--map (car (alist-get it agenda-sets)) (-flatten names)))))

;; (agenda-sets-get '(name2 name3))
;; (agenda-sets-get '(name2 name1))
;; (agenda-sets-get 'name1)
;; (agenda-sets-get nil)

;; (setq agenda-sets
      ;; '((name1
         ;; (1 2 3 4))
        ;; (name2
         ;; (4 5 6 7))))

(defun agenda-sets-match-p (regex str &optional regex-nil)
  "Matches file or dir (STR) with REGEX. Returns REGEX-NIL if REGEX or STR is nil."
    (if-let ((regex-p (stringp regex))
             (str (f-filename str)))
          (string-match-p regex str)
      regex-nil))

(defun agenda-sets-make (recipe)
  "Define `agenda-files' set. Checks if the variable."
  (interactive)
  (let* ((dir (-filter 'file-exists-p (agenda-sets-get:arg :dir recipe 'to-list)))
         (dir-remove (agenda-sets-make-regexp (agenda-sets-get:arg :dir-remove recipe)))
         (dir-filter (agenda-sets-make-regexp (agenda-sets-get:arg :dir-filter recipe)))
         (files (agenda-sets-get:arg :files recipe))
         (files-remove (agenda-sets-make-regexp (agenda-sets-get:arg :files-remove recipe)))
         (files-filter (agenda-sets-make-regexp (agenda-sets-get:arg :files-filter recipe)))
         (sets (agenda-sets-get (agenda-sets-get:arg :sets recipe)))
         (sets-remove (agenda-sets-get (agenda-sets-get:arg :sets-remove recipe)))
         (sets-filter (agenda-sets-get (agenda-sets-get:arg :sets-filter recipe))))
    ;; get dirs
    (--> (when dir
          (->> dir
              (--map (f-directories it nil t))
              (-flatten)
              (-concat dir)
              (--remove (agenda-sets-match-p dir-remove it))
              (--filter (agenda-sets-match-p dir-filter it t))
              (--map (f-files it))
              (-flatten)))
        ;; add sets
        (-concat sets it)
        (-difference it sets-remove)
        (if sets-filter (-intersection it sets-filter) it)
        (--remove (agenda-sets-match-p files-remove it) it)
        (--filter (agenda-sets-match-p files-filter it t) it)
        ;; insert files directly specified
        (-concat files it))))

;; (agenda-sets-make '(:dir ("~/org/music" "~/org/people")
                         ;; :files-filter (:ext "org")))

(defun agenda-sets-scan (&optional sets)
  "Scans SETS using recipies. SETS should be in form of list of (NAME RECIPE). If SETS is not provided use `agenda-sets-definitions'."
  (interactive)
  (let ((sets (if sets sets agenda-sets-definitions)))
    (dolist (s sets)
      (setf (alist-get (car s) agenda-sets nil 'remove)
            (agenda-sets-make (cadr s))))))

;; (agenda-sets-scan)

;; adds
;; (agenda-sets-scan '((lala (:dir "~/org/music"
                           ;; :files-filter (:ext "org")))))

;; removes
;; (setf (alist-get 'lala agenda-sets nil 'remove) nil)

(defun agenda-sets (&optional reload rescan sets)
  "Returns `agenda-sets' list if it is not nil. If it is nil or RELOAD is set then attempt to load `agenda-sets-file'. If load fails or RESCAN is set then attempt to rescan files with `agenda-sets-scan' (optional SETS is passed there) and rewrite `agenda-sets-file' with new value of `agenda-sets'."
  (interactive)
  (when (or rescan
            (and (or reload (not agenda-sets))
                 (not (load agenda-sets-file 'no-error 'no-message))
                 (y-or-n-p "Build agenda-sets-file?")))
    (agenda-sets-scan sets)
    ;; write results to file
    (with-temp-file agenda-sets-file
      (insert "(setq agenda-sets")
      (newline)
      (insert "'" (pp agenda-sets) ")")))
  ;; return
  agenda-sets)

(defun agenda-sets-eq (symb name)
  (string= (symbol-name symb) name))

(defun agenda-sets-use (s)
  "Select agenda set from `agenda-sets' (rebuild if needed) and assign it to `org-agenda-files'."
  (interactive
   (list (completing-read
          "Chose agenda files set"
          (mapcar 'car (agenda-sets)))))
  (setq org-agenda-files
        (alist-get s agenda-sets nil nil 'agenda-sets-eq))
  (message "%s files are used for Org Agenda" (length org-agenda-files)))


(defun agenda-sets-add (s)
  "Select agenda set from `agenda-sets' (rebuild if needed) and add it to `org-agenda-files'."
  (interactive
   (list (completing-read
          "Chose agenda files set"
          (mapcar 'car (agenda-sets)))))
  (setq org-agenda-files
        (append org-agenda-files
                (alist-get s agenda-sets nil nil 'agenda-sets-eq)))
  (message "%s files are used for Org Agenda" (length org-agenda-files)))


(defun agenda-sets-remove (s)
  "Select agenda set from `agenda-sets' (rebuild if needed) and subtract it from `org-agenda-files'."
  (interactive
   (list (completing-read
          "Chose agenda files set"
          (mapcar 'car (agenda-sets)))))
  (setq org-agenda-files
        (-difference org-agenda-files
                     (alist-get s agenda-sets nil nil 'agenda-sets-eq)))
  (message "%s files are used for Org Agenda" (length org-agenda-files)))


(defun agenda-sets-overlap (s)
  "Select agenda set from `agenda-sets' (rebuild if needed) and overlap it with `org-agenda-files'."
  (interactive
   (list (completing-read
          "Chose agenda files set"
          (mapcar 'car (agenda-sets)))))
  (setq org-agenda-files
        (-intersection org-agenda-files
                     (alist-get s agenda-sets nil nil 'agenda-sets-eq)))
  (message "%s files are used for Org Agenda" (length org-agenda-files)))

(provide 'agenda-sets)
