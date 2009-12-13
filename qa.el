;;; qa.el --- Some elisp to make life easier for QA Engineers
;;; Rel:v-0-9

;; Copyright (C) 1998-1999,2004 Thien-Thi Nguyen
;; This file is part of QA.  QA comes with ABSOLUTELY NO WARRANTY.
;; QA is released under GNU GPL; see the file COPYING for details.

;;; Commentary:

;; See qa.info for documentation.

;;; Code:

(require 'cl)                           ; use the source, luke!
(require 'database)

;;;---------------------------------------------------------------------------
;;; TCDB db configuration

;; intdotint
;;
;; Currently, this is represented as a string, although we may want to
;; consider a more rigorous definition when we need to sort, etc.

(defun intdotint-validate (val rec slot db)
  (string-match "[0-9]+\\(\\.[0-9]+\\)*" val)
  ;; todo: check for uniquenes in `db'.
  (run-hook-with-args 'intdotint-validate-function val rec slot db))

(defun intdotint-to-string (x) x)
(defun string-to-intdotint (x) x)
(defun intdotint< (a b) (string< a b))
(defun intdotint= (a b) (string= a b))

(define-displaytype-from-displayspec 'intdotint 'string
  :indent           nil
  :actual->display 'intdotint-to-string
  :display->actual 'string-to-intdotint)

(define-recordfieldtype-from-recordfieldspec 'intdotint 'string
  :type                'intdotint
  :constraint-function 'intdotint-validate
  :default-value       "0"
  :actual->stored      'intdotint-to-string
  :stored->actual      'string-to-intdotint
  :order-fn            'number-order
  :sort-fn             'intdotint<
  :match-function      'intdotint=
  :help-info           "An intdotint.")

;; plist
;;
;; This is a list of the form (PROP1 VAL1 PROP2 VAL2 ...).

(defun plist-validate (val rec slot db)
  (and (listp val)
       (= 0 (% (length val) 2))))

(defun plist-to-string (x) (format "%S" x))
(defun string-to-plist (x) (read x))
(defun plist< (a b) (error "should not call plist< !"))
(defun plist= (a b) (equal a b))

(define-displaytype-from-displayspec 'plist nil
  :indent           nil
  :actual->display 'plist-to-string
  :display->actual 'string-to-plist)

(define-recordfieldtype-from-recordfieldspec 'plist nil
  :type                'plist
  :constraint-function 'plist-validate
  :default-value        nil
  :actual->stored      'plist-to-string
  :stored->actual      'string-to-plist
  :order-fn            'number-order
  :sort-fn             'plist<
  :match-function      'plist=
  :help-info           "A property list.")

;;;---------------------------------------------------------------------------
;;; Support

(defun qa-get (prop)     (get 'qa prop))
(defun qa-put (prop val) (put 'qa prop val))

(defun qa-read-file-name (prompt type &optional mustmatch)
  ;; TYPE is a symbol, usually `spec', `tcdb', `trun' or `html'.
  (let* ((all (qa-get 'files))
         (old (assq type all))
         (fn  (cdr old))
         (ret (let ((file-name-history (append (mapcar 'cdr all)
                                               file-name-history)))
                (read-file-name prompt fn fn mustmatch)))
         (new (cons type ret)))
    (if old
        (substitute new old all :test 'equal)
      (push new all))
    (qa-put 'files all)
    (push ret file-name-history)        ; save globally, too
    (expand-file-name ret)))

(defun qa-data-file (file &optional local-dir)
  (let ((local (expand-file-name file local-dir)))
    (unless (file-exists-p local)
      (copy-file (expand-file-name file
                                   (or (qa-get 'instdatadir)
                                       (file-name-directory
                                        (symbol-file 'qa-begin-session))))
                 local))
    local))

(defun qa-instantiate-template (template &optional outfile)
  ;; Both TEMPLATE and OUTFILE are file names.
  (let (enable-local-variables)
    (when outfile
      (find-file outfile)
      (erase-buffer))
    (insert-file-contents template)
    (save-match-data
      (goto-char (point-max))
      (while (re-search-backward "@.*@" (point-min) t) ; todo: handle \n
        (save-excursion
          (replace-match
           (format (progn
                     (forward-char 1)
                     (read (current-buffer)))
                   (eval (read (current-buffer)))))))) ; todo: handle exc
    (when outfile
      (save-buffer))))

;;;---------------------------------------------------------------------------
;;; Session management

(defmacro while-qa-session (&rest body)
  ;; This macro, although useful, could be better integrated w/ the
  ;; `interactive' form so that we don't need to manually read args.
  ;; Another approach is to use advice to provide protection.
  `(if (not (qa-get 'session))
       (message "(No current QA session)")
     ,@body))

(defun qa-log (msg &rest args)
  (interactive "sMessage: ")
  (while-qa-session
   (let ((f-msg (apply 'format msg args)))
     (qa-put 'session
             (concat (qa-get 'session)
                     (format-time-string "\n%c -- " (current-time)) f-msg))
     (unless (interactive-p)
       (message f-msg)))
   (when (get-buffer "*QA Session*")
     (qa-display-session-log))))

(defun qa-display-session-log ()
  (interactive)
  (while-qa-session
   (with-output-to-temp-buffer "*QA Session*" (princ (qa-get 'session)))))

(defun qa-snarf-spec-items (buffer re)
  (set-buffer buffer)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (save-match-data
      (let (items)
        (while (re-search-forward re (point-max) t)
          (push (cons (buffer-substring-no-properties
                       (match-beginning 1)
                       (match-end 1))
                      ;; Tsk tsk, specific to default `spec-item-regexp'.
                      ;; Should be reworked to look at `\2'.
                      (buffer-substring-no-properties
                       (progn (re-search-forward "\\S-")
                              (forward-char -1) (point))
                       (progn (re-search-forward "\n\\S-" (point-max) t)
                              (forward-char -1) (- (point) 1))))
                items))
        (reverse items)))))

(defun qa-begin-session (spec-file)
  (interactive (list (if (not (qa-get 'session))
                         (qa-read-file-name "Spec file: " 'spec t)
                       (qa-display-session-log)
                       (error "QA session already begun"))))
  (qa-put 'session "QA Session Log")
  (qa-log "begin session, spec file: %s" (file-name-nondirectory spec-file))
  ;; This (and the above snarf) should be a local variable.  Probably better
  ;; yet is to integrate w/ imenu.el and zzz.el so that we have a tree.
  (qa-put 'spec-item-regexp "^\\([0-9]+\\(\\.[0-9]+\\)*\\)\\s-+--")
  (let (buf si)
    (save-window-excursion
      (setq buf (find-file spec-file))
      (setq si (qa-snarf-spec-items buf (qa-get 'spec-item-regexp))))
    (let ((count (length si)))
      (qa-put 'spec-buffer buf)
      (qa-put 'spec-items si)
      ;; Save the original keymap so that we can munge it.  We do this before
      ;; checking for zero spec items (which voids the session) because if
      ;; that is the case, `qa-finish-session' expects the keymap to be there
      ;; for proper restoration.  Hmm, does Emacs have a canonical way to do
      ;; such limited-lifetime keymap overlays?
      (qa-put 'orig-database-view-mode-map
              (copy-keymap database-view-mode-map))
      (mapc (lambda (ent)
              (define-key database-view-mode-map (car ent) (cdr ent)))
            '(("\C-l" . tcdb-force-redisplay)
              (";"    . tcdb-toggle-automatic)
              ("a"    . tcdb-add-case)
              ("d"    . tcdb-delete-case)
              ("j"    . tcdb-jump)
              ("\M-o" . tcdb-other)
              ("q"    . tcdb-exit)
              ("x"    . tcdb-exit)))
      (if (> count 0)
          (qa-log "%s: snarfed %d spec items" buf count)
        (qa-log "Sorry, no spec items found in %s" buf)
        (qa-finish-session t)))))

(defun qa-finish-session (&optional force)
  (interactive "p")
  (let (open-files)
    (cond ((not (qa-get 'session))
           (message "(No current QA session to finish)"))
          (force
           (qa-put 'open-files nil)
           (qa-finish-session))
          ((setq open-files (qa-get 'open-files)) ; ugh
           (if (not (y-or-n-p "Still have open files. Close them first? "))
               (message "Not finishing, still have open files: %s" open-files)
             (mapc 'kill-buffer open-files)
             (qa-finish-session)))
          (t
           (qa-log "finished QA session")
           (setq database-view-mode-map (qa-get 'orig-database-view-mode-map))
           (qa-display-session-log)
           (setplist 'qa nil)))))

;;;---------------------------------------------------------------------------
;;; TCDB editing

(defun tcdb-display-relevance-chain ()
  (interactive)
  (while-qa-session
   (let ((spec-item-id (dbf-displayed-record-field 'spec-item-ref))
         (spec-items (qa-get 'spec-items))
         (obuf (get-buffer-create "*QA Spec Relevance Chain*"))
         lookup)
     (switch-to-buffer-other-window obuf)
     (erase-buffer)                     ; todo: optimize
     (save-match-data
       ;; This should be reimplemented once we have a tree with proper parent
       ;; pointers.  Currently very wasteful.
       (while (setq lookup (assoc spec-item-id spec-items))
         (insert (car lookup) "\t-- " (cdr lookup) "\n")
         (setq spec-item-id
               (mapconcat (lambda (x) x)
                          (reverse
                           (cdr (reverse
                                 (split-string spec-item-id "\\."))))
                          ".")))))))

(defun tcdb-prep-display-buffer (rec)
  (tcdb-display-relevance-chain)
  (other-window 1))                     ; ugh

(defun tcdb-force-redisplay ()
  (interactive)
  (while-qa-session
   (let ((dbf-redisplay-entire-record-p t))
     (dbf-redisplay-entire-record-maybe))))

(defun tcdb-toggle-automatic ()
  (interactive)
  (while-qa-session
   (let ((p (dbf-displayed-record-field 'props)))
     (setq p (plist-put p 'automatic (not (plist-get p 'automatic))))
     (dbf-displayed-record-set-field 'props p))
   (db-accept-record)
   (tcdb-force-redisplay)
   ;; This gratuitous templating may become user-customizable.
   (when (string= "" (dbf-displayed-record-field 'action))
     (db-first-field)
     (insert ";;; ")
     (db-newline 1)                     ; what does "1" mean?
     (insert "(qatr-run")
     (db-newline 1)
     (db-newline 1)
     (insert ")")
     (db-previous-line-or-field 3)
     (db-end-of-line-or-field 1))))

(defun tcdb-add-case ()
  (interactive)
  (while-qa-session
   ;; This record cursor dance is because `db-copy-record' inserts before,
   ;; instead of adding after, the current record.
   (let ((cur-sir (dbf-displayed-record-field 'spec-item-ref)))
     (db-next-record 1)
     (while (string= cur-sir (dbf-displayed-record-field 'spec-item-ref))
       (db-next-record 1))
     (db-previous-record 1))
   (let* ((seq (dbf-displayed-record-field 'subseq-num)))
     (db-copy-record 1)
     (dbf-displayed-record-set-field 'subseq-num (1+ seq))
     (dbf-displayed-record-set-field 'action "")
     (dbf-displayed-record-set-field 'expected-result "")
     (dbf-displayed-record-set-field 'props nil)
     (qa-log "Added test case %s #%d"
             (dbf-displayed-record-field 'spec-item-ref)
             (1+ seq))
     (tcdb-force-redisplay))))

(defun tcdb-delete-case ()
  (interactive)
  (while-qa-session
   (message "At this time, you cannot delete cases, sorry.")
   (tcdb-force-redisplay)))

(defun tcdb-mapirec (func)              ; EDB abstraction is poor
  (let ((idx 0))
    (maprecords (lambda (record)
                  (funcall func idx record)
                  (incf idx))
                dbc-database)))

(defun tcdb-jump (si-ref)
  (interactive (list
                (let ((cur (dbf-displayed-record-field 'spec-item-ref)))
                  (completing-read "Jump to: "
                                   (qa-get 'spec-items)
                                   'identity
                                   t cur nil cur))))
  (let ((cur (dbf-displayed-record-field 'spec-item-ref))
        (db dbc-database))
    (if (string= si-ref cur)
        (message "(Already at %s)" si-ref)
      (qa-put 'tcdb-prev cur)
      (db-jump-to-record
       ;; Linear search is probably not the best thing to do.
       (catch 'index
         (tcdb-mapirec
          (lambda (idx rec)
            (when (string= si-ref (record-field rec 'spec-item-ref db))
              (throw 'index idx)))))))))

(defun tcdb-other ()
  (interactive)
  (tcdb-jump (qa-get 'tcdb-prev)))

(defun tcdb-exit ()
  (interactive)
  (while-qa-session
   (let ((stem (buffer-name)))
     (db-exit t)
     (mapc (lambda (buf)                ; blech, should be done by EDB
             (when (string-match (buffer-name buf) stem)
               (kill-buffer buf)))
           (buffer-list))
     (kill-buffer (get-buffer-create "*QA Spec Relevance Chain*"))
     (unless (one-window-p)
       (delete-window)))))              ; todo: clean up other EDB droppings

(defun qa-edit-tcdb ()
  (interactive)
  (while-qa-session
   (let ((new-file (qa-read-file-name "Test case db: " 'tcdb)))
     (if (string= "" new-file)
         (message "(TCDB edit cancelled.)")
       (when (file-directory-p new-file)
         (error "Cannot use directory as test case db"))
       (find-file new-file)
       (qa-log "editing test case database: %s" (buffer-name))
       (when (= (buffer-size) 0)
         (let ((-*-emacs-lisp-*- "")    ; infinitessimally important
               (fresh-data
                (mapcar (lambda (spec-item)
                          ;; Tsk tsk, this requires intimacy w/ the database
                          ;; schema as well as w/ the database implementation.
                          ;; Double blech!  Do shamefaced maintenance here!
                          (vector (car spec-item) 1 "" "" 0 0 "" nil))
                        (qa-get 'spec-items))))
           (qa-instantiate-template (qa-data-file "tcdb.template")
                                    (buffer-name))
           (goto-char (point-min))
           (qa-log "(new test case db synced w/ %s)" (qa-get 'spec-buffer))))
       (if (not (looking-at ";; Database file written by EDB"))
           (message "(Not really a test case database.)")
         (let* ((curbuf (buffer-name (current-buffer)))
                (fmt (qa-data-file "tcdb.fmt"))
                (file (buffer-file-name (current-buffer))))

           (kill-buffer (current-buffer))       ; this way
           (db-find-file file fmt)              ; lies madness

           (qa-put 'open-files (cons (current-buffer) (qa-get 'open-files)))
           (setq dbf-before-display-record-function 'tcdb-prep-display-buffer)
           (qa-put 'tcdb-prev
                   (dbf-displayed-record-field 'spec-item-ref))))))))

;;;---------------------------------------------------------------------------
;;; Running tests

;; This macro is included here as a stub.  QA users should redefine it to do
;; whatever they consider useful in an automatic test case.  Having said that,
;; one can completely ignore `qatr-run' and use another form to be eval'ed.
(defmacro qatr-run (&rest allowed) t)

(defun qa-run-tests ()
  (interactive)
  (while-qa-session
   (let ((tcdb (qa-read-file-name "Test case database: " 'tcdb t))
         (trun (qa-read-file-name "Test run (output) file: " 'trun)))
     (when (let ((buf (find-buffer-visiting tcdb)))
             (and buf (buffer-modified-p buf)))
       (error "Save before continug: %s" tcdb))
     (when (file-exists-p trun)
       (unless (y-or-n-p "Output file exists, overwrite? ")
         (error "Aborting run, choose another output file.")))
     (copy-file tcdb trun t)            ; todo: exc
     (db-find-file trun
                   (qa-data-file "test-run.fmt" (file-name-directory trun)))
     (message "name now: %S" (setf (database-print-name dbc-database)
                                   (format "Test Run Starting %s for %s"
                                           (format-time-string "%Y-%m-%d %T")
                                           (qa-get 'spec-buffer))))
     (database-make-local 'tcdb dbc-database
                          (cons tcdb (file-attributes tcdb))) ; hmmm
     (database-make-local 'test-run dbc-database 'begin)
     (qa-log "starting test run")
     (qa-log "- test case db: %s" tcdb)
     (qa-log "- test run file: %s" trun))
   ;; This is the big test loop.
   (do ((i 1 (1+ i)))
       ((> i (database-no-of-records dbc-database)) nil)
     (db-jump-to-record i)
     (database-set-local 'test-run dbc-database i)
     (let* ((sir (dbf-displayed-record-field 'spec-item-ref))
            (seq (dbf-displayed-record-field 'subseq-num)))
       (qa-log "testing %s #%s" sir seq)
       (message "[f10]act<-exp,next [f11]accept,next [f12]next")
       ;; This keymap munging is done every time through the loop.  :-/
       (local-set-key [f12] 'exit-recursive-edit) ; skip
       (db-first-field)
       (local-set-key [f10] (lambda () (interactive) ; pass
                              (dbf-displayed-record-set-field
                               'actual-result
                               (dbf-displayed-record-field 'expected-result))
                              (db-accept-record)
                              (db-view-mode)
                              (exit-recursive-edit)))
       (local-set-key [f11] (lambda () (interactive) ; enter
                              (db-accept-record)
                              (db-view-mode)
                              (exit-recursive-edit)))
       (local-set-key [f12] 'exit-recursive-edit) ; skip
       ;; Either do a recursive edit (manual test case) or an eval of the
       ;; `action' field (automatic).
       (if (not (plist-get (dbf-displayed-record-field 'props) 'automatic))
           (recursive-edit)
         (dbf-displayed-record-set-field
          'actual-result
          (format "%s"
                  (condition-case whacked
                      (eval (read (dbf-displayed-record-field 'action)))
                    (error whacked))))
         (db-accept-record))))
   (database-set-local 'test-run dbc-database 'finish)
   ;; Now do scoring.  This needs to be reworked.  Currently, we use a
   ;; database-local variable for the entire score, rather than the more fine
   ;; (and fine-grained) approach of using the per-case `r' and `p' fields.
   (database-make-local 'score dbc-database
                        (let ((ar nil) (ap nil)         ; accumulated r/p
                              (nr 0) (np 0)             ; numeric r/p
                              (db dbc-database))
                          (maprecords
                           (lambda (rec)
                             (let ((exp (record-field rec 'expected-result db))
                                   (act (record-field rec 'actual-result db)))
                               (if (string= "" act)
                                   (setq ar (cons 0 ar)
                                         ap (cons 0 ap))
                                 (setq ar (cons 1 ar))
                                 (setq nr (1+ nr))
                                 (if (string= act exp)
                                     (setq ap (cons 1 ap)
                                           np (1+ np))
                                   (setq ap (cons 0 ap))))))
                           db)
                          (vector nr np
                                  (apply 'vector (reverse ar))
                                  (apply 'vector (reverse ap)))))
   (qa-put 'score (database-get-local 'score dbc-database))
   (db-save-database)
   (tcdb-exit)                          ; hmm
   ;; Now update test case histories.
   (db-find-file (cdr (assq 'tcdb (qa-get 'files)))
                 (qa-data-file "tcdb.fmt"))
   (database-make-local 'score dbc-database (qa-get 'score))
   (let ((run-array  (aref (qa-get 'score) 2))
         (pass-array (aref (qa-get 'score) 3))
         (db dbc-database))
     (tcdb-mapirec
      (lambda (idx rec)
        (let ((nr  (record-field rec 'nr db))
              (np  (record-field rec 'np db)))
          (record-set-field rec 'nr (+ nr (aref run-array  idx)) db nil) ; yuk!
          (record-set-field rec 'np (+ np (aref pass-array idx)) db nil) ; yuk!
          (db-accept-record)
          (dbf-set-this-record-modified-p t)))))
   (db-save-database)
   (tcdb-exit)))

;;;---------------------------------------------------------------------------
;;; Reporting

(defun qa-html-from-test-run ()         ; todo: add histories to output
  (interactive)
  (while-qa-session
   (let ((trun (qa-read-file-name "Test run file: " 'trun t))
         (html (qa-read-file-name "Report (HTML) file: " 'html)))
     (when (file-exists-p html)
       (unless (y-or-n-p "Report (HTML) file exists, overwrite? ")
         (error "Aborting report, choose another report file.")))
     (db-find-file trun
                   (qa-data-file "test-run.fmt" (file-name-directory trun)))
     (let ((cur  (current-buffer))
           (db   dbc-database)
           (obuf (find-file html)))
       (erase-buffer)
       (insert "<html><head><title>\n"
               "Report for " (buffer-name (qa-get 'spec-buffer)) " "
               "generated " (format-time-string "%Y-%m-%d %T") "\n"
               "</title></head>\n"
               "<body>\n")

       (let* ((score (database-get-local 'score db))
              (run-array  (aref score 2))
              (pass-array (aref score 3)))

         (insert "<h1>Summary</h1>\n")
         (let ((tot (database-no-of-records db))
               (r   (aref score 0))
               (p   (aref score 1)))
           (insert (format "%d Test Cases, %d(%d%%) Ran, %d(%d%%) Passed\n\n"
                           tot r (/ (* 100.0 r) tot) p (/ (* 100.0 p) tot))))

         (insert "<table border>\n")
         (insert "<tr><td>Test Case</td><td>Ran?</td><td>Pass?</td></tr>\n")
         (tcdb-mapirec
          (lambda (idx rec)
            (let* ((sir (record-field rec 'spec-item-ref db))
                   (seq (number-to-string (record-field rec 'subseq-num db))))
                (insert "<tr><td><a href=\"#" sir "-" seq "\">"
                        sir "-" seq "</a></td>"
                        "<td>" (number-to-string (aref run-array  idx)) "</td>"
                        "<td>" (number-to-string (aref pass-array idx)) "</td>"
                        "</tr>\n"))
              (incf idx)))
         (insert "</table> <p>\n\n")

         (insert "<h1>Details</h1>\n")
         (insert "<ul>\n")
         (tcdb-mapirec
          (lambda (idx rec)
            (let* ((sir (record-field rec 'spec-item-ref db))
                   (seq (number-to-string (record-field rec 'subseq-num db)))
                   (nm  (concat sir "-" seq))
                   (inp (record-field rec 'action db))
                   (exp (record-field rec 'expected-result db))
                   (act (record-field rec 'actual-result db)))
              (insert "<li>" sir "<p>\n")
              (when (string= seq "1")
                (insert (cdr (assoc sir (qa-get 'spec-items))) "<p>\n"))
              (insert "<em><a name=\"" nm "\">Action" seq ":</a></em> "
                      inp "<p>\n")
              (if (= 0 (aref run-array idx))
                  (insert "<em><strong>NOT RUN!!!</strong></em><p>\n")
                (if (= 1 (aref pass-array idx))
                    (insert "<em>Expected/Actual Result:</em> " exp " <p>\n")
                  (insert "<em>Expected Result:</em> " exp " <p>\n")
                  (insert "<em>Actual Result:</em> " act " <p>\n"))))
            (incf idx)))
         (insert "</ul> <p>\n"))

       (insert "\n\n<hr>\n\n<p>")

       (insert "<table border>\n")
       (insert "<tr><td>type</td><td>file</td><td>last modified</td></tr>\n")
       (mapc (lambda (type)
               (let ((file (cdr (assq type (qa-get 'files)))))
                 (when file
                   (insert (format "<tr><td>%s</td><td>%s</td>%s</tr>\n"
                                   type file
                                   (format-time-string
                                    "<td>%c</td>"
                                    (nth 5 (file-attributes file))))))))
             '(trun tcdb spec))
       (insert "</table>\n\n")

       ;; Add other boilerplate here.
       (insert "The End\n</body></html>\n")

       (save-buffer)
       (kill-buffer (current-buffer))

       (switch-to-buffer cur)
       (tcdb-exit)))))

;;;---------------------------------------------------------------------------
;;; Load-time actions

(add-to-list 'kill-buffer-query-functions
             (lambda ()
               (if (not (and (qa-get 'session)
                             (eq (qa-get 'spec-buffer) (current-buffer))))
                   t
                 (message "%s not killed, being used by QA session"
                          (current-buffer))
                 (sit-for 2)
                 nil)))

(add-to-list 'kill-buffer-hook
             (lambda ()
               (let ((curbuf (current-buffer)))
                 (when (member curbuf (qa-get 'open-files))
                   (qa-log "closed file: %s" curbuf))
                 (qa-put 'open-files (delete curbuf (qa-get 'open-files))))))

;; Installation:

;;;---------------------------------------------------------------------------
;;; That's all, folks!

(provide 'qa)

;;; qa.el ends here
