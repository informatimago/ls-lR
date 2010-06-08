;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ls-lr-browser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Exports commands to browse the ls-lr file systems.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2009-12-26 <PJB> Created.
;;;;               (Some code from com.informatimago.common-lisp.browser).
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2009 - 2009
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(in-package "LS-LR-BROWSER")




;;;----------------------------------------
;;;
;;;----------------------------------------

(defun working-directory ()
  *working-directory*)

(defun change-working-directory (new-directory)
  (let ((old-directory (working-directory)))
    (unless (eql old-directory new-directory)
      (enter-directory new-directory)
      (setf *working-directory* new-directory)
      (leave-directory old-directory)))
  *working-directory*)


(define-command pwd ()
  "Prints the pathname of the working directory."
  (format t "~&~A~%" (entry-pathname (working-directory))))


(defmethod find-entry-named/ci ((self directory) name)
  (find name (directory-contents self)
        :test (function string-equal)
        :key (function entry-name)))


(defun resolve (name &key only-directory)
  "
RETURN: A directory entry found at the pathname NAME,
        relative to the (working-directory).
NOTE:   When the entry is not a directory entry, it may have been
        flushed out of its parent...  Entering in the parent directory
        again to find an entry with the same name won't return the
        same object.
"
  (etypecase name
    ((or symbol string character)
     (loop
        :named loop
        :with current = (working-directory) 
        :for item :in (or (split-sequence #\/ (string name)
                                          :remove-empty-subseqs t)
                          "/")
        :do (setf current (cond
                            ((string= name "/") (root-directory))
                            ((string= name ".") current)
                            ((string= name "..") (or (entry-parent current)
                                                     (root-directory)))
                            (t (with-cached-directory (current current)
                                 (let ((entry
                                        (or (find-entry-named    current name)
                                            (find-entry-named/ci current name))))
                                   (cond
                                     ((null entry)
                                      (error "There's no entry named ~S in the directory ~S"
                                             name (entry-pathname current)))
                                     ((directory-p entry) entry)
                                     (only-directory
                                      (error "The entry named ~S in the directory ~S is not a directory but a ~A."
                                             name
                                             (entry-pathname current)
                                             (type-of entry)))
                                     (t (return-from loop entry))))))))
        :finally (return-from loop current)))
    (directory name)
    (entry (if only-directory
             (error "The entry named ~S in the directory ~S is not a directory but a ~A."
                    (entry-name name)
                    (entry-pathname (entry-parent name))
                    (type-of name))
             entry))))


(define-command cd (&optional path)
  "Change the working directory.
Name is resolved relatively to the working directory.
See also: pushd, popd, pwd.
"
  (CHANGE-WORKING-DIRECTORY (resolve path :only-directory t)))


(defvar *directory-stack* '()
  "Conceptually, includes (working-directory) as T.O.S.")


(define-command pushd (&optional path)
  "Push the working directory onto the stack, and change
the working directory to the path (or home directory).
Prints the current directory stack.
See also: popd, cd.
"
  (push (working-directory) *directory-stack*) ; conceptually it's here.
  (cond
    ((null path)
     ;; swap top and below-top:
     (let ((top (pop *directory-stack*)))
       (if *directory-stack*
           (push top (cdr *directory-stack*))
           (push top *directory-stack*))))
    ((integerp path)
     ;; rotates stack 
     (let ((index (mod path (length *directory-stack*))))
       (setf *directory-stack* (concatenate 'list
                                 (subseq *directory-stack* index)
                                 (subseq *directory-stack* 0 index)))))
    ((or (stringp path) (symbolp path))
     (push (change-working-directory (resolve path :only-directory t)) *directory-stack*))
    ((directory-p path)
     (push (change-working-directory path) *directory-stack*)))
  (format t "~{~A~^ ~}~%" (cons (change-working-directory (pop *directory-stack*))
                                *directory-stack*)))


(define-command popd  ()
  "Unstack the working directory from the stack.
Prints the current directory stack.
See also: pushd, cd.
"
  (format t "~{~A~^ ~}~%"
          (if *directory-stack*
            (cons (change-working-directory (pop *directory-stack*)) *directory-stack*)
            (list (working-directory)))))



(defun relativize (path default)
  (let ((dp (split-sequence #\/ path))
        (dd (split-sequence #\/ default)))
    (cond
      ((equal dp dd)
       ".")
      ((and (> (length dp) (length dd)) (equal (subseq dp 0 (length dd)) dd))
       (format nil "~{~A~^/~}" (subseq dp (length dd))))
      ((and (< (length dp) (length dd)) (equal (subseq dd 0 (length dp)) dd))
       (format nil "~{..~^/~}" (subseq dd (length dp))))
      (t
       path))))


(defun format-long-entry (stream entry colon at &rest parameters)
  (declare (ignore colon parameters))
  (flet ((format-entry (entry name)
           (format stream "~1A~/lslrfs:format-access-rights/ ~8A ~8A ~10D ~/lslrfs:format-ls-date/ ~A~:[~; -> ~A~]"
                   (typecase entry
                     (directory     "d")
                     (regular-file  "-")
                     (socket        "s")
                     (named-pipe    "p")
                     (symbolic-link "l")
                     (t             "?"))
                   (entry-access-rights entry)
                   (user-name  (entry-owner entry))
                   (group-name (entry-group entry))
                   (entry-size entry)
                   (entry-modification-date entry)
                   name
                   (when (symbolic-link-p entry)
                     (list (symbolic-link-original-path entry))))))
   (if at
       (format-entry (first entry) (second entry))
       (format-entry entry (relativize (entry-pathname entry)
                                       (entry-pathname (working-directory)))))))

(defun human-readable (cardinal)
  (loop
     :for factor :in '(" " "K" "M" "G" "T" "P")
     :for mantissa = cardinal :then (/ mantissa #.(expt 2 10))
     :while (<= #.(expt 2 10) mantissa)
     :finally (return (format nil "~D ~A" (round mantissa) factor))))


(defun print-du (usage title &key human-readable-p)
  (format t "~12D ~A~%"
          (if human-readable-p
              (human-readable usage)
              usage)
          title))


(define-command du (&option ("c" ("total")          flag grand-total-p    nil "produces a grand total")
                            ("h" ("human-readable") flag human-readable-p nil "human readable (KB, MB, GB, TB)")
                            ("s" ("summarize")      flag summarizep       nil "summarize (doesn't do otherwise)")
                    &rest paths)
  "List the files or directories."
  (declare (ignore summarizedp))
  (let ((grand-total      0)) 
    (dolist (path (or paths (list (working-directory))))
      (let ((entry (resolve path)))
        (incf grand-total (entry-disk-usage entry))
        (print-du (entry-disk-usage entry)
                  (relativize (entry-name entry)
                              (entry-name (working-directory)))
                  :human-readable-p human-readable-p)))
    (when grand-total-p
      (print-du  grand-total "Total"
                 :human-readable-p human-readable-p))
    grand-total))


(define-command ls (&option
                    ("l" ("long")           flag opt-long-p          nil
                         "long listing: item kind, size, date, name; otherwise only name.")
                    ("a" ("all")            flag opt-all-p           nil
                         "do not ignore entries starting with .")
                    ("d" ("directory")      flag opt-directory-p      nil
                         "list directory entries instead of contents, and do not dereference symbolic links")
                    ("h" ("human-readable") flag opt-human-readable-p nil
                         "with -l, print sizes in human readable format (e.g., 1K 234M 2G)")
                    &rest paths)
  "List the files or directories."
  (setf *today* (now))
  (let ((*print-circle* nil)
        (*print-pretty* nil))
    (dolist (path (if (null paths)
                      (list ".")
                      paths))
      (let ((entry (resolve path)))
        (flet ((format-entry (entry path)
                 (if opt-long-p
                     (format t "~1A~/lslrfs:format-access-rights/ ~8A ~8A ~10D ~/lslrfs:format-ls-date/ ~A~:[~; -> ~A~]~%"
                             (typecase entry
                               (directory     "d")
                               (regular-file  "-")
                               (socket        "s")
                               (named-pipe    "p")
                               (symbolic-link "l")
                               (t             "?"))
                             (entry-access-rights entry)
                             (user-name  (entry-owner entry))
                             (group-name (entry-group entry))
                             (funcall (if opt-human-readable-p
                                           (function human-readable)
                                           (function identity))
                                      (if (directory-p entry)
                                          (entry-disk-usage entry)
                                          (entry-size entry)))
                             (entry-modification-date entry)
                             path
                             (when (symbolic-link-p entry)
                               (list (symbolic-link-original-path entry))))
                     (format t "~A~%" path
                             ;; (relativize (entry-pathname entry)
                             ;;                      (entry-pathname (working-directory)))
                             ))))
          (if (directory-p entry)
              (with-cached-directory (entry entry)
                (if opt-directory-p
                    (format-entry entry path)
                    (progn
                      (when opt-all-p
                        (format-entry entry ".")
                        (when (entry-parent entry)
                          (format-entry (entry-parent entry) "..")))
                      (loop
                         :for item :across (sort (concatenate 'vector (directory-contents entry))
                                                 (function string<) :key (function entry-name))
                         :do (when (or opt-all-p (not (char= #\. (aref (entry-name item) 0))))
                               (format-entry item (format nil "~A/~A" path (entry-name item)))))
                      (print-du (entry-disk-usage entry)
                                (relativize (entry-pathname entry)
                                            (entry-pathname (working-directory)))
                                :human-readable-p t))))
              (format-entry entry path))))))
  (values))


;;;----------------------------------------
;;;
;;;----------------------------------------


#-(and)
(
 (pathname ::= item pathname)
 (pathname ::= item)

 (item ::= word)
 (item ::= "*")
 (item ::= "?")
 (item ::= range)
 (item ::= alternatives)

 (word ::= word-character word)
 (word ::= word-character)
 (word-character ::= any-character-but-\\-*-?-[-{)
 (word-character ::= "\\" any-character)

 (range ::= "[" intervals "]")
 (range ::= "[" "^" intervals "]")
 
 (intervals ::= "-" simple-intervals)
 (intervals ::= simple-intervals)
 
 (simple-intervals ::= interval simple-intervals)
 (simple-intervals ::= )
 
 (interval ::= interval-char "-" interval-char)
 (interval ::= interval-char)
 
 (interval-char ::= any-character-but-\\-or-])
 (interval-char ::= "\\" any-character)
 
 (alternatives ::= "{" first-pathname-in-alt "," rest-alternatives "}")
 (rest-alternatives ::= other-pathname-in-alt "," rest-alternatives)
 (rest-alternatives ::= other-pathname-in-alt)
 (first-pathname-in-alt ::= pathname) ; follow = ","
 (other-pathname-in-alt ::= pathname) ; follow = ",}"
 )



(defun parse-any-character-but-\\-*-?-[-{ (stream)
  (let ((ch (peek-char nil stream nil nil)))
    (and ch (not (position ch "\\*?[{")) (read-char stream nil nil))))

(defun parse-\\-any-character (stream)
  (let ((ch (peek-char nil stream nil nil)))
    (when (and ch (eql #\\ ch) (read-char stream nil nil))
      (let ((ch (read-char stream nil nil)))
        (or ch (error "Missing a character after last \\ in word."))))))

(defun parse-word-character (stream)
  (or (parse-any-character-but-\\-*-?-[-{ stream)
      (parse-\\-any-character stream)))

(defun parse-word (stream &optional follow)
  (let ((buffer  (loop
                    :for ch = (let ((ch (peek-char nil stream nil nil)))
                                (if (or (null ch) (position ch follow))
                                    nil
                                    (parse-word-character stream)))
                    :while (and ch (not (position ch follow)))
                    :collect ch)))
    (and buffer (coerce buffer 'string))))


(defun parse-any-character-but-\\-or-] (stream)
    (let ((ch (peek-char nil stream nil nil)))
    (and ch (not (position ch "\\[")) (read-char stream nil nil))))


(defun parse-interval-char (stream)
  (or (parse-any-character-but-\\-or-] stream)
      (parse-\\-any-character stream)))


(defun parse-interval (stream)
  (let ((start (parse-interval-char stream)))
    (unless start (error "Expected a character in the interval."))
    (if (and (eql #\- (peek-char nil stream nil nil)) (read-char stream nil nil))
        (let ((end (parse-interval-char stream)))
          (unless end (error "Missing a character after - in the interval starting with ~S" start))
          (list :interval start end))
        start)))


(defun parse-simple-intervals (stream)
  (loop
     :while (let ((ch (peek-char nil stream nil nil)))
               (and ch (not (position ch "\\]"))))
     :collect (parse-interval stream)))


(defun parse-intervals (stream)
  (let ((ch (peek-char nil stream nil nil)))
    (if (and ch (eql #\- ch) (read-char stream nil nil))
        (cons "-" (parse-simple-intervals stream))
        (parse-simple-intervals stream))))


(defun parse-range (stream)
  (let ((ch (peek-char nil stream nil nil)))
    (when (and ch (eql #\[ ch) (read-char stream nil nil))
      (let ((ch (peek-char nil stream nil nil)))
        (prog1 (if (and ch (eql #\^ ch) (read-char stream nil nil))
                   (list :inverted-range (parse-intervals stream))
                   (list :range          (parse-intervals stream)))
          (unless (eql #\] (read-char stream nil nil))
            (error "Missing a closing ] in range.")))))))

(defun parse-first-pathname-in-alt (stream)
  (parse-pathname stream ","))

(defun parse-other-pathname-in-alt (stream)
  (parse-pathname stream ",}"))

(defun parse-rest-alternatives (stream)
  (let ((first-alternative (parse-other-pathname-in-alt stream))
        (ch (peek-char nil stream nil nil)))
    (if (and (eql #\, ch) (read-char stream nil nil))
        (cons first-alternative (parse-rest-alternatives stream))
        (list first-alternative))))

(defun parse-alternatives (stream)
  (let ((ch (peek-char nil stream nil nil)))
    (when (and ch (eql #\{ ch) (read-char stream nil nil))
      (let ((first-alternative (parse-first-pathname-in-alt stream)))
        (let ((ch (read-char stream nil nil)))
          (unless (eql #\, ch)
            (error "Missing a comma after the alternative."))
          (prog1 (list :alternatives (cons first-alternative (parse-rest-alternatives stream)))
            (let ((ch (read-char stream nil nil)))
              (unless (eql #\} ch)
                (error "Expected a comma or a closing brace to end the alternatives, not ~S." ch)))))))))


(defun parse-item (stream &optional follow)
  (or (parse-word stream follow)
      (let ((ch (peek-char nil stream nil nil)))
        (and ch (position ch "*?")
             (ecase (read-char stream nil nil)
               ((#\*) :any-string)
               ((#\?) :any-character))))
      (parse-range stream)
      (parse-alternatives stream)))


(defun parse-pathname (stream &optional follow)
  (cons (parse-item stream follow)
        (let ((ch  (peek-char t stream nil nil)))
          (and (not (position ch '(nil #\})))
               (not (position ch follow))
               (parse-pathname stream follow)))))


(defun parse-pathname-string (word)
  (with-input-from-string (stream word)
    (parse-pathname stream)))


;;("abc" :ANY-STRING "def" (:RANGE ((:INTERVAL #\a #\z))) "ghi"
;;   (:ALTERNATIVES (("1") ("2") ("xxx" (:ALTERNATIVES (("33") ("44")))))))

(defun expand-alternatives (pattern)
  (cond
    ((atom pattern) (list pattern))
    ((atom (first pattern))
     (mapcar (lambda (rest) (cons (first pattern) rest))
             (expand-alternatives (rest pattern))))
    ((and (listp (first pattern))
          (eql :alternatives (first (first pattern))))
     (let ((tails (expand-alternatives (rest pattern))))
       (mapcan (lambda (head-alternative)
                 (let ((heads  (expand-alternatives head-alternative)))
                   (mapcan (lambda (tail)
                             (mapcar (lambda (head) (append head tail)) heads))
                           tails)))
               (second (first pattern)))))
    (t
     (mapcar (lambda (rest) (cons (first pattern) rest))
             (expand-alternatives (rest pattern))))))


(defun normalize-pattern (pattern)
  (loop
     :with current-item = nil
     :with state = :initial
     :with normalized = '()
     :for item :in pattern
     :do (ecase state
           ((:initial)
            (cond
              ((stringp item)
               (setf current-item item
                     state :string))
              ((eql item :any-string)
               (setf current-item item
                     state :any-string))
              ((eql item :any-character)
               (push item normalized))
              ((and (listp item) (eql (first item) :range))
               (push item normalized))
              (t
               (warn "unexpected item ~S" item)
               (push item normalized))))
           ((:string)
            (cond
              ((stringp item)
               (setf current-item (concatenate 'string current-item item)))
              ((eql item :any-string)
               (push current-item normalized)
               (setf current-item item
                     state :any-string))
              ((eql item :any-character)
               (push current-item normalized)
               (push item normalized)
               (setf state :initial))
              ((and (listp item) (eql (first item) :range))
               (push current-item normalized)
               (push item normalized)
               (setf state :initial))
              (t
               (warn "unexpected item ~S" item)
               (push current-item normalized)
               (push item normalized)
               (setf state :initial))))
           ((:any-string)
            (cond
              ((stringp item)
               (push current-item normalized)
               (setf current-item item
                     state :string))
              ((eql item :any-)
               (push current-item normalized)
               (setf current-item item
                     state :any-string))
              ((eql item :any-character)
               (push current-item normalized)
               (push item normalized)
               (setf state :initial))
              ((and (listp item) (eql (first item) :range))
               (push current-item normalized)
               (push item normalized)
               (setf state :initial))
              (t
               (warn "unexpected item ~S" item)
               (push current-item normalized)
               (push item normalized)
               (setf state :initial)))))
     :finally (progn (push current-item normalized)
                     (return (nreverse normalized)))))

(defun split-directories (pattern)
  (loop
     :with names = '()
     :with current-name = '()
     :for item :in pattern
     :do (if (stringp item)
             (let ((subnames (split-sequence #\/ item)))
               (if (null (rest subnames))
                   ;; only one substring <=> no slash ==> no push name.
                   (unless (string= "" (first subnames))
                     (push (first subnames) current-name))
                   (progn
                     (if (string= "" (first subnames))
                         (progn
                           (pop subnames)
                           (when current-name
                             (push (nreverse current-name) names)))
                         (progn
                           (push (pop subnames) current-name)
                           (push (nreverse current-name) names)))
                     (loop
                        :while (rest subnames)
                        :do (let ((name (pop subnames)))
                              (unless (string= "" name)
                                (push name names)))
                        :finally (setf current-name (if (string= "" (first subnames))
                                                        '()
                                                        subnames))))))
             ;; not a string:
             (push item current-name))
     :finally (progn
                (when current-name
                  (push (nreverse current-name) names))
                (return (nreverse names)))))


(defun expand-pathname-pattern (pattern)
  (mapcar (lambda (pattern)
            (split-directories (normalize-pattern pattern)))
          (expand-alternatives pattern)))


;; (map nil 'print (expand-pathname-pattern (parse-pathname-string "/{tmp,usr/tmp}/*/{pjb,bgb,afb}/??xx.{txt,[Tt]ext}")))
;; 
;; ("tmp" (:ANY-STRING) "pjb" (:ANY-CHARACTER :ANY-CHARACTER "xx.txt")) 
;; ("tmp" (:ANY-STRING) "pjb" (:ANY-CHARACTER :ANY-CHARACTER "xx." (:RANGE (#\T #\t)) "ext")) 
;; ("tmp" (:ANY-STRING) "bgb" (:ANY-CHARACTER :ANY-CHARACTER "xx.txt")) 
;; ("tmp" (:ANY-STRING) "bgb" (:ANY-CHARACTER :ANY-CHARACTER "xx." (:RANGE (#\T #\t)) "ext")) 
;; ("tmp" (:ANY-STRING) "afb" (:ANY-CHARACTER :ANY-CHARACTER "xx.txt")) 
;; ("tmp" (:ANY-STRING) "afb" (:ANY-CHARACTER :ANY-CHARACTER "xx." (:RANGE (#\T #\t)) "ext")) 
;; ("usr" "tmp" (:ANY-STRING) "pjb" (:ANY-CHARACTER :ANY-CHARACTER "xx.txt")) 
;; ("usr" "tmp" (:ANY-STRING) "pjb" (:ANY-CHARACTER :ANY-CHARACTER "xx." (:RANGE (#\T #\t)) "ext")) 
;; ("usr" "tmp" (:ANY-STRING) "bgb" (:ANY-CHARACTER :ANY-CHARACTER "xx.txt")) 
;; ("usr" "tmp" (:ANY-STRING) "bgb" (:ANY-CHARACTER :ANY-CHARACTER "xx." (:RANGE (#\T #\t)) "ext")) 
;; ("usr" "tmp" (:ANY-STRING) "afb" (:ANY-CHARACTER :ANY-CHARACTER "xx.txt")) 
;; ("usr" "tmp" (:ANY-STRING) "afb" (:ANY-CHARACTER :ANY-CHARACTER "xx." (:RANGE (#\T #\t)) "ext")) 


(defun make-regexp-pattern (pattern)
  (cl-ppcre:CREATE-SCANNER  
   (with-output-to-string (out)
     (princ "^" out)
     (dolist (item pattern)
       (cond
         ((stringp item)            (princ item out))
         ((eql item :any-string)    (princ ".*" out))
         ((eql item :any-character) (princ "."  out))
         ((or (eql item :range)
              (eql item :inverted-range))
          (princ "[")
          (when (eql item :inverted-range)
            (princ "^"))
          (dolist (interval (second item))
            (cond
              ((characterp interval)
               (princ interval out))
              ((and (listp interval) (eql (first interval :INTERVAL)))
               (princ (second interval) out)
               (princ "-" out)
               (princ (third  interval) out))))
          (princ "]"))
         (t
          (error "Unexpected item in pattern: ~S" item))))
     (princ "$" out))))

(defun match-regexp-pattern (pattern string)
  (cl-ppcre:scan pattern string))


(defun match-directory-entries (current-directory pattern path)
  (if (null pattern)
      (list current-directory)
      (let ((item (first pattern)))
        (cond
          ((equal '(".") item)
           (match-directory-entries current-directory
                                    (rest pattern)
                                    (append item path)))
          ((equal '("..") item)
           (match-directory-entries (entry-parent current-directory)
                                    (rest pattern)
                                    (append item path)))
          ((and (stringp (first item)) (null (rest item)))
           (with-cached-directory (current-directory current-directory)
             (let ((entry (find-entry-named current-directory (first item))))
               (if (rest pattern)
                   (if (directory-p entry)
                       (match-directory-entries entry
                                                (rest pattern)
                                                (append item path))
                       '())
                   (if entry
                       (list (append item path))
                       '())))))
          (t
           (let ((regexp-pattern (make-regexp-pattern item)))
             (with-cached-directory (current-directory current-directory)
               (let ((good-entries
                      (remove-if-not
                       (lambda (entry)
                         (match-regexp-pattern regexp-pattern
                                               (entry-name entry)))
                       (directory-contents current-directory))))
                 (if (rest pattern)
                     (mapcan
                      (lambda (entry)
                        (when (directory-p entry)
                          (match-directory-entries entry
                                                   (rest pattern)
                                                   (cons (entry-name entry) path)))))
                     (mapcar (lambda (entry) (cons (entry-name entry) path))
                             good-entries))))))))))




(defun pathname-expansion (words)
  (mapcan
   (lambda (word)
     (let ((entries (mapcan
                     (lambda (pattern)
                       (match-directory-entries (working-directory)
                                                pattern
                                                '()))
                     (expand-pathname-pattern (parse-pathname-string word)))))
       (if entries
           (mapcar (lambda (path) (format nil "~{~A~^/~}" (nreverse path)))
                   entries)
           (list word))))
   words))


(defun browse-ls-lr-file (pathname &key (if-does-not-exist :error)
                          (external-format :default))
  (format *query-io* "~&Reading the ls-lR file ~S~%" pathname)
  (finish-output *query-io*)
  (with-open-ls-lr-file (file pathname
                              :if-does-not-exist if-does-not-exist
                              :external-format external-format)
    (declare (ignorable file))
    (format *query-io*
            "~&You may type HELP to get a list of available commands.~%")
    (finish-output *query-io*)
    (command-line:command-repl
     *query-io*
     :prompt (let ((filename (namestring (pathname pathname)))
                   (counter 0))
               (lambda (stream)
                 (format stream "~A~A[~A]> "
                         filename
                         (entry-pathname (working-directory))
                         (incf counter))
                 (finish-output stream)))
     :filter-command-line (function pathname-expansion)))
   (format *query-io* "~&Good bye!~%")
   (finish-output *query-io*)
  (values))



;;;; The END ;;;;
