;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ls-lr-reader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Parses ls-lR files (eg. from ftp servers).
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2009-12-14 <PJB> Created
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

(in-package "LS-LR-READER")




;;;----------------------------------------
;;; TEXT-STREAM
;;;----------------------------------------

(defstruct text-stream
  stream
  pathname
  (line-number 0)
  last-line-read)

(defmethod text-stream-read-line ((self text-stream))
  (incf (text-stream-line-number self))
  (let ((line (read-line (text-stream-stream self) nil nil)))
    ;; (format *trace-output* "Read ~:R line: ~S~%" (text-stream-line-number self) line)
    (when (zerop (mod (text-stream-line-number self) 100000))
      (format *trace-output* "~&Read ~D lines" (text-stream-line-number self))
      (room)
      (format *trace-output* "~&")
      (force-output *trace-output*))
    (setf (text-stream-last-line-read self) line)))

(defmethod text-stream-read-sequence (sequence (self text-stream))
  (read-sequence sequence (text-stream-stream self)))

(defmethod text-stream-file-position ((self text-stream))
  (file-position (text-stream-stream self)))

(defmethod (setf text-stream-file-position) (new-position (self text-stream))
  (file-position (text-stream-stream self) new-position))



;;;----------------------------------------
;;; CACHED-DIRECTORY directory entry
;;;----------------------------------------

(defstruct (cached-directory (:include directory))
  ;; start-position and end-position are file position where
  ;; the data for the directory can be found.  Thus we can
  ;; avoid keeping all the non-directory entries, we'll just
  ;; read them again if needed.
  stream start-position start-line
  (level 0)
  (file-disk-usage 0 #| Sum of the disk usage of the non-directory entries. |#))


(defmethod compute-disk-usage ((self cached-directory))
  (flet ((directory-disk-usage (item)
           (if (directory-p item)
             (compute-disk-usage item)
             0)))
    (let ((contents (ls-lr-file-system::directory-%contents self)))
      (setf (entry-disk-usage self)
            (etypecase contents
              (hash-table
               (let ((du (cached-directory-file-disk-usage self)))
                 (maphash (lambda (k item)
                              (declare (ignore k))
                            (incf du (directory-disk-usage item)))
                          contents)
                 du))
              (list
               (reduce (function +) contents
                       :key (function directory-disk-usage)
                       :initial-value (cached-directory-file-disk-usage self))))))))


(defun read-directory-contents (current-directory text-stream)
  (setf (cached-directory-start-position current-directory) (text-stream-file-position text-stream)
        (cached-directory-start-line     current-directory) (text-stream-line-number   text-stream))
  (loop
     :for line = (text-stream-read-line text-stream)
     :while (and line (plusp (length line)))
     :do (add-entry current-directory
                    (parse-entry-line current-directory (text-stream-stream text-stream) line)))
  current-directory)


(defmethod enter-directory ((self cached-directory))
  (if (cached-directory-start-position self)
    (when (zerop (cached-directory-level self))
      (incf (cached-directory-level self)) ; keep it before to avoid infinite loop.
      (let* ((stream (cached-directory-stream self))
             (text-stream (make-text-stream :stream stream
                                            :pathname (pathname stream)
                                            :line-number (cached-directory-start-line self))))
        (setf (text-stream-file-position text-stream) (cached-directory-start-position self))
        (loop
           :for line = (text-stream-read-line text-stream)
           :while (and line (plusp (length line)))
           :do (let* ((new-entry (parse-entry-line self stream line))
                      (old-entry (find-entry-named self (entry-name new-entry))))
                 (cond
                   ((null old-entry) (add-entry self new-entry))
                   ((and (eql (typep new-entry 'directory)
                              (typep old-entry 'directory))))
                   (t (error "There is already an entry named ~S in the directory ~S"
                             (entry-name new-entry) self)))))))
    (error "Incomplete ls-lR file: no information for the directory ~A"
           (entry-pathname self)))
  (values))


(defmethod leave-directory ((self cached-directory))
  (when (zerop (decf (cached-directory-level self)))
    (let ((contents (ls-lr-file-system::directory-%contents self)))
      (etypecase contents
        (hash-table
         (maphash (lambda (k v) (unless (directory-p v) (remhash k contents))) contents))
        (list
         (setf (ls-lr-file-system::directory-%contents self)
               (remove-if-not (function directory-p) contents))))))
  (values))


(defmacro with-cached-directory ((directory-variable directory-expression)
                                 &body body)
  (let ((vdir (gensym)))
    `(let ((,vdir ,directory-expression))
       (enter-directory ,vdir)
       (unwind-protect
            (let ((,directory-variable ,vdir))
              ,@body)
         (leave-directory ,vdir)))))





;;;----------------------------------------
;;;
;;;----------------------------------------

(defun remove-first-dots (path)
  (if (string= "." (first path))
      (remove-first-dots (rest path))
      path))


(defvar *read-word-buffer* (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)
  "Buffer for read-word")

(defun read-word (stream)
  (let ((word *read-word-buffer*))
    (setf (fill-pointer word) 0)
    (loop
       :for ch = (read-char stream nil nil)
       :while (and ch (char= #\space ch))
       :finally (when ch (vector-push ch word)))
    (loop
       :for ch = (read-char stream nil nil)
       :while (and ch (char/= #\space ch))
       :do (vector-push-extend ch word (length word)))
    (copy-seq word)))


(defun parse-entry-line (parent-directory stream line)
  "
DO:     Parses a line such as:
        -rw-r--r--   1 root     other    120835965 May 19  2009 ls-lR.gz
        drwxr-xr-x  37 infoadmin infoadmin      37 Dec  4 14:57 vol1
RETURN: A directory entry structure.
"
  (with-input-from-string (input line)
    (let* ((kind          (read-char input))
           (rights        (parse-access-rights (read-word input)))
           (links         (read input))
           (owner         (intern-user  (read-word input)))
           (group         (intern-group (read-word input)))
           (size          (read input))
           (month         (position (read-word input)
                                    #(nil
                                      "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                    :test (function string-equal)))
           (day           (read input))
           (hour-or-year  (read-word input))
           (colon         (position #\: hour-or-year))
           (year          (if colon (current-year) (parse-integer hour-or-year)))
           (hour          (if colon (parse-integer hour-or-year :end colon)        12))
           (minute        (if colon (parse-integer hour-or-year :start (1+ colon))  0))
           (date          (encode-universal-time 0 minute hour day month year 0))
           (name-original (string-left-trim " " (read-line input))))
      (declare (ignore links))
      (ecase kind
        (#\- (make-regular-file     :access-rights rights
                                    :parent parent-directory
                                    :owner  owner
                                    :group  group
                                    :size   size
                                    :modification-date date
                                    :name   name-original))
        (#\s (make-socket           :access-rights rights
                                    :parent parent-directory
                                    :owner  owner
                                    :group  group
                                    :size   size
                                    :modification-date date
                                    :name   name-original))
        (#\p (make-named-pipe       :access-rights rights
                                    :parent parent-directory
                                    :owner  owner
                                    :group  group
                                    :size   size
                                    :modification-date date
                                    :name   name-original))
        (#\l (make-symbolic-link    :access-rights rights
                                    :parent parent-directory
                                    :owner  owner
                                    :group  group
                                    :size   size
                                    :modification-date date
                                    :name   (subseq name-original 0 (search " -> " name-original))
                                    :original-path (subseq name-original (+ 4 (search " -> " name-original)))))
        (#\d (make-cached-directory :access-rights  rights
                                    :parent parent-directory
                                    :stream stream
                                    :owner  owner
                                    :group  group
                                    :size   size
                                    :modification-date date
                                    :name   name-original))))))



(defun make-cached-directories (root path &optional stream)
  (let ((root (or root
                  (make-cached-directory :name ""
                                         :owner *default-user*
                                         :group *default-group*
                                         :access-rights *default-directory-access-rights*
                                         :stream stream))))
    (if path
      (let ((subdir (or (find-entry-named root (first path))
                        (add-entry root (make-cached-directory :name (first path)
                                                               :parent root
                                                               :owner *default-user*
                                                               :group *default-group*
                                                               :access-rights *default-directory-access-rights*
                                                               :stream stream)))))
        (values (make-cached-directories subdir (rest path)) root stream))
      (values root root))))



(defun report-error (text-stream control-string &rest arguments)
  (let ((*print-circle* nil)
        (*print-pretty* nil))
    (format *error-output*
            "~&~A:~A: ~?~%"
            (text-stream-pathname text-stream) (text-stream-line-number text-stream)
            control-string arguments)
    (finish-output *error-output*)))




(defun read-ls-lR-text-stream (text-stream
                               &optional (root (make-cached-directories nil nil (text-stream-stream text-stream))))
  (flet ((skip-till-empty-line ()
           (format *trace-output* "~&Skipping till next empty line...~%")
           (force-output *trace-output*)
           (loop
              :for line = (text-stream-read-line text-stream)
              :while (and line (plusp (length line))))))
    (loop
       :for line = (text-stream-read-line text-stream)
       :for len = (length line)
       :while line
       :do (if (and (plusp len) (char= #\: (aref line (1- len))))
             (let ((current-directory
                    (make-cached-directories
                     root
                     (split-sequence #\/
                                     (subseq line
                                             (position #\. line :test-not (function char=))
                                             (1- len))
                                     :remove-empty-subseqs t)
                     (text-stream-stream text-stream))))
               (setf line (text-stream-read-line text-stream)
                     len  (length line))
               (if (and (< 6 len) (string= "total " line :end2 6))
                 ;;(handler-case
                     (progn
                       (setf (cached-directory-level current-directory) 1)
                       (read-directory-contents current-directory text-stream)
                       (setf (cached-directory-file-disk-usage current-directory)
                             (reduce (function +) (directory-contents current-directory)
                                     :key (lambda (item)
                                              (if (directory-p item)
                                                0
                                                (compute-disk-usage item)))
                                     :initial-value 0))
                       (leave-directory current-directory))
                   ;; (error (err)
                   ;;        (report-error text-stream "~A while processing ~S"
                   ;;                      err (text-stream-last-line-read text-stream))
                   ;;        (skip-till-empty-line)))
                 (progn
                   (report-error text-stream  "expected a total line after ~S directory line, not ~S"
                                 (entry-pathname current-directory)
                                 (text-stream-last-line-read text-stream))
                   (skip-till-empty-line))))
             (progn
               (report-error text-stream "expected a directory line, not ~S"
                             (text-stream-last-line-read text-stream))
               (skip-till-empty-line)))
       :finally (progn (compute-disk-usage root)
                       (return root)))))



;;;----------------------------------------
;;; LS-LR-FILE
;;;----------------------------------------
;;; A ls-lR file kept open while browsing.
;;;


(defstruct ls-lr-file
  stream
  root-directory)


(defun open-ls-lr-file (pathname &key (if-does-not-exist :error)
                        (external-format :default))
  (let ((stream (open pathname
                      :direction :input
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format)))
    (if (streamp stream)
        (make-ls-lr-file :stream stream
                         :root-directory (read-ls-lr-text-stream (make-text-stream
                                                                  :stream stream
                                                                  :pathname pathname)))
        stream)))


(defun close-ls-lr-file (file)
  (close (ls-lr-file-stream file))
  (setf (ls-lr-file-stream file)         nil
        (ls-lr-file-root-directory file) nil))

(defvar *root-directory*)
(defvar *working-directory*)

(defmacro with-open-ls-lr-file ((file-var pathname
                                          &key (if-does-not-exist :error)
                                          (external-format :default))
                                &body body)
  (let ((vfile (gensym)))
    `(let ((,vfile (open-ls-lr-file ,pathname
                                    :if-does-not-exist ,if-does-not-exist
                                    :external-format ,external-format)))
       (unwind-protect
            (let ((*root-directory*     (ls-lr-file-root-directory ,vfile))
                  (*working-directory*  (ls-lr-file-root-directory ,vfile))
                  (,file-var            ,vfile))
              ,@body)
         (close-ls-lr-file ,vfile)))))




;; (defmacro regexp-cond (&body clauses)
;;   (let ((min (gensym "MIN-"))
;;         (max (gensym "MAX-"))
;;         (beg (gensym "BEG-"))
;;         (end (gensym "END-"))
;;         (reg (gensym "REG-"))
;;         (str (gensym "STR-")))
;;     (loop
;;        :with form = nil
;;        :for clause :in (reverse clauses)
;;        :do  (cond
;;               ((atom clause)
;;                (error "Stray atom in ~S clauses: ~S" 'regexp-cond clause))
;;               ((atom (first clause))
;;                (error "Stray atom in ~S clauses: ~S" 'regexp-cond clause))
;;               (t (destructuring-bind (((&rest vars) pattern string) &body body) clause
;;                    (setf form  `(let (,@(unless (stringp pattern) `((,reg ,pattern)))
;;                                       (,str ,string))
;;                                   (multiple-value-bind (,min ,max ,beg ,end)
;;                                       (cl-ppcre:scan ,(if (stringp pattern)
;;                                                           pattern
;;                                                           reg)
;;                                                      ,str)
;;                                     (if ,min
;;                                       ,(if body
;;                                            `(let ,(mapcar
;;                                                    (let ((i -1))
;;                                                     (lambda (var)
;;                                                       (incf i)
;;                                                       `(,var (subseq  ,str (aref ,beg ,i) (aref ,end ,i)))))
;;                                                    vars)
;;                                               ,@body)
;;                                            `(subseq ,str ,min ,max))
;;                                       ,form)))))))
;;        :finally (return form))))


;; (cl-ppcre:scan
;;  "^([-dlcb])([-r][-w][-xSs][-r][-w][-xSs][-r][-w][-xTt]) +([0-9]+) +([-.A-Za-z0-9]+) +([-.A-Za-z0-9]+) +([0-9]+) +([A-Z][a-z][a-z] +[0-9]+[0-9:]+) +(.*)$"
;;  "lrwxrwxrwx   1 infoadmin infoadmin      15 Jul 21  2008 quantian.css -> ../quantian.css")

;; (defvar *log-counter* 0)
;; 
;; (defun log-line (control-string &rest arguments)
;;   (when (= 1000 (incf *log-counter*))
;;     (format *trace-output* "~?~%" control-string arguments)
;;     (force-output *trace-output*)
;;     (setf *log-counter* 0)
;;     t))



;; 
;; 
;; (defun load-ls-lr-file (pathname)
;;   ;; free *root* first to let the garbage collector collect garbage.
;;   (setf *root* nil)
;;   (setf *root*
;;         (let* ((formats '(("gz"  . "gunzip")
;;                           ("bz2" . "bunzip2")
;;                           ("bz"  . "bunzip")
;;                           ("Z"   . "uncompress")))
;;                (compressed (assoc (pathname-type pathname) formats
;;                                   :test (function string-equal))))
;;           (if compressed
;;               (with-open-stream (input #+clisp (ext:make-pipe-input-stream
;;                                                 (format nil "~A < ~S" (cdr compressed) pathname))
;;                                        #-clisp (error "Cannot deal with compressed files in ~A"
;;                                                       (lisp-implementation-type)))
;;                 (read-ls-lr-text-stream (make-text-stream
;;                                          :stream input
;;                                          :pathname pathname)))
;;               (with-open-file (input pathname)
;;                 (read-ls-lr-text-stream (make-text-stream
;;                                          :stream input
;;                                          :pathname pathname))))))
;;    (setf *working-directory* *root*)
;;    (enter-directory *working-directory*)
;;    (compute-disk-usage *root*)
;;    *root*)


;;;; THE END ;;;;
