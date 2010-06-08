;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ls-lr-file-system.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A hierarchical directory file structure.
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

(in-package "LS-LR-FILE-SYSTEM")


;;;----------------------------------------
;;; Dates
;;;----------------------------------------

(defun now () (get-universal-time))

(defun current-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))


(defvar *today* (now)
  "Used as a reference to determine which short form a date must be formated as.
Client code can rebind it to another universal date or set it to (now).")


(defvar *short-month-names*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))


(defun parse-short-month (short-month-name)
  (let ((pos (position short-month-name *short-month-names*
              :test (function string-equal))))
    (and pos (1+ pos))))


(defun parse-ls-date (string)
  (let* ((month (parse-short-month (subseq string 0 3)))
         (day (parse-integer string :start 4 :end 6))
         (colon (position #\: string :start 7))
         (year (if colon (current-year) (parse-integer string :start 8)))
         (hour (if colon (parse-integer string :start 7 :end 9) 12))
         (minute (if colon (parse-integer string :start (1+ colon)) 0)))
    (encode-universal-time 0 minute hour day month year 0)))


(defun format-ls-date (stream date colon at &rest arguments)
  (declare (ignore at arguments))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time date)
    (if colon
     (cond
       ((< (- *today* date) (* 24 60 60))
        (format stream  "~2,'0D:~2,'0D:~2,'0D   " ho mi se))
       ((< (- *today* date) (* 6 30 24 60 60))
        (format stream "~2,'0D-~2,'0D ~2,'0D:~2,'0D" mo da ho mi))
       (t
        (format stream "~4,'0D-~2,'0D-~2,'0D " ye mo da)))
     (cond
       ((< (- *today* date) (* 6 30 24 60 60))
        (format stream "~3A ~2D ~2,'0D:~2,'0D"
                (aref *short-month-names* (1- mo)) da ho mi))
       (t
        (format stream "~3A ~2D ~5D"
                (aref *short-month-names* (1- mo)) da ye))))))


;;;----------------------------------------
;;; Users and groups
;;;----------------------------------------
;;; Users and groups are interned structures.
;;;


(defstruct user
  name)

(defvar *users* (make-hash-table :test (function equal))
  "Maps name strings to owner objects.")

(defun intern-user (name)
  (or (gethash name *users*)
      (setf (gethash name *users*) (make-user :name name))))


(defstruct group
  name)

(defvar *groups* (make-hash-table :test (function equal))
  "Maps name strings to group objects.")

(defun intern-group (name)
  (or (gethash name *groups*)
      (setf (gethash name *groups*) (make-group :name name))))



;;;----------------------------------------
;;; Access rights
;;;----------------------------------------
;;; Access rights are encoded into a fixnum
;;;


(deftype access-rights () `(integer ,(expt 2 12)))


(defun make-access-rights (&rest specs)
  "
SPECS: ([:owner|:group|:other] [:all:rw|:read|:write|:execute|:access|:super|:sticky]*)*
:super is valid only for :owner and :group.
:sticky is valid only for :other.
:access is an alias of :execute for directories.
"
  (loop
     :with base = 0 :with super = 9
     :with rights = 0
     :while specs
     :do (let ((category (pop specs)))
           (case category
             ((:owner) (setf base 0 super 9))
             ((:group) (setf base 3 super 10))
             ((:other) (setf base 6 super 11))
             (otherwise
              (error "Invalid category keyword ~S (expected :OWNER, :GROUP or :OTHER)."
                     category)))
           (loop
              :do (case (first specs)
                    ((:all)             (setf rights (dpb 7 (byte 3 base) rights)))
                    ((:rw)              (setf rights (dpb 3 (byte 2 base) rights)))
                    ((:read)            (setf rights (dpb 1 (byte 1 (+ 0 base)) rights)))
                    ((:write)           (setf rights (dpb 1 (byte 1 (+ 1 base)) rights)))
                    ((:execute :access) (setf rights (dpb 1 (byte 1 (+ 2 base)) rights)))
                    ((:super :sticky)   (setf rights (dpb 1 (byte 1 super) rights)))
                    (otherwise (loop-finish)))
              :do (pop specs)))
     :finally (return rights)))


(defun parse-access-rights (string)
  (+
   (if (char= #\r (char string 0)) 1 0)
   (if (char= #\w (char string 1)) 2 0)
   (if (char= #\x (char string 2)) 4 0)
   (if (char= #\S (char string 2)) 512 0)
   (if (char= #\s (char string 2)) 516 0)
   (if (char= #\r (char string 3)) 8 0)
   (if (char= #\w (char string 4)) 16 0)
   (if (char= #\x (char string 5)) 32 0)
   (if (char= #\S (char string 5)) 1024 0)
   (if (char= #\s (char string 5)) 1056 0)
   (if (char= #\r (char string 6)) 64 0)
   (if (char= #\w (char string 7)) 128 0)
   (if (char= #\x (char string 8)) 256 0)
   (if (char= #\T (char string 8)) 2048 0)
   (if (char= #\t (char string 8)) 2304 0)))


(defun format-access-rights (stream bits colon right-align-p &rest parameters)
  (declare (ignore colon))
  (let ((width          (or (first parameters) 0)))
    (check-type width integer)
    (unless right-align-p
      (format stream "~VA" (max (- width 9) 0) ""))
    (format stream "~:[-~;r~]~:[-~;w~]~:[~:[-~;x~]~;~:[S~;s~]~]~:[-~;r~]~:[-~;w~]~:[~:[-~;x~]~;~:[S~;s~]~]~:[-~;r~]~:[-~;w~]~:[~:[-~;x~]~;~:[T~;t~]~]"
            (logbitp 0 bits)
            (logbitp 1 bits)
            (logbitp 9 bits) (logbitp 2 bits)
            (logbitp 3 bits)
            (logbitp 4 bits)
            (logbitp 10 bits) (logbitp 5 bits)
            (logbitp 6 bits)
            (logbitp 7 bits)
            (logbitp 11 bits) (logbitp 8 bits))
    (when right-align-p
      (format stream "~VA" (max (- width 9) 0) "")))
  (values))



;;;----------------------------------------
;;; Generic directory entry
;;;----------------------------------------
;;; Generic entries contain almost all the data that all the entries
;;; contain (at least at the user interface level, we don't deal with
;;; technical internal i-node data).
;;;


(defstruct entry
  name
  parent
  (access-rights      (make-access-rights :owner :read :write :execute
                                          :group :read :write :execute
                                          :other :read :write :execute))
  (owner              (intern-user "root"))
  (group              (intern-user "root"))
  (size               0)
  (modification-date  (now))
  (disk-usage         0))


(defmethod entry-pathname ((self entry))
  (format nil "~A/~A"
          (if  (entry-parent self)
               (entry-pathname (entry-parent self))
               "")
          (entry-name self)))


(defmethod print-object ((self entry) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream ":PATH ~S" (entry-pathname self)))
  self)


;;;----------------------------------------
;;; REGULAR-FILE directory entry
;;;----------------------------------------

(defstruct (regular-file (:include entry))
  #|nothing|#)


;;;----------------------------------------
;;; SOCKET directory entry
;;;----------------------------------------

(defstruct (socket (:include entry))
  #|nothing|#)


;;;----------------------------------------
;;; NAMED-PIPE directory entry
;;;----------------------------------------

(defstruct (named-pipe (:include entry))
  #|nothing|#)


;;;----------------------------------------
;;; SYMBOLIC-LINK directory entry
;;;----------------------------------------

(defstruct (symbolic-link (:include entry))
  original-path)



;;;----------------------------------------
;;; DIRECTORY directory entry
;;;----------------------------------------
;;; The directory contents is either an a-list or a hash-table
;;; depending on its size.
;;;

(defstruct (directory (:include entry))
  %contents)


(defmethod directory-contents ((self directory))
  (etypecase (directory-%contents self)
    (hash-table (let ((result '()))
                  (maphash (lambda (k v) (declare (ignore k)) (push v result))
                           (directory-%contents self))
                  result))
    (list       (mapcar (function cdr) (directory-%contents self)))))


(defmethod (setf directory-contents) (new-contents (self directory))
  (assert (every (function entry-p) new-contents))
  (setf (directory-%contents self)
        (if (< (length new-contents) #+clisp 30 #-clisp 6)
          (mapcar (lambda (entry) (cons (entry-name entry) entry)) new-contents)
          (loop
             :with table = (make-hash-table :test (function equal))
             :for entry :in new-contents
             :do (setf (gethash (entry-name entry) table) entry)
             :finally (return table)))))


(defmethod find-entry-named ((self directory) name)
  (etypecase (directory-%contents self)
    (hash-table (gethash name (directory-%contents self)))
    (list       (cdr (assoc name (directory-%contents self) :test (function string=))))))



(defmethod add-entry ((self directory) (entry entry))
  (flet ((error-already ()
           (error "There is already an entry named ~S in the directory ~S"
                  (entry-name entry) self)))
    (etypecase (directory-%contents self)
      (hash-table (if (gethash (entry-name entry) (directory-%contents self))
                    (error-already)
                    (setf (gethash (entry-name entry) (directory-%contents self)) entry)))
      (list       (if (assoc (entry-name entry) (directory-%contents self) :test (function string=))
                    (error-already)
                    (if (< (length (directory-%contents self)) #+clisp 30 #-clisp 6)
                      (setf (directory-%contents self)
                            (acons (entry-name entry) entry (directory-%contents self)))
                      (loop
                         :with table = (make-hash-table :test (function equal))
                         :for (k . v) :in (directory-%contents self)
                         :do (setf (gethash k table) v)
                         :finally (setf (gethash (entry-name entry) table) entry
                                        (directory-%contents self) table)))))))
  
  entry)



(defmethod delete-entry-named ((self directory) name)
  (etypecase (directory-%contents self)
    (hash-table (remhash name (directory-%contents self)))
    (list       (setf (directory-%contents self) (delete name (directory-%contents self)
                                                         :test (function string=)
                                                         :key (function car)))))
  self)



;;;----------------------------------------
;;;
;;;----------------------------------------

(defvar *default-user*  (intern-user  "root"))
(defvar *default-group* (intern-group "root"))

(defvar *default-directory-access-rights*
  (make-access-rights :owner :all
                      :group :read :access
                      :other :read :access))


(defun make-directories (root path)
  (let ((root (or root
                  (make-directory :name ""
                                  :owner *default-user*
                                  :group *default-group*
                                  :access-rights *default-directory-access-rights*))))
    (if path
      (let ((subdir (or (find-entry-named root (first path))
                        (add-entry root (make-directory :name (first path)
                                                        :parent root
                                                        :owner *default-user*
                                                        :group *default-group*
                                                        :access-rights *default-directory-access-rights*)))))
        
        (values (make-directories subdir (rest path)) root))
      (values root root))))



(defvar *allocation-block* 512
  "Number of bytes per allocation block.")


(defgeneric compute-disk-usage (entry)
  (:method ((self directory))
    (setf (entry-disk-usage self)
          (reduce (function +) (directory-contents self)
                  :key (function compute-disk-usage)
                  :initial-value 0)))
  (:method ((self regular-file))
    (setf (entry-disk-usage self)
          (* (ceiling (entry-size self)
                      *allocation-block*)
             *allocation-block*)))
  (:method ((self symbolic-link))
    (setf (entry-disk-usage self)
          (* (ceiling (length (symbolic-link-original-path self))
                      *allocation-block*)
             *allocation-block*)))
  (:method ((self entry)) 0))


;;;; THE END ;;;;
