;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               command-line.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports functions to parse unix command lines
;;;;    following the unix shell syntax.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-01-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
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


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.COMMAND-LINE"
  (:nicknames "COMMAND-LINE")
  (:use "COMMON-LISP" "SPLIT-SEQUENCE")
  (:shadow "GET" "SET" "VARIABLE")
  (:export
   "COMMAND-REPL"
   "DEFINE-COMMAND" "&OPTION"

   "OPTION" "OPTION-P" "MAKE-OPTION" "OPTION-SHORT-NAMES" "OPTION-LONG-NAMES"
   "OPTION-TYPE" "OPTION-PARAMETER-NAME" "OPTION-DEFAULT-VALUE"
   "OPTION-DOCUMENTATION-STRING"
   "FLAG" "COUNTER"
   ;; "GET-SHORT-OPTION" "GET-LONG-OPTION" "PARSE-OPTIONS"

   "COMMAND" "COMMAND-P" "COMMAND-NAME" "COMMAND-OPTIONS" "COMMAND-PARAMETERS"
   "COMMAND-HELP-STRING"
   ;; "SIMPLE-PARSE-ORDINARY-LAMBDA-LIST"
   "PRINT-COMMAND-USAGE" "CALL-COMMAND" "COMMAND-EVAL"
   
   ;; Conditions (may be signaled by the commands):
   "COMMAND-ERROR"
   "COMMAND-ERROR-COMMAND" "COMMAND-ERROR-CONCRETE-COMMAND"
   "UNKNOWN-COMMAND-ERROR"
   "COMMAND-OPTION-ERROR"
   "COMMAND-OPTION-ERROR-OPTION" "COMMAND-OPTION-ERROR-CONCRETE-OPTION"
   "COMMAND-OPTION-UNKNON-OPTION-ERROR"
   "COMMAND-OPTION-MISSING-VALUE-ERROR"
   "COMMAND-OPTION-ERROR-VALUE-QUALIFIER"
   "COMMAND-OPTION-INVALID-VALUE-ERROR"
   "COMMAND-OPTION-ERROR-BAD-VALUE"
   "COMMAND-OPTION-ERROR-EXPECTED-TYPE")
  
  (:documentation"
    This package exports functions to parse unix command lines.

    Copyright Pascal J. Bourguignon 2010 - 2010
    
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
"))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.COMMAND-LINE")


;;;----------------------------------------
;;; Conditions
;;;----------------------------------------

(define-condition command-error (error)
  ((command          :accessor command-error-command               :initarg :command
                     :documentation "The command structure, if available.")
   (concrete-command :accessor command-error-concrete-command      :initarg :concrete-command
                     :documentation "The list of words making up the command line."))
  (:documentation "Condition signaled when an error concerning a given command occurs."))


(define-condition unknown-command-error (command-error)
  ()
  (:report (lambda (condition stream)
               (format stream "Unknown command: ~S~%Command line: ~{~S~^ ~}"
                       (first (command-error-concrete-command condition))
                       (command-error-concrete-command condition))))
  (:documentation "Condition signaled when an unknown command is given."))


(define-condition command-option-error (command-error)
  ((option          :accessor command-option-error-option          :initarg :option
                    :documentation "The option structure, if available.")
   (concrete-option :accessor command-option-error-concrete-option :initarg :concrete-option
                    :documentation "The exact option string as given on the command line."))
  (:documentation "Condition signaled when an error concerning a given option occurs."))


(define-condition command-option-unknown-option-error (error)
  ()
  (:report (lambda (condition stream)
               (format stream "Unknown option ~S"
                       (command-option-error-concrete-option condition)))))


(define-condition command-option-missing-value-error (command-option-error)
  ((value-qualifier :accessor command-option-error-value-qualifier :initarg :value-qualifier))
  (:report (lambda (condition stream)
               (format stream "Option ~S expected a ~A value"
                       (command-option-error-concrete-option condition)
                       (command-option-error-value-qualifier condition)))))


(define-condition command-option-invalid-value-error (command-option-error)
  ((value-qualifier :accessor command-option-error-value-qualifier :initarg :value-qualifier)
   (bad-value       :accessor command-option-error-bad-value       :initarg :bad-value)
   (expected-type   :accessor command-option-error-expected-type   :initarg :expected-type))
  (:report (lambda (condition stream)
               (format stream "Option ~S does not take a value such as ~S~
                               ~*~:[~;~2:*, but expects a ~A value of type ~A~]"
                       (command-option-error-concrete-option condition)
                       (command-option-error-bad-value       condition)
                       (command-option-error-value-qualifier condition)
                       (command-option-error-expected-type   condition)))))



;;;----------------------------------------
;;; Options
;;;----------------------------------------

(defstruct (option (:type list))
  "Describes an option.
See *TEST-OPTIONS* for a few examples.
SHORT-NAMES a string, each character being a short option alias (without the -).
LONG-NAMES  a list of strings each being a long option alias (without the --).
TYPE        a lisp type, or FLAG or COUNTER.
            FLAG options are switches (several occurences switch on and off).
            COUNTER options are numbers that are incremented for each occurence.
            Other types require a value converted to that type:
               BOOLEAN takes: false nil no 0 true t yes 1
               INTEGER takes an integer.
               STRING and others take any string.
PARAMETER-NAME is the name used in for the command function parameter.
DEFAULT-VALUE  is the default value for that option when it's not given.
DOCUMENTATION-STRING is the documentation string for the command help and lisp function.
"
  short-names long-names type parameter-name default-value documentation-string)


(defun get-short-option (character options)
  "
RETURN: the option in the OPTIONS list that has CHARACTER as short name, or NIL.
"
  (find character options
        :key (function option-short-names)
        :test (function position)))


(defun get-long-option (string options)
  "
RETURN: the option in the OPTIONS list that has STRING as long name, or NIL.
"
  (find string options
        :key (function option-long-names)
        :test (lambda (string strings) (member string strings :test (function string=)))))


(defun parse-options (arguments option-definitions)
  "
DO:                 Parses the ARGUMENTS according to the OPTION-DEFINITIONS.
ARGUMENTS:          A list of strings containing the arguments from the command line.
OPTION-DEFINITIONS: A list of OPTIONs accepted by the command.
RETURN:             A list of arguments to be passed to the command function.
NOTE:               The non-option arguments are passed as-is without checking.
"
  (loop
     :with options = (mapcar (lambda (option)
                                 (cons (option-parameter-name option)
                                  (option-default-value option)))
                             option-definitions)
     :with old-arguments = '()
     :while arguments
     :do (let ((arg (pop arguments)))
           (labels ((option-value (option)
                      (cdr (assoc (option-parameter-name option) options)))
                    ((setf option-value) (new-value option)
                      (setf (cdr (assoc (option-parameter-name option) options)) new-value))
                    (concrete-option ()
                      (subseq arg 0 (position #\= arg)))
                    (value-for-option (value option)
                      (let ((value (or value (pop arguments))))
                        (when (null value)
                          (error 'command-option-missing-value-error
                                 :option option 
                                 :concrete-option (concrete-option)
                                 :value-qualifier (option-parameter-name option)))
                        value))
                    (process-option (option value)
                      (if option
                        (case (option-type option)
                          ((flag)
                           (when value
                             (error 'command-option-invalid-value-error
                                    :option option
                                    :concrete-option (concrete-option)
                                    :bad-value value))
                           (setf (option-value option) (not (option-value option))))
                          ((counter)
                           (when value
                             (error 'command-option-invalid-value-error
                                    :option option
                                    :concrete-option (concrete-option)
                                    :bad-value value))
                           (incf (option-value option)))
                          ((boolean)
                           (let ((value (value-for-option value option)))
                             (cond
                               ((member value  '("false" "nil" "no"  "0") :test (function string-equal))
                                (setf (option-value option) nil))
                               ((member value  '("true"  "t"   "yes" "1") :test (function string-equal))
                                (setf (option-value option) t))
                               (t
                                (error 'command-option-invalid-value-error
                                       :option option
                                       :concrete-option (concrete-option)
                                       :bad-value value
                                       :expected-type '(member "false" "nil" "no" "0" "true" "t" "yes" "1"))))))
                          ((integer)
                           (let ((value (value-for-option value option)))
                             (handler-case (setf (option-value option) (parse-integer value))
                               (error ()
                                      (error 'command-option-invalid-value-error
                                             :option option
                                             :concrete-option (concrete-option)
                                             :bad-value value
                                             :expected-type 'integer)))))
                          (otherwise
                           (let ((value (value-for-option value option)))
                             (setf (option-value option) value))))
                        (error 'command-option-unknown-option-error
                               :concrete-option arg))))
             (cond
               ((string= "--" arg) (loop-finish))
               ((and (< 2 (length arg)) (char= #\- (aref arg 0)) (char= #\- (aref arg 1)))
                (let ((pos-equal (position #\= arg)))
                  (process-option (get-long-option (subseq arg 2 pos-equal) option-definitions)
                                  (and pos-equal (subseq arg (1+ pos-equal))))))
               ((and (< 1 (length arg)) (char= #\- (aref arg 0)))
                (loop
                   :for i :from 1 :below (length arg)
                   :do (process-option (get-short-option (aref arg i) option-definitions) nil)))
               (t (push arg old-arguments)))))
     :finally (return (nconc (mapcar (function cdr) options)
                             (nreconc old-arguments arguments)))))



(defparameter *test-options*
  '(("c"  ("total")    flag    grand-total-p nil "print a grand total?")
    ("bB" ("backward") flag    backward-p    nil "from the end to the beginning?")
    ("v"  ("verbose")  counter verbose-level 0   "increments the verbosity")
    ("k"  ("kill")     boolean killp         nil "kill it?")
    ("t"  ("timeout")  integer timeout       10  "timeout delay")
    ("n"  ("name")     string  name          ""  "user name"))
  "Some test options.")


(defun test/parse-options ()
  "Test PARSE-OPTIONS"
  (assert (equal (parse-options '() *test-options*)
                 '(NIL NIL 0 NIL 10 "")))
  (assert (equal (parse-options '("-c" "-c" "-cvvk" "yes" "--timeout=42"
                                  "--name" "Mycroft" "-n" "-B" "--verbose"
                                  "-" "Hello" "--" "youhou" "hi") *test-options*)
                 '(T NIL 3 T 42 "-B" "-" "Hello" "youhou" "hi")))
  :success)




;;;----------------------------------------
;;; Commands
;;;----------------------------------------

(defvar *commands* (make-hash-table :test (function equalp))
  "The command dictionary.
Note: case insensitive command names.")


(defstruct command
  "
A command is a lisp function that is invokable from a unix-like command line.
NAME:        The name of the command, by which it is invokable.
OPTIONS:     The list of options the command may take.
PARAMETERS:  An ordinary lambda-list specifying the other parameters the command function takes.
HELP-STRING: The help for the command (and docstring for the command fucntion).
FUNCTION:    The command function.
"
  name options parameters help-string
  function)



(defun find-command-named (name)
  "
RETURN: The command named NAME if it exists, or NIL.
"
   (gethash (string name) *commands*))


;;; The following two functions are for run-time creation and deletion
;;; of commands.  Compilation time command definition should be done
;;; with DEFINE-COMMAND.

(defun add-new-command (name options parameters help-string function)
  "
DO:     Create a new command and add to the command dictionnary.
"
  (setf (gethash (string name) *commands*)
        (make-command
         :name name
         :options options
         :parameters parameters
         :help-string help-string
         :function function)))


(defun delete-command-named (name)
  "
DO:     Delete the command named NAME from the command dictionnary.
"
  (remhash (string name) *commands*))




(defmacro define-command (name parameters help-string &body body)
  "
DO:     Defines a command with its function.  A lisp function with the
        same NAME is defined by define-command.

NOTE:   The lisp function takes a single argument, which is
        destructured, to be able to handle an arbitrary nummber of
        arguments (eg. expanded from wildcards).

NOTE:   The options are prepended to the mandatory parameters, and are
        therefore always expected, in the same order as given in the
        DEFINE-COMMAND option-lambda-list (PARAMETERS).

TODO:   Move declarations from the body to the function.

NAME:           The name of the command and lisp function.

PARAMETERS:     An option-lambda-list, that is, an ordinary lambda-list that may
                be prefixed by: &OPTION OPTION... [&MANDATORY]

HELP-STRING:    A documentation string.

BODY:           The command function body.
"
  (check-type help-string string) ; help string is mandatory.
  (let* ((other-pos  (position-if (lambda (item)
                                    (or (eql '&mandatory item)
                                        (member item lambda-list-keywords)))
                                  parameters))
         (options    (when (eql '&option (first parameters))
                       (subseq parameters 1 other-pos)))
         (parameters (let ((parameters (cond
                                         (other-pos (subseq parameters other-pos))
                                         (options   '())
                                         (t         parameters))))
                       (append (mapcar (function option-parameter-name) options)
                               (if (eql '&mandatory (first parameters))
                                   (rest parameters)
                                   parameters))))
         (varguments (gensym)))
    ;; (print (list name options parameters))
    `(progn

       (defun ,name (,varguments)
         ,help-string
         (destructuring-bind ,parameters ,varguments
           (locally ,@body))) ;; TODO: parse body and move declarations above.

       (add-new-command  ',name ',options ',parameters ',help-string (function ,name))
       ',name)))



;;;----------------------------------------
;;; Print command usage
;;;----------------------------------------

(defun simple-parse-ordinary-lambda-list (lambda-list)
  "
DO:     A simplistic ordinary lambda-list parse.
RETURN: Four lists, the mandatories, optionals, the rest,
        and the key parameter names.
NOTE:   The default values, keywords, and auxiliary variables are
        dropped, and no specific error checking is done (eg. if
        &optional is repeated, parsing will silently stop there).
"
  (flet ((clean (parameters)
           (mapcar (lambda (parameter) (if (listp parameter) (first parameter) parameter))
                   parameters)))
    (let ((mandatories (loop
                          :while (and lambda-list (not (member (first lambda-list) lambda-list-keywords)))
                          :collect (pop lambda-list)))
          (optionals   (clean (and lambda-list
                                   (eql (first lambda-list) '&optional)
                                   (pop lambda-list)
                                   (loop
                                      :while (and lambda-list (not (member (first lambda-list) lambda-list-keywords)))
                                      :collect (pop lambda-list)))))
          (resty        (and lambda-list
                             (eql (first lambda-list) '&rest)
                             (pop lambda-list)
                             lambda-list
                             (pop lambda-list)))
          (keys         (clean (and lambda-list
                                    (eql (first lambda-list) '&key)
                                    (pop lambda-list)
                                    (loop
                                       :while (and lambda-list (not (member (first lambda-list) lambda-list-keywords)))
                                       :collect (pop lambda-list))))))
      (values mandatories optionals resty keys))))


(defun print-command-usage (command)
  "
DO:    Prints a usage message for the given command.
"
  (let ((options (command-options command)))
    (multiple-value-bind (mandatory-parameters optional-parameters rest-parameter key-parameters)
        (simple-parse-ordinary-lambda-list (command-parameters command))
      (format t "~2%~A usage:~2%" (command-name command))
      (format t "    ~A~:[~; [OPTIONS]...~]~
                    ~:[~; [--]~]~
                    ~:[~;~:* ~{~A~^ ~}~]~
                    ~:[~;~:* [~{~A~^ ~}]~]~
                    ~:[~;~:* [~A...]~]~
                    ~:[~;~:* ~{~(:~A~) ~:*~A~^ ~}~]~
                    ~2%    ~:[~;OPTIONS:~%~]"
              (command-name command)
              options
              (or mandatory-parameters
                  optional-parameters
                  rest-parameter
                  key-parameters)
              mandatory-parameters
              optional-parameters
              rest-parameter
              key-parameters
              options)
      (dolist (option options)
        (if (member (option-type option) '(flag counter))
          (format t "~%        ~{-~C~^, ~}~:[~;, ~]~:*~{--~A~^, ~}~%"
                  (coerce (option-short-names option) 'list)
                  (option-long-names option))
          (let ((parameter (option-parameter-name option)))
            (flet ((couple (opt) (list opt parameter)))
              (format t "~%        ~:{-~C ~A~^, ~}~:[~;, ~]~:*~:{--~A=~A, ~2:*--~A ~A~^, ~}~%"
                      (map 'list (function couple) (option-short-names option))
                      (mapcar    (function couple) (option-long-names option)))
              (format t "            ~A is ~A~%"
                      (option-parameter-name option)
                      (option-type option)))))
        (format t "            ~A~%" (option-documentation-string option)))
      (format t "~A~%" (command-help-string command)))))



;; (print-command-usage (make-command :options *test-options*
;;                                    :parameters '(LEFT RIGHT &OPTIONAL UP DOWN &KEY FRONT BACK)
;;                                    :name "example"
;;                                    :help-string "An example command, just to test."
;;                                    :function (lambda (&rest args) (declare (ignore args)))))





(defun call-command (command args)
  "
DO:    Call the function of the command, passing it the  arguments.
"
  (funcall (command-function command) args))



;;;----------------------------------------
;;; Command REPL
;;;----------------------------------------


(defun parse-word (stream)
  "
DO:     Parse a command-line word from the STREAM.
RETURN: A list of words (strings).

NOTE:   Words are split on spaces, tabs and newlines,
        unless escaped with a backslash, or enclosing
        double or single quotes.  These escapes can
        also be used inside the words.

NOTE:   Escape characters (backslash, quotes) are removed
        from the returned word (unless escaped).
"
  (when (peek-char t stream nil nil)
    (let ((buffer (make-array 8 :element-type 'character
                              :adjustable t :fill-pointer 0
                              :initial-element #\space)))
      (loop
         :with state = :out
         :for ch = (read-char stream nil nil)
         :while ch
         :do (ecase state
               ((:out)
                (case ch
                  ((#\") (setf state :in-double))
                  ((#\') (setf state :in-single))
                  ((#\\) (setf state :back-out))
                  ((#\space #\tab #\newline)  (loop-finish))
                  (otherwise (vector-push-extend ch buffer))))
               ((:back-out)
                (vector-push-extend ch buffer)
                (setf state :out))
               ((:back-in-double)
                (vector-push-extend ch buffer)
                (setf state :in-double))
               ((:back-in-single)
                (vector-push-extend ch buffer)
                (setf state :in-single))
               ((:in-double)
                (case ch
                  ((#\") (setf state :out))
                  ((#\\) (setf state :back-in-double))
                  (otherwise (vector-push-extend ch buffer))))
               ((:in-single)
                (case ch
                  ((#\') (setf state :out))
                  ((#\\) (setf state :back-in-single))
                  (otherwise (vector-push-extend ch buffer)))))
         :finally (ecase state
                    ((:out))
                    ((:in-single) (error "Missing a closing single-quote."))
                    ((:in-double) (error "Missing a closing double-quote."))
                    ((:back-in-double
                      :back-in-single
                      :back-out) (error "Missing a character after last backslash."))))
      (copy-seq buffer))))


(defun split-words (text)
  "
DO:     Splits the words from the LINE.
        Do not expands anything.
        (Call PATHNAME-EXPAND of each WORD).

RETURN: A list of WORDs split from the LINE.

TEXT:   A string containing a unix command line.
"
  (with-input-from-string (input text)
    (loop
       :for word = (parse-word input)
       :while word
       :collect word)))


(defun read-lines (&optional (stream t) (eof-error-p t) (eof-value t))
  "
DO:     Read lines from the stream and concatenate them while they're
        ended with a backslash.
RETURN: A string containing the multiline.
"
  (loop
     :with result = '()
     :for line = (read-line stream eof-error-p eof-value)
     :while line
     :do (if (and (plusp (length line))
                  (char= #\\ (aref line (1- (length line)))))
           (progn
             (push (subseq line 0 (1- (length line))) result)
             (push #\newline result))
           (progn
             (push line result)
             (loop-finish)))
     :finally (return (with-output-to-string (output)
                        (dolist (item (nreverse result))
                          (princ item output))))))






(defun write-prompt (&optional (stream t))
  (format stream "~&> ")
  (finish-output stream))


(defun write-error (error &optional (stream t))
  (let ((*print-pretty* nil))
   (format stream "~&Error: ~A~%" error))
  (finish-output stream))

(defun command-eval (words)
  "
DO:     Evaluates the command line WORDS.
        The first word must be the name of a command (case insensitive).
        The other words are parsed as options and arguments for the command.
RETURN: The result of the command.
"
  (when words
    (let ((command (find-command-named (first words))))
      (if command
        (handler-bind
            ((command-option-error
              (lambda (err)
                (setf (command-error-command err) command)
                (if (truep (variable "debug-on-error"))
                    (progn
                      (write-error err)
                      (invoke-debugger err))
                    (error err))))
             (command-error
              (lambda (err)
                (when (truep (variable "debug-on-error"))
                  (write-error err)
                  (invoke-debugger err))))
             (error
              (lambda (err)
                (when (truep (variable "debug-on-error"))
                  (write-error err)
                  (invoke-debugger err)))))
            (let* ((arguments (parse-options (rest words)
                                             (command-options command)))
                   (arguments
                    (multiple-value-bind (mandies opties resty keyies)
                        (simple-parse-ordinary-lambda-list
                         (command-parameters command))
                      (declare (ignore resty))
                      (if keyies
                          (let ((start
                                 (+ (length (command-options command))
                                    (length mandies) (length opties))))
                            (append
                             (subseq arguments 0 start)
                             (loop
                                :for (k v) :on (subseq arguments start)
                                :by (function cddr)
                                :collect (if (and (< 1 (length k))
                                                  (char= #\: (aref k 0)))
                                             (intern (string-upcase (subseq k 1))
                                                     "KEYWORD")
                                             k)
                                :if v :collect v)))
                          arguments))))
              (call-command command arguments)))
        (error 'unknown-command-error :concrete-command words)))))



;;;----------------------------------------
;;; A few built-in commands:
;;;----------------------------------------


(define-command quit ()
  "Terminates the command REPL."
  (throw 'quit nil))



(define-command help (&optional command)
  "If a COMMAND is given, then prints the usage of the COMMAND
else prints the list of available commands."
  (if command
    (let ((cmd (gethash command *commands*)))
      (if cmd
        (print-command-usage cmd)
        (error 'unknown-command-error :concrete-command (list command))))
    (let ((commands '()))
      (maphash (lambda (k v) (declare (ignore k)) (push v commands)) *commands*)
      (setf commands (sort commands
                           (function string<)
                           :key (function command-name)))
      (let ((width (reduce (function max) commands
                           :key (lambda (command) (length (string (command-name command))))
                           :initial-value 0)))
        (dolist (command commands)
          (format t (format nil "~%~~VA ~~{~~,,1@<~~%~VA ~~,70:;~~A~~>~~}~~%" width "") 
                  width (command-name command) 
                  (split-sequence-if 
                   (lambda (x) (member x '(#\newline #\space))) 
                   (command-help-string command))))
        (format t "~%"))))
  (values))


(define-command echo (&rest words)
  "Prints all its arguments."
  (format t "~{~A~^ ~}~%" words)
  words)


(defun println (&rest things)
  (format t "~{~A~^ ~}~%" things))



;;;----------------------------------------
;;; Variables
;;;----------------------------------------

(defvar *variables* (make-hash-table :test (function equal))
  "A table of command-line 'variables' readable with the GET command
and created and modifiable with the SET command.")


(defun variable (name)
  (gethash name *variables*))


(defun (setf variable) (new-value name)
  (setf (gethash name *variables*) new-value))


(defun truep (value)
  (member value '("true" "yes" "t" "1")
          :test (function string-equal)))


(define-command get (&optional variable)
  "Echoes the value of the VARIABLE if given, otherwise list all the variables."
  (if variable
      (println (variable variable))
      (let ((variables '()))
        (maphash (lambda (k v) (setf variables (acons k v variables))) *variables*)
        (let* ((variables (sort variables (function string<) :key (function car)))
               (width (reduce (function max) variables
                              :key (lambda (kv) (length (car kv)))
                              :initial-value 0)))
          (dolist (kv variables)
            (destructuring-bind (k . v) kv
              (format t "~VA ~S~%" width k v)))))))


(define-command set (variable value)
  "Changes the value of the variables and echoes the new value."
  (println (setf (variable variable) value)))


(define-command unset (variable)
  "Delete the variable."
  (remhash variable *variables*))



;;;----------------------------------------
;;; Lisp REPL
;;;----------------------------------------

(defmacro handling-errors (&body body)
  `(HANDLER-CASE (progn ,@body)
     (simple-condition 
         (ERR) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&")
       (finish-output))
     (condition 
         (ERR) 
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err)
       (finish-output))))


(defun repl ()
  (loop
     :with +eof+ = (gensym)
     :with +exit-forms+ = '(:quit :exit :continue)
     :for hist :from 1
     :initially (format t "~2%Welcome to Common Lisp!~%Exit from Lisp REPL with either ~{~S~^, ~}" +exit-forms+)
     :do (progn
           (format t "~%~A[~D]> " (package-name *package*) hist)
           (finish-output)
           (handling-errors
            (setf - (read *standard-input* nil +eof+))
            (when (or (eq - +eof+) (member - +exit-forms+))
              (loop-finish))
            (let ((results (multiple-value-list (eval -))))
              (shiftf +++ ++ + -)
              (shiftf /// // / results)
              (shiftf *** ** * (first /)))
            (format t "~& --> ~{~S~^ ;~%     ~}~%" /)
            (finish-output)))))


(define-command lisp (&optional expression)
  "With an argument, evaluates it as a lisp form.
Without an argument, enters a Lisp REPL."
  (if expression
      (println (eval (read-from-string expression)))
      (progn
       (repl)
       (format t "~%Welcome back to the command line REPL!~%"))))

;;;----------------------------------------
;;; A shell repl
;;;----------------------------------------

(defun run-shell-command (command)
  (ext:shell command))

(defun run-shell ()
  (ext:shell ))

(define-command shell (&optional expression)
  "With an argument, evaluates it as a shell command.
Without an argument, enters a shell REPL."
  (if expression
      (println (run-shell-command expression))
      (progn
        (run-shell)
        (format t "~%Welcome back to the command line REPL!~%"))))


;;;----------------------------------------
;;; Miscellaneous commands
;;;----------------------------------------

(define-command divide (numerator denominator)
  "Divides the NUMERATOR by the DENOMINATOR."
  (let ((*read-eval* nil))
    (println (/ (read-from-string numerator)
                (read-from-string denominator)))))


;;;----------------------------------------
;;; The Command REPL
;;;----------------------------------------

(defun command-repl (iostream
                     &key (prompt 'write-prompt)
                     (filter-command-line 'identity))
  "
DO:     Read, evaluates and prints the result of commands given
        on the iostream as command lines.
        Exits when the command QUIT is given.
"
  (catch 'quit
    (loop
       :with package = (find-package "COMMON-LISP-USER")
       :initially (format t "~&Welcome to the command line REPL!~%")
       :do (restart-case
               (loop
                  :for text = (progn (funcall prompt iostream)
                                     (read-lines iostream nil nil))
                  :while text
                  :do (let ((words (split-words text)))
                        ;; The toplevel error handling must be a handler-case
                        ;; to avoid entering the debugger from the inner handler-binds.
                        (handler-case
                            (handler-bind
                                ((error
                                  (lambda (err)
                                    (when (truep (variable "debug-on-error"))
                                      (write-error err iostream)
                                      (invoke-debugger err)))))
                                               
                              (let ((*standard-input*  iostream)
                                    (*standard-output* iostream)
                                    (*package* package))
                                (let ((result
                                       (command-eval
                                        (funcall filter-command-line words))))
                                  (declare (ignorable result))
                                  ;; (print result iostream)
                                  )
                                (setf package *package*)))
                          (unknown-command-error (err)
                            (write-error err iostream))
                          (command-error (err)
                            (write-error err iostream)
                            (when (command-error-command err)
                              (help (command-name
                                     (command-error-command err)))))
                          (error (err)
                            (write-error err iostream)
                            (when (truep (variable "debug-on-error"))
                              (invoke-debugger err))))))
             (continue () :report "Return to the REPL"))))
  (format iostream "~&Bye.~%")
  (values))


;;;; THE END ;;;;

