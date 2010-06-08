;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loader for the ls-lR browser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2009-12-26 <PJB> Created.
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

(asdf:oos 'asdf:load-op :split-sequence)
(asdf:oos 'asdf:load-op :cl-ppcre)

;; (map nil (function delete-file) (cl:directory "*.fas"))

(dolist (file '("command-line"
                ;; --
                "packages"
                "ls-lr-file-system"
                "ls-lr-reader"
                "ls-lr-browser"))
  (load (identity file)))

(defun unload ()
  (map nil (function delete-package)
       '("LS-LR-BROWSER" "LS-LR-READER" "LS-LR-FILE-SYSTEM" "COMMAND-LINE")))

(defun lslr ()
  (lslrb:browse-ls-lr-file "tmp.ls-lR"
                           :external-format #+clisp charset:iso-8859-1 #-clisp :default)
  #- (and)
  (lslrb:browse-ls-lr-file "20100101.ls-lR"
                           :external-format #+clisp charset:iso-8859-1 #-clisp :default)
  ;; (command-line:command-repl *query-io*)
  )

;; (LS-LR-READER:LOAD-LS-LR-FILE
;;  ;; "test.txt"
;;  "ls-lR-clean.txt"
;;  ;; "ls-lR.gz"
;;  )


;; csplit -k -f 20091226- -b "%04d.txt" -n 4 20091226.ls-lR 1000000 '{*}'
;; csplit -k -f 20091226- -b "%04d.txt" -n 4 20091226.ls-lR  100000 '{*}'

;;;; THE END ;;;
