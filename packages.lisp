;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the packages for the ls-lR browser.
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


(defpackage "LS-LR-FILE-SYSTEM"
  (:nicknames "LSLRFS")
  (:use "COMMON-LISP")
  (:shadow "DIRECTORY")
  (:EXPORT
   "NOW" "*TODAY*" "CURRENT-YEAR" "PARSE-SHORT-MONTH"
   "PARSE-LS-DATE" "FORMAT-LS-DATE"
   
   "USER"  "USER-P"  "MAKE-USER"  "USER-NAME"  "INTERN-USER"  "*DEFAULT-USER*"
   "GROUP" "GROUP-P" "MAKE-GROUP" "GROUP-NAME" "INTERN-GROUP" "*DEFAULT-GROUP*"
   
   "ACCESS-RIGHTS" "ACCESS-RIGHTS-P" "MAKE-ACCESS-RIGHTS"
   "PARSE-ACCESS-RIGHTS" "FORMAT-ACCESS-RIGTHS"
   "*DEFAULT-DIRECTORY-ACCESS-RIGHTS*"

   "ENTRY" "ENTRY-P" "MAKE-ENTRY" "ENTRY-NAME"
   "ENTRY-PARENT" "ENTRY-ACCESS-RIGHTS" "ENTRY-OWNER" "ENTRY-GROUP"
   "ENTRY-SIZE" "ENTRY-MODIFICATION-DATE" "ENTRY-DISK-USAGE"
   "ENTRY-PATHNAME"

   "REGULAR-FILE"  "REGULAR-FILE-P"  "MAKE-REGULAR-FILE"
   "NAMED-PIPE"    "NAMED-PIPE-P"    "MAKE-NAMED-PIPE"
   "SOCKET"        "SOCKET-P"        "MAKE-SOCKET"
   "SYMBOLIC-LINK" "SYMBOLIC-LINK-P" "MAKE-SYMBOLIC-LINK"
   "SYMBOLIC-LINK-ORIGINAL-PATH"
   
   "DIRECTORY" "DIRECTORY-P" "MAKE-DIRECTORY"
   "DIRECTORY-CONTENTS"
   "FIND-ENTRY-NAMED" "ADD-ENTRY" "DELETE-ENTRY-NAMED"
   "MAKE-DIRECTORIES"

   "*ALLOCATION-BLOCK*" "COMPUTE-DISK-USAGE")
  (:documentation
   "This package exports a pseudo file system structure."))


(defpackage "LS-LR-READER"
  (:nicknames "LSLRR")
  (:use "COMMON-LISP" "SPLIT-SEQUENCE" "LS-LR-FILE-SYSTEM")
  (:shadowing-import-from "LS-LR-FILE-SYSTEM" "DIRECTORY")
  (:EXPORT
   "LS-LR-FILE" "LS-LR-FILE-P" "LS-LR-FILE-ROOT-DIRECTORY"
   "OPEN-LS-LR-FILE" "CLOSE-LS-LR-FILE"  "WITH-OPEN-LS-LR-FILE"
   "CACHED-DIRECTORY" "CACHED-DIRECTORY-P"
   "ENTER-DIRECTORY" "LEAVE-DIRECTORY" "WITH-CACHED-DIRECTORY"
   "*WORKING-DIRECTORY*" "*ROOT-DIRECTORY*")
  
  (:documentation
   "This package exports functions to read ls-lR files.
Since it is expected that these files be big, we don't
load them in RAM entirely, but keep the directory tree.
When the contents of a directory is needed call ENTER-DIRECTORY
to load the non-directory entries, and call LEAVE-DIRECTORY to
release them."))



(defpackage "LS-LR-BROWSER"
  (:nicknames "LSLRB")
  (:use "COMMON-LISP" "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.COMMAND-LINE"
        "LS-LR-FILE-SYSTEM" "LS-LR-READER")
  (:shadowing-import-from "LS-LR-FILE-SYSTEM" "DIRECTORY")
  (:EXPORT
   "BROWSE-LS-LR-FILE"
   )
  
  (:documentation
   "This package exports functions to browse ls-lR file systems."))


;;;; THE END ;;;;
