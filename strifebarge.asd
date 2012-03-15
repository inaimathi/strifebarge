;; StrifeBarge is a multiplayer, HTML-based guessing game
;; Copyright (C) 2012  Inaimathi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; strifebarge.asd

(asdf:defsystem #:strifebarge
  :serial t
  :author "leo.zovic@gmail.com"
  :maintainer "leo.zovic@gmail.com"
  :licence "AGPL v3"
  :depends-on (#:hunchentoot
               #:cl-who
	       #:cl-ppcre
	       #:cl-json
	       #:ironclad
               #:parenscript
               #:cl-css
	       #:cl-actors
               #:clsql)
  :components ((:file "package")
	       (:file "util") (:file "model") 
	       (:file "js-macros") (:file "js") (:file "css")
	       (:file "space") (:file "ship") (:file "board") (:file "game") (:file "history-event") (:file "player")
               (:file "strifebarge")
	       (:file "start")))

