(in-package :strifebarge)

(defun compile-js (file-name origin js) 
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "//////////~%// This is a generated file. ~%// If you want to edit this javascript, tweak '~a' and re-evaluate it instead.~%//////////~%~%" origin)
    (format stream js)))

;;;;;;;;;;;;;;; basic shortcuts
(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro fn (&body body) `(lambda () ,@body))

(defpsmacro doc-ready (&body body) 
  `($ document (ready (fn ,@body))))

(defpsmacro parse-json (target)
  `(chain j-query (parse-j-s-o-n ,target)))

(defpsmacro post-to (target-page data-hash on-success) ; data hash declared like (create :k v ...)
  "target-page is a page url.
data-hash is the data sent along as the post request; declared as (create :k v ...)
on-success is a function to run on a successful response; 
it should expect a single argument (the data returned by the target handler)"
  `(chain $ (post ,target-page
		  ,data-hash
		  ,on-success)))

;;;;;;;;;;;;;;; SSE specific
(defpsmacro define-event-source (name source-url)
  `(defvar ,name (new (-event-source ,source-url))))

(defpsmacro define-event-listener (event-source-name message-type on-receive)
  "event-source-name must be a defined event source. 
message-type is the event label sent by the server for this action (the default label is 'message').
on-receive is a function called when a satisfying message is received. It should take one argument (the event)."
  `(chain ,event-source-name (add-event-listener ,message-type ,on-receive false)))

;;;;;;;;;;;;;;; StrifeBarge specific
(defpsmacro $-space-at ((x y) &rest chains)
  `($ "#game-board tr" (eq ,y) (children "td") (eq ,x) ,@chains))