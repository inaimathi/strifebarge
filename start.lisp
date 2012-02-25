(in-package :strifebarge)

(defparameter *web-server* (start (make-instance 'hunchentoot:easy-acceptor :port *server-port*)))
(web-folders "js" "img" "css")