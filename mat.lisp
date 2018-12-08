(mapc #'ql:quickload '("cl-who" 
		       "clack" 
		       "cl-js-generator"))

(in-package #:cl-js-generator)


(setq cl-who:*attribute-quote-char* #\")
(setf cl-who::*html-mode* :html5)

(defparameter *ssl-port* 7777)
(defparameter *server-ip* 
  (let ((ipstr (with-output-to-string (s)
		 (sb-ext:run-program "/usr/bin/hostname" '("-i") :output s)
		 )))
    (string-trim 
     '(#\Space #\Newline #\Backspace #\Tab 
       #\Linefeed #\Page #\Return #\Rubout)
     ipstr)
    ))


(defun handler (env)
  '(200 nil ("hello world")))

;; openssl req -new -x509 -nodes -out /tmp/server.crt -keyout /tmp/server.key



(unless (and (probe-file "/tmp/server.key")
	     (probe-file "/tmp/server.crt"))
  ;; generate keys if they don't exist
  (let* ((p (sb-ext:run-program "/usr/bin/openssl" '("req" "-new" "-x509"
						   "-nodes"  "-out"
						   "server.crt" "-keyout"
						   "server.key")
			      :directory "/tmp/" :output :stream :input :stream
			      :wait nil))
       (stream-in (sb-ext:process-input  p))
       (stream-out (sb-ext:process-output p)))
  (flet ((consume ()
	   (loop while (listen stream-out) do
		(format t "~a" (read-char stream-out))))
	 (consume-until-colon ()
	   (loop for char = (read-char stream-out nil 'foo)
	      until (or (eq char 'foo)
			(eq #\: char))
	      do (format t "~a" char)
		))
	 (consume-until-colon-nowait ()
	   (loop while (listen stream-out) do
		(let ((line (read-line stream-out nil 'foo)))
		  (print line)
		  ))))
    (loop for e in
	 '("NL"
	   "Noord-Brabant" "Veldhoven" "ck" "certifacte_unit"
	   "nn" "kielhorn.martin@gmail.com")
       do
	 (consume-until-colon)
	 (write-line (format nil "~a~%" e) stream-in)
	 (format t "~&> ~a~%" e)
	 (finish-output stream-in)))
  (close stream-in)
  (sb-ext:process-wait p)
  (sb-ext:process-close p)))


(progn
  (defvar *clack-server* nil) ;; initialize with nil
  (when *clack-server* ;; stop pre-existing server
    (clack.handler:stop *clack-server*)
    (setf *clack-server* nil))
  (setf *clack-server* ;; start new server
	(clack:clackup
	 (lambda (env)
	   (funcall 'handler env))
	 :port *ssl-port*
	 :ssl t :ssl-key-file  #P"/tmp/server.key" :ssl-cert-file #P"/tmp/server.crt"
	 :use-default-middlewares nil)))



(let ((script-str
       (cl-js-generator::beautify-source
	`(let-g ((bla 3))
		(setf document.body.innerHTML (Tag.toHTML (todoMVC)))))))
  (defun handler (env)
    (destructuring-bind (&key server-name remote-addr path-info remote-port &allow-other-keys) env
      (cond
	((string= "/" path-info)
	 `(200 (:content-type "text/html; charset=utf-8")
	       (,(cl-who:with-html-output-to-string (s)
		   (cl-who:htm
		    (:html
		     (:head
		      (:meta :charset "utf-8")
		      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
		      (:title "matrixjs todomvc")
		      (:link :rel "stylesheet" :href "common/base.css")
		      (:link :rel "stylesheet" :href "common/index.css")
		      (:script :src "https://rawgit.com/flatiron/director/master/build/director.min.js")
		      (:script :src "js/Cells.js")
		      (:script :src "js/Model.js")

		      (:script :src "js/mxWeb.js")
		      (:script :src "js/app.js")
		      (:script :src "js/Todo.js")
		      )
		  
		     (:body (:h1 "script failed to load.")
			    (:p (princ env s))
			    (:script :type "text/javascript"
				     (princ script-str s)
				     ))))))))
	((or (string= "/common/base.css" path-info)
	     (string= "/common/index.css" path-info)
	     (string= "/js/Cells.js" path-info)
	     (string= "/js/Model.js" path-info)
	     (string= "/js/mxWeb.js" path-info)
	     (string= "/js/app.js" path-info)
	     (string= "/js/Todo.js" path-info))
	 `(200 (:content-type "text/javascript; charset=utf-8")
	       (,(let* ((fn (format nil "/home/martin/stage/cl-try-matrix-js/~a"
				    (subseq path-info 1 (length path-info)))))
		   (format t "file: ~a" fn)
		   (with-open-file (s fn)
		     (let* ((size (file-length s))
			    (str (make-string size)))
		       (read-sequence str s)
		       str))))))))))


