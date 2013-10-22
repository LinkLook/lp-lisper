
(asdf:operate 'asdf:load-op 'drakma)
(asdf:operate 'asdf:load-op 'cl-html-parse)

(defpackage :spider-man
  (:use :cl 
	:drakma 
	:html-parse))

(in-package :spider-man)

(defvar *uri-todo* nil)
(defvar *uri-done* nil)
(defconstant +body-buf-size+ 65536)
(defvar *body-buf* (make-array +body-buf-size+ :element-type '(unsigned-byte 8) :fill-pointer t :adjustable t))
(defvar *spider-path-root* "/home/da/spider/")

	

(defun info-log-to-file (uri uri-b status headers reason)
  (with-open-file (log-file
		   (concatenate 'string *spider-path-root* "info.log")
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :append)
    (format log-file "~A~%~A~%~A~%~A~%~A~%~{~A~}~%" uri uri-b status headers reason (make-list 80 :initial-element "="))))

(defun err-log-to-file (message)
  (with-open-file (log-file
		   (concatenate 'string *spider-path-root* "err.log")
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :append)
    (format log-file "~A~%" message)))

(defun be-a-path (path)
  (let* ((new-path-1 (substitute #\_ #\? path))
	 (new-path-2 (substitute #\- #\& new-path-1)))
    (if (eq (aref new-path-2 (1- (length new-path-2))) #\/)
      (concatenate 'string new-path-2 "_") 
      (concatenate 'string new-path-2 "/_"))))

(defun parse-url (url-string)
  :documents "return (protocol domain path)"
  (let ((protocol-end (position #\: url-string)))
    (if (or (not protocol-end) (string-not-equal "://" url-string :start2 protocol-end :end2 (+ protocol-end 3)))
	(values nil nil nil)
    (let* ((domain-end (position #\/ url-string :start (if protocol-end (+ protocol-end 3) 0)))
	   (protocol (subseq url-string 0 protocol-end))
	   (domain (subseq url-string (+ 3 protocol-end) (if domain-end domain-end (length url-string))))
	   (path (if domain-end (subseq url-string domain-end) "/")))
      (values protocol domain (be-a-path path))))))
;(parse-url "ftp://www.a.b/s")

(defun parse-dom-tree-r (dom-tree uri-prifix)
  (let ((href-flag nil))
    (dolist (kw dom-tree)
      (cond ((listp kw) (parse-dom-tree-r kw uri-prifix))
	    ((keywordp kw)
	     (if (eq kw :href)
		 (setf href-flag t)))
	    ((stringp kw)
	     (when (and href-flag (string= uri-prifix kw :end2 (length uri-prifix)))
		 (push kw *uri-todo*)))
;		 (format t "push ~A TODO~%" kw)))
	    (t (err-log-to-file (format nil "Type=?~A~%" kw))))
      (if (not (eq kw :href))
	  (setf href-flag nil)))))


(defun parse-body-file (file-name uri-root)
  (handler-case
      (parse-dom-tree-r (html-parse:parse-html file-name) uri-root)
    (condition (c)
      (err-log-to-file (format nil "condition: ~A~%" c)))))

;(extract-href #P"spider/coolshell.cn" "http://coolshell.cn")

(defun save-body-file (body-stream file-name)
  (with-open-file (out-file 
		   (ensure-directories-exist file-name)
		   :direction :output 
		   :element-type '(unsigned-byte 8) 
		   :if-does-not-exist :create
		   :if-exists :supersede)
    (loop
       for bytes = (read-sequence *body-buf* body-stream)
	 while (and bytes (> bytes 0))
	 do (write-sequence *body-buf* out-file :end bytes))))

(defun save-body-to (uri-string)
  (multiple-value-bind (protocol domain path)
      (parse-url uri-string)
    (if protocol (pathname (concatenate 'string *spider-path-root* domain path))
	(progn
	  (err-log-to-file "Unknown protocol~%")
	  nil))))

;(defun url-encode (url-string)
;  (let* ((octets (string-to-octets url-string :external-format :utf-8)))))

(defun run-once (uri-root)
  (let ((uri-string (string-downcase (pop *uri-todo*))))
    (unless (find uri-string *uri-done* :test #'string=)
      (multiple-value-bind (body-stream status headers uri-b stream must-close reason)
	  (drakma:http-request uri-string :want-stream t)
	(let ((file-name (save-body-to uri-string))
	      (content-type (drakma:get-content-type headers)))
	  (when file-name 
	      (save-body-file body-stream file-name)
	      (when (and content-type 
			 (>= (length content-type) 9)
			 (string= (string-downcase content-type) "text/html" :end1 9)))
		(parse-body-file file-name uri-root)))
	(close body-stream)
	(info-log-to-file uri-string uri-b status headers reason)
	(if must-close (close stream)))
      (push uri-string *uri-done*)
      (format t "Done for ~A~%" uri-string))))

(defun spider (uri-root)
  (setf *uri-todo* nil)
  (setf *uri-done* nil)
  (push uri-root *uri-todo*)
  (unwind-protect
       (loop
	  while *uri-todo*
	  do (run-once uri-root))
    (with-open-file (out 
		     (pathname (concatenate 'string *spider-path-root* "done.lst"))
		     :if-does-not-exist :create :if-exists :supersede :direction :output)
      (format out "~{~A~%~}~%" *uri-done*))))

;(spider "http://coolshell.cn")
