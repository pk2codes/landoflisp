(defparameter *port* 8080)

(defun http-char (c1 c2 &optional (default #\Space))
  "decodes two decimal values represented as characters into the corresponding ASCII character"
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))


(defun decode-param (s)
  "decodes a string containing URL Encoded characters into the real ASCII representation. Stops parsing after %"
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (caddr (list lst)))))
                 (#\+ (cons #\space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))


(defun parse-params (s)
  "parses URL Params and transforms them into a list of tuples (PARAM . VALUE)"
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun parse-url (s)
  "Parses 1st line of Request header. format: METHOD relativePath HTTPVERSION. Returns '(relPath + tuples of param value)" 
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

(defun get-header (stream)
  "extracts every header value into '((HEADERNAME . value)) "
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))


(defun serve (request-handler)
  (format T "Started webserver on port ~d...~%~%" *port*)
  (let ((socket (socket-server *port*)))
    ; ensures that socket-server-close is called
    (unwind-protect
         (loop (with-open-stream (stream (socket-accept socket))
                 (let* ((url (parse-url (read-line stream)))
                        (path (car url))
                        (header (get-header stream))
                        
                        (params (append (cdr  url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (socket-server-close socket))))

(defparameter *day-names*
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter *month-names*
  '("Jan" "Feb" "Mar" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun get-header-date ()
  (multiple-value-bind
      (sec min hour date month year weekday dst-p tz)
      (get-decoded-time)
    (format NIL "~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT~@d"
            (nth weekday *day-names*)
            date
            (nth (- month 2) *month-names*)
            year
            hour min sec
            tz)))

(defun get-response-headers (code)
  (let ((date (get-header-date)))
    (format NIL "HTTP/1.1 ~d OK~%Date: ~A ~%Server: Lisp/1.0.0~%Content-Type:text/html~%~%"  code date)))

(defun send-response (code body)
 (format t "~a ~a"
            (get-response-headers code)
            body))

(defun js-func (name param-names body)
  (format nil "function ~a(~a){~a}"
          name
          (format nil "~{~A~^, ~}" param-names)
          body))


(defun js-block (js-code)
  (format nil "<script>~a</script>" js-code))

(defun js-hello-world ()
  "alert('hello world!')")

(defun html-wrapper (body)
  (format nil "<html><head>Awesome Lisp Server</head><body>~a</body></html>" body))


(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (send-response 200 "<html><form>What is your name? <input name='name' /></form></html>")
            (send-response 200  (format NIL "<html>I really like you, ~a! <3</html>" (cdr name)))))
      (send-response 200 (html-wrapper  (js-block (js-hello-world))))))

(serve #'hello-request-handler)



