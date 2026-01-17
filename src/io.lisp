(in-package :ghapi)

(defvar *body* nil)
(defvar *status-code* nil)
(defvar *headers* nil)
(defvar *response* nil)

(defvar *stream* nil)

(defun read-lines (place)
  (etypecase place
    ((or pathname string)
     (with-open-file (stream place)
       (read-lines stream)))
    (stream
     (loop
        for line = (read-line place nil nil)
        while line
        when (> (length line) 0)
        collect line))))


(defmacro with-requests (&body body)
  `(let ((*stream* (or *stream* :open)))
    ,@body))

(defun %request (&key
                   (path "")
                   (method :get))
  (unless (and (boundp '*token*) *token*)
    (load-token))
  (assert (boundp '*token*))
  (assert '*token*)
  (let* ((url (format nil "https://api.github.com/~A" path)))
    (multiple-value-bind
          (body status-code headers uri stream)
        (drakma:http-request url
                             :method method
                             :additional-headers
                             `(("Authorization" . ,(concatenate 'string "Bearer "
                                                                *token*))
                               ("X-GitHub-Api-Version". "2022-11-28"))
                             :close (if *stream* nil t)
                             :stream (if (eq *stream* :open)
                                         nil
                                         *stream*)
                             :decode-content t
                             ;; :basic-authorization (list *username* *password*)
                             )
      (declare (ignore uri))
      (when *stream*
        (setq *stream* stream))
      (setq *body* body
            *status-code* status-code
            *headers* headers)
      (ecase status-code
        (200
         (let ((string (flexi-streams:octets-to-string body :external-format :utf-8)))
           (setq *response*
                 (json:decode-json-from-string string))))
        (204
         (values))))))

(defun request (format &rest args)
  (%request :path (apply #'format nil format args)
            :method :get))

(defun request-put (format &rest args)
  (%request :path (apply #'format nil format args)
            :method :put))

(defun request-delete (format &rest args)
  (%request :path (apply #'format nil format args)
            :method :delete))
