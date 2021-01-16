(in-package :ghapi)

(defvar *username* nil)
(defvar *password* nil)
(defvar *org* nil)

(defun load-cred (&optional (path (merge-pathnames (user-homedir-pathname)
                                                   (make-pathname :name ".ghapi"
                                                                  :type "cred"))))
  (when (probe-file path)
    (with-open-file (s path)
      (let ((list (read s)))
        (setq *username* (cdr (assoc :username list))
              *password* (cdr (assoc :password list))))))
  nil)
