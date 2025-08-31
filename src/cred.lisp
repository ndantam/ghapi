(in-package :ghapi)

(defvar *token* nil)
(defvar *username* nil)
(defvar *org* nil)


(defun load-token (&optional (path (merge-pathnames (user-homedir-pathname)
                                                   (make-pathname :name ".ghapi"
                                                                  :type "token"))))
  (when (probe-file path)
    (with-open-file (s path)
      (setq *token* (read-line s))))
  nil)




;;; Basic Auth no longer supported as of 2025-08-31

;; (defvar *password* nil)
;; (defun load-cred (&optional (path (merge-pathnames (user-homedir-pathname)
;;                                                    (make-pathname :name ".ghapi"
;;                                                                   :type "cred"))))
;;   (when (probe-file path)
;;     (with-open-file (s path)
;;       (let ((list (read s)))
;;         (setq *username* (cdr (assoc :username list))
;;               *password* (cdr (assoc :password list))))))
;;   nil)
