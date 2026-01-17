(in-package :ghapi)

;;; Users ;;;

(defun user (username-or-id)
  (etypecase username-or-id
    (string (request "users/~A" username-or-id))
    (integer (request "user/~D" username-or-id))))

(defun ensure-username (username-or-id)
  (etypecase username-or-id
    (string username-or-id)
    (integer (cdr (assoc :login (user username-or-id))))))

;;; Clone ;;

(defun clone-repo (url &key base-directory)
  (when base-directory
    (let ((base-directory (uiop/pathname:ensure-directory-pathname base-directory)))
      (ensure-directories-exist base-directory)
      (uiop/os:chdir base-directory)))
  (uiop/run-program:run-program (list "git"  "clone" url)
                                :output *standard-output*
                                :error-output *error-output*))


(defun username-valid-p (username)
  (if (cl-ppcre:scan "^[a-zA-Z\\d\\-]+$" username)
      t nil))



;;; Org Repos ;;;

(defun org-data (&optional (org *org*))
  (request "orgs/~A" org))

(defun org-repos (&optional (org *org*))
  (with-requests
    (loop
       for page from 0
       for repos = (request "orgs/~A/repos?page=~D"
                            org page)
       while repos
       nconc repos)))


(defun org-repos-filter (org predicate)
  (multiple-value-bind (response code)
      (org-repos org)
    (declare (ignore code))
    (remove-if-not predicate response)))


(defun org-repos-prefix (prefix &key (org *org*) clone-directory)
  (let* ((n (length prefix))
         (prefix (string-downcase prefix))
         (repos (org-repos-filter org
                                  (lambda (repo)
                                    (let ((name (cdr (assoc :name repo))))
                                      (and (>= (length name) n)
                                           (string= prefix
                                                    (string-downcase name)
                                                    :end2 n)))))))
    ;; (map nil (lambda (repo)
    ;;            (print (cdr (assoc :name repo))))
    ;;      repos)

    ;; maybe clone
    (when clone-directory
      (dolist (repo repos)
        (let ((url (cdr (assoc :ssh--url repo))))
          (when url
            (clone-repo url :base-directory clone-directory)))))
    ;; result
    repos))



;;; Teams ;;;

(defun org-teams (&optional (org *org*))
  (request "orgs/~A/teams?per_page=128" org))

(defun org-team-ids (&optional (org *org*))
  (let ((teams (org-teams org)))
    (loop for team in teams
       collect (cdr (assoc :name team)))))

(defun org-member-ids (&optional (org *org*))
  (with-requests
    (let* ((org-data (request "orgs/~A" org))
           (org-plan (cdr (assoc :plan org-data)))
           (member-count (cdr (assoc :filled--seats org-plan))))
      (loop for m in (request "orgs/~A/members?per_page=~D" org member-count)
         collect (cdr (assoc :login m))))))

(defun org-team (&key (org *org*) team)
  (request "orgs/~A/teams/~A" org team))

(defun team-add-member (team member)
  (let ((member (ensure-username member)))
    (assert (username-valid-p member))
    (request-put (format nil "teams/~A/memberships/~A" team member))))

(defun org-team-add-members (&key
                               (org *org*)
                               team members)
  (let* ((team (org-team :org org :team team))
         (id (cdr (assoc :id team))))
    (loop for member in (ensure-list members)
       do
         (format t "~&Adding `~A'~%" member)
         (team-add-member id member))))

(defun org-delete-team (&key (org *org*) team slug)
  (assert (not (equalp "owners" team)))
  (assert (or team slug))
  (assert (not (and team slug)))
  (request-delete  "orgs/~A/teams/~A" org
                   (if slug
                       slug
                       (drakma:url-encode team :utf-8))))


(defun org-delete-member (&key (org *org*) member)
  (assert (username-valid-p *username*))
  (assert (username-valid-p member))
  (if (string= (string-upcase member)
               (string-upcase *username*))
      (error "Refusing to delete current user: ~A" *username*)
      (request-delete  "orgs/~A/memberships/~A" org member)))

;; TODO: recurse until everything is deleted
(defun org-purge (&key (org *org*)
                    keep-members
                    (keep-teams '("students")))
  (declare (type list keep-members keep-teams))
  (assert (username-valid-p *username*))
  (dolist (x keep-members)
    (check-type x string ))
  (dolist (x keep-teams)
    (check-type x string))
  ;; body
  (let ((members (sort (org-member-ids org) #'string-greaterp))
        (team-alist (sort (org-teams org)
                          #'string-greaterp
                          :key (lambda (alist) (cdr (assoc :name alist)))))
        keep*-members delete-members
        keep*-teams delete-teams
        keep*-slugs delete-slugs)
    ;; Find members
    (dolist (member members)
      (if (or (equalp member *username*)
              (position member keep-members :test #'equalp))
          (push member keep*-members)
          (push member delete-members)))
    ;; Find Teams
    (dolist (team team-alist)
      (let ((name (cdr (assoc :name team)))
            (slug (cdr (assoc :slug team))))
        (if (position name keep-teams :test #'equalp)
            (progn
              (push name keep*-teams)
              (push slug keep*-slugs))
            (progn
              (push name delete-teams)
              (push slug delete-slugs)))))


    (format t "~&Keep members: ~{~A~^, ~}" keep*-members)
    (format t "~&Keep teams: ~{~A~^, ~}" keep*-teams)
    (format t "~&Keep slugs: ~{~A~^, ~}" keep*-slugs)
    (format t "~&Delete members: ~{~A~^, ~}" delete-members)
    (format t "~&Delete teams: ~{~A~^, ~}" delete-teams)
    (format t "~&Delete slugs: ~{~A~^, ~}" delete-slugs)
    (terpri)
    (when (yes-or-no-p "Proceed?")
      (dolist (member delete-members)
        (format t "~&Deleting member ~A..." member)
        (org-delete-member :org org :member member))
      (loop for name in delete-teams
            for slug in delete-slugs
            do
               (format t "~&Deleting team ~A (~A)..." name slug)
               (org-delete-team :org org :slug slug)))))


;; (defun driver ()
;;   (let ((org (sb-posix:getenv "GHCLASS_ORG"))
;;         (cmd (sb-posix:getenv "GHCLASS_CMD"))
;;         (opt1 (sb-posix:getenv "GHCLASS_OPT1"))
;;         (opt2 (sb-posix:getenv "GHCLASS_OPT2")))
;;     (load-cred)
;;     (assert *username*)
;;     (assert *password*)
;;     (assert (not (zerop (length org))))
;;     (assert (not (zerop (length cmd))))
;;     (assert (not (zerop (length opt1))))

;;     (format t "~&> ghclass ~A ~A ~A ~A~%"
;;             org cmd opt1 (or opt2 ""))
;;     (cond
;;       ((string= cmd "clone-prefix")
;;        (org-repos-prefix opt1
;;                          :org org
;;                          :clone-directory (if (zerop (length opt2))
;;                                               (sb-posix:getcwd)
;;                                               opt2)))
;;       (t (error "Unknown command")))))




;; ;; (defun eof-json-decode (stream)
;; ;;   (handler-case (json:decode-json stream)
;; ;;     (end-of-file (e)
;; ;;       (declare (ignore e))
;; ;;       nil)))
