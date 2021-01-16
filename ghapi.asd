(asdf:defsystem ghapi
  :version "0.0.1"
  :description "Interface to GitHub REST API"
  :depends-on (:drakma
               :cl-json
               :flexi-streams
               :cl-ppcre
               :alexandria)
  :license :bsd-2
  :homepage "https://github.com/ndantam/ghapi"
  :source-control "https://github.com/ndantam/ghapi"
  :author "Neil T. Dantam"
  :components ((:file "src/package")
               (:file "src/cred" :depends-on ("src/package"))
               (:file "src/io" :depends-on ("src/package"))
               (:file "src/ghapi" :depends-on ("src/cred" "src/io"))))
