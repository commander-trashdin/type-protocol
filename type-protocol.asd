;;;; type-protocol.asd

(asdf:defsystem #:type-protocol
    :description "Describe type-protocol here"
    :author "Your Name <your.name@example.com>"
    :license  "Specify license here"
    :version "0.0.1"
    :serial t
    :depends-on (#:closer-mop)
    :components ((:file "package")
                 (:file "definitions")
                 (:file "numeric")
                 (:file "enum")
                 (:file "custom")
                 (:file "classes")
                 (:file "structs")
                 (:file "fixed-containers")
                 (:file "set-like")
                 (:file "arb-containers")
                 (:file "satisfies")
                 (:file "functions")))
