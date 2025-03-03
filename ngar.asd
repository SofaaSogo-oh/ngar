(asdf:defsystem :ngar
  :DEPENDS-ON (#:cl-ascii-table
               #:function-cache
               #:cl-ppcre)
  :serial t
  :COMPONENTS ((:file "package")
               (:file "main")))
