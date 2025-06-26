(asdf:defsystem :ngar
  :DEPENDS-ON (#:cl-ascii-table
               #:function-cache
               #:uiop
               #:cl-ppcre)
  :serial t
  :COMPONENTS ((:file "package")
               (:file "ngar-helper")
               (:file "bitset-utils")
               (:file "main")))
