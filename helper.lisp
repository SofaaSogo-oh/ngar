(asdf:load-system :ngar)

(defparameter *desktop-path*
  "/mnt/c/Users/NaumiK/Desktop")

(defparameter *desktop-path*
  "/mnt/c/Users/User/Desktop")

(ngar:list-cp-into-desktop *DESKTOP-PATH*
                           (list
                             "note-16-56.xopp"))
