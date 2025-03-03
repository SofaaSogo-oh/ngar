(asdf:load-system :ngar)

(defparameter *home-desktop-path*
  "/mnt/c/Users/NaumiK/Desktop")

(ngar:cp-into-desktop *home-desktop-path*
                 "note-16-56.xopp")
