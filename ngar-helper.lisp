(in-package #:ngar)

(defun exec-command (command)
  (uiop:run-program command
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun cp-into-desktop-cmd (path file)
  (format nil "cp ~a ~a" file path))

(defun cp-into-desktop (path file)
  (let ((cmd (cp-into-desktop-cmd 
              path file)))
    (format t "\"~a\"" cmd)
    (exec-command cmd))) 

