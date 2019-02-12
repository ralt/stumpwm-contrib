(defpackage :remember-monitors
  (:use :common-lisp))

(in-package :remember-monitors)

(defvar *lock* (bt:make-lock))
(defvar *variable* (bt:make-condition-variable))
(defvar *main-loop-thread* (bt:make-thread #'main-loop
					   :name "remember-monitors"))

(defvar *ignored-commands* '(stumpwm:run-shell-command)
  "List of commands to ignore. Purely there for performance reasons.")
(export '*ignored-commands*)

(defun head-hook (new-head screen)
  (declare (ignore new-head screen))
  (bt:condition-notify *variable*))

(defun post-command-hook (command)
  ;; Filter out a bunch of common commands that are irrelevant,
  ;; e.g. run-shell-command to get the time for modeline, i.e. it
  ;; doesn't change the list of screens/windows at all
  (unless (member command *ignored-commands*)
    (bt:condition-notify *variable*)))

(defun build-layout (current-screens)
  (let ((layout (make-hash-table :test #'equal)))
    (setf (gethash (format nil "~{~a~^:~}"
			   (mapcar #'stumpwm::screen-id current-screens))
		   layout)
	  (build-layout-for-screens current-screens))
    layout))

(defun build-layout-for-screens (screens)
  (let ((layout (make-hash-table :test #'equal)))
    (dolist (screen screens)
      (setf (gethash (stumpwm::screen-id screen) layout)
	    (mapcar #'xlib:window-id
		    (stumpwm::screen-mapped-windows screen))))
    layout))

(defun main-loop ()
  (bt:with-lock-held (*lock*)
    (let* ((current-screens (sort screen-list
				  #'(lambda (screen1 screen2)
				      (> (stumpwm::screen-id screen1)
					 (stumpwm::screen-id screen2)))))
	   (layout (build-layout current-screens)))
      (loop
	 (bt:condition-wait *variable* *lock*)
	 (let* ((new-screens (sort screen-list
				   #'(lambda (screen1 screen2)
				       (> (stumpwm::screen-id screen1)
					  (stumpwm::screen-id screen2)))))
		(new-layout (build-layout-for-screens new-screens)))
	   (unless (all-screens-are-the-same current-screens new-screens)
	     ;; assign something in `layout` too...
	     (move-orphan-windows)
	     (move-back-older-windows-to-old-screen-that-just-came-back)
	     (setf current-screens new-screens)
	     (setf layout new-layout)))))))

(stumpwm:add-hook stumpwm:*new-head-hook* (lambda (new-head screen)
					    (head-hook new-head screen)))
(stumpwm:add-hook stumpwm:*post-command-hook* (lambda (command)
						(post-command-hook command)))
