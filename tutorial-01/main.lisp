;;;; Tutorial 01: Hello SDL

(defpackage #:tutorial-01
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-01)

(defun main (&aux (title "Tutorial 01: Hello SDL") (width 640) (height 480))
  ;; Prevent slime from hanging if crashing
  (sdl2:make-this-thread-main
   (lambda ()
     (sdl2:with-init (:video)
       (sdl2:with-window (window :title title :w width :h height :flags '(:shown))
	 ;; Emacs on Windows doesn't like displaying the window
	 (sdl2:hide-window window)
	 (sdl2:show-window window)

	 ;; Window loads behind emacs on Windows.
	 (sdl2:raise-window window)

	 (let ((surface (sdl2:get-window-surface window)))
	   (sdl2:fill-rect surface nil (sdl2:map-rgb (sdl2:surface-format surface) #xFF #xFF #xFF))
	   (sdl2:update-window window)
	   (sdl2:delay 2000)
	   (sdl2:free-surface surface)))))))

