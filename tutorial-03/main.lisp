;;;; Tutorial 03: Event Driven Programming

(defpackage #:Tutorial-03
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-03)

(defun load-media (path file)
  (sdl2:load-bmp (file-path path file)))

(defun file-path (path file)
  (let ((path-end (aref path (1- (length path)))))
    (if (or (string-equal path-end #\\) (string-equal path-end #\/))
	(concatenate 'string path file)
	(concatenate 'string path "/" file))))

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-03/assets/")))
	       (title "Tutorial 03: Event Driven Programming") (width 640) (height 480))
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
	 
	 (let ((surface (sdl2:get-window-surface window))
	       (image (load-media path "x.bmp")))
	   
	   (sdl2:blit-surface image nil surface nil)
					;(sdl2:fill-rect surface nil (sdl2:map-rgb (sdl2:surface-format surface) #xFF #xFF #xFF))
	   (sdl2:update-window window)

	   
	   (sdl2:with-event-loop ()
	     (:quit () t))
	   (sdl2:free-surface surface)))))))
