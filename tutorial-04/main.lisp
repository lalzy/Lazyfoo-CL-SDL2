;;;; Tutorial 04: Key Presses

(defpackage #:tutorial-04
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-04)

(defun load-media (path)
  (sdl2:load-bmp path))

(defun file-path (path file)
  (let ((path-end (aref path (1- (length path)))))
    (if (or (string-equal path-end #\\) (string-equal path-end #\/))
	(concatenate 'string path file)
	(concatenate 'string path "/" file))))

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-04/assets/")))
	       (title "Tutorial 04: Key Presses") (width 640) (height 480))
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
	       (image (load-media (file-path path "press.bmp"))))
	   
	   (sdl2:with-event-loop ()
	     (:quit () t)
	     (:keydown (:keysym keysym)
		       (setf image
			     (case (sdl2:scancode keysym)
			       (:scancode-up (load-media (image-path path "up.bmp")))
			       (:scancode-down (load-media (image-path path "down.bmp")))
			       (:scancode-left (load-media (image-path path "left.bmp")))
			       (:scancode-right (load-media (image-path path "right.bmp")))
			       (t (load-media (image-path path "press.bmp"))))))
	     (:idle ()
		    (sdl2:blit-surface image nil surface nil)
		    (sdl2:update-window window)))
	   (sdl2:free-surface surface)))))))
