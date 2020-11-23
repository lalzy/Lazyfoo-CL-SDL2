;;;; Tutorial 05: Optimized Surface Loading and Soft Stretching

(defpackage #:tutorial-05
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-05)


(defun load-surface (path file format)
  (sdl2:convert-surface (sdl2:load-bmp (file-path path file)) format))


(defun file-path (path file)
  (let ((path-end (aref path (1- (length path)))))
    (if (or (string-equal path-end #\\) (string-equal path-end #\/))
	(concatenate 'string path file)
	(concatenate 'string path "/" file))))

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-05/assets/")))
	       (title "Tutorial 05: Optimized Surface Loading and Soft Stretching") (width 640) (height 480))
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
	 
	 (let* ((surface (sdl2:get-window-surface window))
		(image (load-surface path "stretch.bmp" (sdl2:surface-format surface)))
		(rect (sdl2:make-rect 0 0 width height)))
	   
	   (sdl2:with-event-loop ()
	     (:quit () t)
	     
	     (:idle ()
		    (sdl2:blit-scaled image nil surface rect)
		    (sdl2:update-window window)))
	   (sdl2:free-surface surface)
	   (sdl2:free-rect rect)))))))
