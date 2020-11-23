;;;; Tutorial 09: The Viewport

(defpackage #:tutorial-09
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-09)

(defun file-path (path file)
  (let ((path-end (aref path (1- (length path)))))
    (if (or (string-equal path-end #\\) (string-equal path-end #\/))
	(concatenate 'string path file)
	(concatenate 'string path "/" file))))

(defun load-texture (renderer path)
  (let* ((surface (sdl2-image:load-image path))
	 (texture (sdl2:create-texture-from-surface renderer (sdl2-image:load-image path))))
    (sdl2:free-surface surface)
    texture))


(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-09/assets/")))
	       (title "Tutorial 09: The Viewport") (width 640) (height 480))
  ;; Prevent slime from hanging if crashing
  (sdl2:make-this-thread-main
   (lambda ()
     (sdl2:with-init (:video)
       (sdl2:with-window (window :title title :w width :h height :flags '(:shown))
	 (sdl2:with-renderer (renderer window)

	   ;; Emacs on Windows doesn't like displaying the window
	   (sdl2:hide-window window)
	   (sdl2:show-window window)

	   ;; Window loads behind emacs on Windows.
	   (sdl2:raise-window window)

	   (sdl2-image:init '(:png))

	   (let ((top-left-viewport (sdl2:make-rect 0 0 (round width 2) (round height 2)))
		 (top-right-viewport (sdl2:make-rect (round width 2) 0 (round width 2) (round height 2)))
		 (bottom-viewport (sdl2:make-rect 0 (round height 2) width (round height 2)))
		 (texture (load-texture renderer (file-path path "viewport.png"))))
	     
	     (sdl2:with-event-loop ()
	       (:quit () t)
	       
	       (:idle ()
		      (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
		      (sdl2:render-clear renderer)

		      (sdl2:render-set-viewport renderer top-left-viewport)
		      (sdl2:render-copy renderer texture)
		      
		      (sdl2:render-set-viewport renderer top-right-viewport)
		      (sdl2:render-copy renderer texture)
		      
		      (sdl2:render-set-viewport renderer bottom-viewport)
		      (sdl2:render-copy renderer texture)
		      
		      (sdl2:render-present renderer)))
	     (sdl2:free-rect top-left-viewport)
	     (sdl2:free-rect top-right-viewport)
	     (sdl2:free-rect bottom-viewport)
	     (sdl2:destroy-texture texture)
	     (sdl2-image:quit))))))))
