;;;; Tutorial 07: Texture Loading and Rendering

(defpackage #:tutorial-07
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-07)

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

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-07/assets/")))
	       (title "Tutorial 07: Texture Loading and Rendering") (width 640) (height 480))
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

	   (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
	   (sdl2-image:init '(:png))

	   
	   (let* ((texture (load-texture renderer (file-path path "texture.png"))))
	     
	     (sdl2:with-event-loop ()
	       (:quit () t)
	       
	       (:idle ()

		      (sdl2:render-clear renderer)
		      (sdl2:render-copy renderer texture)
		      (sdl2:render-present renderer)))
	     (sdl2:destroy-texture texture)
	     (sdl2-image:quit))))))))
