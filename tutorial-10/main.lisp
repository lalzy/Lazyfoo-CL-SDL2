;;;; Tutorial 10: Color Keying

(defpackage #:tutorial-10
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-10)

(defun file-path (path file)
  (let ((path-end (aref path (1- (length path)))))
    (if (or (string-equal path-end #\\) (string-equal path-end #\/))
	(concatenate 'string path file)
	(concatenate 'string path "/" file))))

(defun load-texture (renderer path)
  (let ((surface (sdl2-image:load-image path))
	texture)
    (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface) 0 #xFF #xFF))
    (setf texture (sdl2:create-texture-from-surface renderer surface))
    (sdl2:free-surface surface)
    texture))
	  
(defun render (renderer texture &optional (x 0) (y 0))
  (let ((w (sdl2:texture-width texture))
	(h (sdl2:texture-height texture)))
    (sdl2:render-copy renderer texture :dest-rect (sdl2:make-rect x y w h))))

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-10/assets/")))
	       (title "Tutorial 10: Color Keying") (width 640) (height 480))
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
	   
	   (let ((foo (load-texture renderer (file-path path "foo.png")))
		 (background (load-texture renderer (file-path path "background.png"))))
	     
	     (sdl2:with-event-loop ()
	       (:quit () t)
	       
	       (:idle ()
		      (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
		      (sdl2:render-clear renderer)
		      
		      (render renderer background)
		      (render renderer foo 240 190)
		      
		      (sdl2:render-present renderer)))
	     (sdl2:destroy-texture foo)
	     (sdl2:destroy-texture background)
	     (sdl2-image:quit))))))))
