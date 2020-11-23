;;;; Tutorial 11: Clip Rendering and Sprite Sheets

(defpackage #:tutorial-11
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-11)

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

(defun render (renderer texture &key clip (x 0) (y 0))
  (let ((w (if clip (sdl2:rect-width clip) (sdl2:texture-width texture)))
	(h (if clip (sdl2:rect-height clip) (sdl2:texture-height texture))))
    (sdl2:render-copy renderer texture :source-rect clip :dest-rect (sdl2:make-rect x y w h))))

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-11/assets/")))
	       (title "Tutorial 11: Clip Rendering and Sprite Sheets") (width 640) (height 480))
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
	   
	   (let ((texture (load-texture renderer (file-path path "dots.png")))
		 (clip1 (sdl2:make-rect 0 0   100 100))
		 (clip2 (sdl2:make-rect 100 0   100 100))
		 (clip3 (sdl2:make-rect 0 100 100 100))
		 (clip4 (sdl2:make-rect 100 100 100 100)))
	     
	     (sdl2:with-event-loop ()
	       (:quit () t)
	       
	       (:idle ()
		      (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
		      (sdl2:render-clear renderer)

		      (render renderer texture :clip clip1
					       :x 0
					       :y 0)
		      
		      (render renderer texture :clip clip2
					       :x (- width (sdl2:rect-width clip2))
					       :y 0)
		      
		      (render renderer texture :clip clip3
					       :x 0
					       :y (- height (sdl2:rect-height clip3)))
		      
		      (render renderer texture :clip clip4
					       :x (- width (sdl2:rect-width clip4))
					       :y (- height (sdl2:rect-height clip4)))
		     
		      (sdl2:render-present renderer)))
	     (sdl2:destroy-texture texture)
	     (sdl2:free-rect clip1) (sdl2:free-rect clip2)
	     (sdl2:free-rect clip3) (sdl2:free-rect clip4)
	     (sdl2-image:quit))))))))
