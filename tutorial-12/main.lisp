;;;; Tutorial 12: Color Modulation

(defpackage #:tutorial-12
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-12)

(defun file-path (path file)
  (let ((path-end (aref path (1- (length path)))))
    (if (or (string-equal path-end #\\) (string-equal path-end #\/))
	(concatenate 'string path file)
	(concatenate 'string path "/" file))))

(defun load-texture (renderer path &key (color-key nil))
  (let ((surface (sdl2-image:load-image path))
	texture)
    (when color-key
      (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface)
						      (aref color-key 0)
						      (aref color-key 1)
						      (aref color-key 2))))
    (setf texture (sdl2:create-texture-from-surface renderer surface))
    (sdl2:free-surface surface)
    texture))

(defun set-color (texture &key (r 0) (g 0) (b 0))
  (sdl2:set-texture-color-mod texture r g b))

(defun render (renderer texture &key (clip nil) (x 0) (y 0))
  (let ((w (if clip (sdl2:rect-width clip) (sdl2:texture-width texture)))
	(h (if clip (sdl2:rect-height clip) (sdl2:texture-height texture))))
    
    (sdl2:render-copy renderer texture :source-rect clip :dest-rect (sdl2:make-rect x y w h))))

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-12/assets/")))
	       (title "Tutorial 12: Color Modulation") (width 640) (height 480))
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
	   
	   (let ((texture (load-texture renderer (file-path path "colors.png")))
		 (red 255)
		 (green 255)
		 (blue 255))
	     
	     (sdl2:with-event-loop ()
	       (:quit () t)

	       (:keydown (:keysym keysym)
			 (case (sdl2:scancode keysym)
			   (:scancode-q (incf red 32))
			   (:scancode-w (incf green 32))
			   (:scancode-e (incf blue 32))
			   (:scancode-a (decf red 32))
			   (:scancode-s (decf green 32))
			   (:scancode-d (decf blue 32)))
			 
			 (labels ((ensure-color-value (color)
				    (cond ((> color 255) 255)
					  ((< color 0) 0)
					  (t color))))
			   (setf red (ensure-color-value red)
				 green (ensure-color-value green)
				 blue (ensure-color-value blue))))
	       
	       (:idle ()
		      (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
		      (sdl2:render-clear renderer)

		      (set-color texture :r red :g green :b blue)
		      
		      (render renderer texture)
		      
		      (sdl2:render-present renderer)))
	     (sdl2:destroy-texture texture)
	     (sdl2-image:quit))))))))
