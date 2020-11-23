;;;; Tutorial 13: Alpha Blending

(defpackage #:tutorial-13
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-13)

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

(defun set-blending-mode (texture blend-mode)
  (sdl2:set-texture-blend-mode texture blend-mode))

(defun set-alpha (texture alpha)
  (sdl2:set-texture-alpha-mod texture alpha))


(defun render (renderer texture &key (clip nil) (x 0) (y 0))
  (let ((w (if clip (sdl2:rect-width clip) (sdl2:texture-width texture)))
	(h (if clip (sdl2:rect-height clip) (sdl2:texture-height texture))))
    
    (sdl2:render-copy renderer texture :source-rect clip :dest-rect (sdl2:make-rect x y w h))))

(defun within-byte-size (num)
  (max 0 (min 255 num)))

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-13/assets/")))
	       (title "Tutorial 13: Alpha Blending") (width 640) (height 480))
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
	   
	   (let ((modulate (load-texture renderer (file-path path "fadeout.png")))
		 (background (load-texture renderer (file-path path "fadein.png")))
		 (alpha 255))

	     (set-blending-mode modulate :blend)
	     
	     (sdl2:with-event-loop ()
	       (:quit () t)

	       (:keydown (:keysym keysym)
			 (setf alpha
			       (case (sdl2:scancode keysym)
				 (:scancode-w
				  (within-byte-size (+ alpha 32)))
				 (:scancode-s
				  (within-byte-size (- alpha 32)))
				 (t alpha))))
	       
	       (:idle ()
		      (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
		      (sdl2:render-clear renderer)

		      
		      (render renderer background)
		      (set-alpha modulate alpha)
		      (render renderer modulate)
		      
		      (sdl2:render-present renderer)))
	     (sdl2:destroy-texture modulate)
	     (sdl2:destroy-texture background)
	     (sdl2-image:quit))))))))
