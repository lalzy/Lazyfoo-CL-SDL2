;;;; Tutorial 15: Rotation and Flipping

(defpackage #:tutorial-15
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-15)

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


(defun render (renderer texture &key (clip nil) (x 0) (y 0)  (angle 0) (center nil) (flip nil))
  (let ((w (if clip (sdl2:rect-width clip) (sdl2:texture-width texture)))
	(h (if clip (sdl2:rect-height clip) (sdl2:texture-height texture))))
    
    (sdl2:render-copy-ex renderer texture :source-rect clip :dest-rect (sdl2:make-rect x y w h)
					  :angle angle :center center :flip (list flip))))

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-15/assets/")))
	       (title "Tutorial 15: Rotation and Flipping") (width 640) (height 480))
  ;; Prevent slime from hanging if crashing
  (sdl2:make-this-thread-main
   (lambda ()
     (sdl2:with-init (:video)
       (sdl2:with-window (window :title title :w width :h height :flags '(:shown))
	 (sdl2:with-renderer (renderer window :flags '(:accelerated :presentvsync))
	   ;; Emacs on Windows doesn't like displaying the window
	   (sdl2:hide-window window)
	   (sdl2:show-window window)
	   ;; Window loads behind emacs on Windows.
	   (sdl2:raise-window window)
	   (sdl2-image:init '(:png))
	   
	   (let* ((sprite (load-texture renderer (file-path path "arrow.png") :color-key #(0 255 255)))
		  (degrees 0)
		  (flip-type :none))

	     (sdl2:with-event-loop ()
	       (:quit () t)

	       (:keydown (:keysym keysym)
			 (case (sdl2:scancode keysym)
			   (:scancode-a (decf degrees 60))
			   (:scancode-d (incf degrees 60))
			   (:scancode-q (setf flip-type :horizontal))
			   (:scancode-w (setf flip-type :none))
			   (:scancode-e (setf flip-type :vertical))))
	       
	       (:idle ()
		      (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
		      (sdl2:render-clear renderer)

		      (render renderer sprite :x (round (- width (sdl2:texture-width sprite)) 2)

					      :y (round (- height (sdl2:texture-height sprite)) 2)

					      :angle degrees :flip flip-type)

		      
			  
		      (sdl2:render-present renderer)))
	     (sdl2:destroy-texture sprite)
	     (sdl2-image:quit))))))))
