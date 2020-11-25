;;;; Tutorial 20: Gamepads and Joysticks

(defpackage #:tutorial-20
  (:use #:cl)
  (:export :main))


(in-package #:tutorial-20)

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

(defun load-from-rendered-text (renderer font Texture-text &key (R #xFF) (G #xFF) (B #xFF) (A #xFF))
  (let* ((surface (sdl2-ttf:render-text-solid font texture-text r g b a))
	 (texture (sdl2:create-texture-from-surface renderer surface)))
    (sdl2:free-surface surface)
    texture))

(defun render (renderer texture &key (clip nil) (x 0) (y 0)  (angle 0) (center nil) (flip :none))
  (let ((w (if clip (sdl2:rect-width clip) (sdl2:texture-width texture)))
	(h (if clip (sdl2:rect-height clip) (sdl2:texture-height texture))))
    
    (sdl2:render-copy-ex renderer texture :source-rect clip :dest-rect (sdl2:make-rect x y w h)
					  :angle angle :center center :flip (list flip)
					  )
    ))

(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-20/assets/")))
	       (title "Tutorial 20: Gamepads and Joysticks") (width 640) (height 480))
  ;; Prevent slime from hanging if crashing
  (sdl2:make-this-thread-main
   (lambda ()
     (sdl2:with-init (:video :joystick :haptic)
       (sdl2:with-window (window :title title :w width :h height :flags '(:shown))
	 (sdl2:with-renderer (renderer window :flags '(:accelerated :presentvsync))
	   ;; Emacs on Windows doesn't like displaying the window
	   (sdl2:hide-window window)
	   (sdl2:show-window window)
	   ;; Window loads behind emacs on Windows.
	   (sdl2:raise-window window)
	   
	   (sdl2-image:init '(:png))
	   (sdl2-ttf:init)
	   
	   
	   (let* ((texture (load-texture renderer (file-path path "splash.png")))
		  (dead-zone 8000)
		  controller
		  haptic
		  (xdir 0)
		  (ydir 0))
	     
	     ;; Slime doesn't like recompiling with ttf even though we close it. Unwind-protect fixes this
	     (unwind-protect
		  (progn

		    (when (> (sdl2:joystick-count) 0)
		      (format t "opening controller: ~a~%" (sdl2:joystick-name-for-index 0))
		      (format t "joystick-count = ~a~%" (sdl2:joystick-count))
		      (setf controller (sdl2:joystick-open 0))
		      (setf haptic (sdl2:haptic-open-from-joystick controller))
		      (sdl2:rumble-init haptic)
		      
		      
		      (format t "controller = ~a~%" controller))

		    (sdl2:with-event-loop ()
		      (:quit () t)

		      (:joybuttondown
		       (:which which)
		       (when (= which (sdl2:joystick-instance-id controller))
			 (sdl2:rumble-play haptic .75 500)))

		      (:idle ()
			     (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
			     (sdl2:render-clear renderer)

			     
			     (render renderer texture)

			     
			     (sdl2:render-present renderer))))

	       (format t "closing~%")
	       (sdl2:haptic-close haptic)
	       (sdl2:joystick-close controller)
	       (sdl2:destroy-texture texture)
	       (sdl2-ttf:quit)
	       (sdl2-image:quit)
	       (sdl2:quit)))))))))
