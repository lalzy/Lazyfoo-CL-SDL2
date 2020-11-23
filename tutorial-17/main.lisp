;;;; Tutorial 17: Mouse Events

(defpackage #:tutorial-17
  (:use #:cl)
  (:export :main))


(in-package #:tutorial-17)

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

;; Using a struct instead of a class to represent the button class in Lazy Foo's tutorial.
(defstruct button
  (x 0) (y 0) (w 300) (h 200) (state :outside))

(defun get-clip (button)
  "Choose the correct part of the button.png texture based on the state"
  (let ((w (button-w button))
	(h (button-h button)))
    
    (case (button-state button)
      (:outside (sdl2:make-rect 0 (* 0 h) w h))
      (:inside (sdl2:make-rect 0 (* 1 h) w h))
      (:down (sdl2:make-rect 0 (* 2 h) w h))
      (:up (sdl2:make-rect 0 (* 3 h) w h)))))

(defun render-buttons (renderer texture buttons)
  "Draw all the button sprites"
  (dolist (button buttons)
    (render renderer texture
	    :x (button-x button) :y (button-y button)
	    
	    :clip (get-clip button))))

(defun inside-button-collision (button mouse-x mouse-y)
  "Collision check"
  (and (and (> mouse-x (button-x button)) (< mouse-x (+ (button-w button) (button-x button))))
       (and (> mouse-y (button-y button)) (< mouse-y (+ (button-h button) (button-y button))))))

(defun change-click-state (buttons opposite wanted)
  "Changes the button-sprite's state based on if mouse is pressed, or released."
  (dolist (button buttons)
    (let ((state (button-state button)))
      (when (or (string-equal state opposite) (string-equal state :inside))
	(setf (button-state button) wanted)))))


(defun main (&aux (path (namestring (asdf:system-relative-pathname :sdl2-tutorials "tutorial-17/assets/")))
	       (title "Tutorial 17: Mouse Events") (width 640) (height 480))
  "Using events rather than states as it's easier and cleaner with CL-SDL2(Especially for the mouse)"
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
	   (sdl2-ttf:init)
	   
	   (let* ((texture (load-texture renderer (file-path path "button.png")))
		  (button-width 300)
		  (button-height 200)
		  (buttons (list (make-button)
				 (make-button :X (- width button-width) :Y 0)
				 (make-button :x 0 :y (- height button-height))
				 (make-button :x (- width button-width) :y (- height button-height)))))

	     ;; Slime doesn't like recompiling with ttf even though we close it. Unwind-protect fixes this
	     (unwind-protect
		  (progn

		    (sdl2:with-event-loop ()
		      (:quit () t)

		      (:mousemotion (:x x :y y :state mouse-state)
				    (dolist (button buttons)
				      (setf (button-state button)
					    (if (inside-button-collision button x y)
						(if (> mouse-state 0)
						    :down
						    :inside)
						:outside))))
		      
		      (:mousebuttonup (:button mouse-button)
				      (change-click-state buttons :down :up))
		      
		      (:mousebuttondown (:button mouse-button)
					(change-click-state buttons :up :down))
		      
		      (:idle ()
			     (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
			     (sdl2:render-clear renderer)


			     (render-buttons renderer texture buttons)

			     (sdl2:render-present renderer))))
	       
	       (format t "closing~%")
	       (sdl2:destroy-texture texture)
	       (sdl2-ttf:quit)
	       (sdl2-image:quit)))))))))
