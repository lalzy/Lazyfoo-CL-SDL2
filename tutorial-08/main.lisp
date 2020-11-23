;;;; Tutorial 08: Geometry Rendering

(defpackage #:tutorial-08
  (:use #:cl #:iterate)
  (:export :main))

(in-package #:tutorial-08)

(defun main (&aux (title "Tutorial 08: Geometry Rendering") (width 640) (height 480))
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

	   (let ((rect-fill (sdl2:make-rect (round width 4) (round height 4) (round width 2) (round height 2)))
		 (rect-outline (sdl2:make-rect (round width 6)       (round height 6)
					       (round (* width 2) 3) (round (* height 2) 3))))
	     
	     (sdl2:with-event-loop ()
	       (:quit () t)
	       
	       (:idle ()
		      (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
		      (sdl2:render-clear renderer)

		      (sdl2:set-render-draw-color renderer #xFF #x00 #x00 #xFF)
		      (sdl2:render-fill-rect renderer rect-fill)
		      
		      (sdl2:set-render-draw-color renderer #x00 #xFF #x00 #xFF)
		      (sdl2:render-draw-rect renderer rect-outline)

		      (sdl2:set-render-draw-color renderer #x00 #x00 #xFF #xFF)
		      (sdl2:render-draw-line renderer 0 (round height 2) width (round height 2))

		      (sdl2:set-render-draw-color renderer #xFF #xFF #x00 #xFF)
		      
		      (loop for i from 0 below height by 4 do
			(sdl2:render-draw-point renderer (round width 2) i))

		      (sdl2:render-present renderer)))
	     (sdl2:free-rect rect-fill)
	     (sdl2:free-rect rect-outline)
	     (sdl2-image:quit))))))))
