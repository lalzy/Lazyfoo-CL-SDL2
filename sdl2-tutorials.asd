;;;; Lazy-foo.asd

(asdf:defsystem #:sdl2-tutorials
  :description "Common Lisp adaption of Lazy-Foo's SDL2 tutorials"
  :author "Lalzy"
  :serial t
  :depends-on (#:sdl2 #:iterate #:sdl2-image #:sdl2-ttf)
  :components ((:file "tutorial-01/main") ; Hello SDL
	       (:file "tutorial-02/main") ; Getting an Image on the Screen
	       (:file "tutorial-03/main") ; Event Driven Programming
	       (:file "tutorial-04/main") ; Key Presses
	       (:file "tutorial-05/main") ; Optimized Surface Loading and Soft Stretching
	       (:file "tutorial-06/main") ; Extension Libraries and Loading Other Image Formats
	       (:file "tutorial-07/main") ; Texture Loading and Rendering
	       (:file "tutorial-08/main") ; Geometry Rendering
	       (:file "tutorial-09/main") ; The Viewport
	       (:file "tutorial-10/main") ; Color Keying
	       (:file "tutorial-11/main") ; Clip Rendering and Sprite Sheets
	       (:file "tutorial-12/main") ; Color Modulation
	       (:file "tutorial-13/main") ; Alpha Blending
	       (:file "tutorial-14/main") ; Animated Sprites and Vsync
	       (:file "tutorial-15/main") ; Rotation and Flipping
	       (:file "tutorial-16/main") ; True Type Fonts
	       (:file "tutorial-17/main") ; Mouse Events
	       (:file "tutorial-18/main") ; Key States
	       (:file "tutorial-19/main") ; Gamepads and Joysticks
	       (:file "tutorial-20/main") ; Gamepads and Joysticks
	       )) 
