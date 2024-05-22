(in-package #:spring-lisp-game-jam-2024)

(defun main ()
  (raylib:set-config-flags (cffi:foreign-bitfield-value 'raylib:config-flags '(:window-resizable :vsync-hint)))
  (raylib:with-window ("Spring Lisp Game Jam 2024" (+viewport-width+ +viewport-height+))
    (raylib:set-target-fps 60)
    (eon:with-game-context
      (game)
      (eon:do-screen-loop (eon:make-fit-viewport :width +viewport-width+ :height +viewport-height+)))))
