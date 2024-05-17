(defpackage spring-lisp-game-jam-2024
  (:use #:cl #:cffi #:cffi-ops #:alexandria #:promise-async-await))

(in-package #:spring-lisp-game-jam-2024)

(defun game-asset (pathname)
  (merge-pathnames pathname #.(merge-pathnames #P"assets/" (asdf:component-pathname (asdf:find-system '#:spring-lisp-game-jam-2024)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun camera-3d-parameters-form (form)
    `((raylib:vector3-x (raylib:camera-3d-position ,form))
      (raylib:vector3-y (raylib:camera-3d-position ,form))
      (raylib:vector3-z (raylib:camera-3d-position ,form))
      (raylib:vector3-x (raylib:camera-3d-target ,form))
      (raylib:vector3-y (raylib:camera-3d-target ,form))
      (raylib:vector3-z (raylib:camera-3d-target ,form))
      (raylib:vector3-x (raylib:camera-3d-up ,form))
      (raylib:vector3-y (raylib:camera-3d-up ,form))
      (raylib:vector3-z (raylib:camera-3d-up ,form))))
  (defun parameters-camera-3d-form (params)
    (destructuring-bind (px py pz tx ty tz ux uy uz) params
      `(raylib:make-camera-3d :position (raylib:make-vector3 :x ,px :y ,py :z ,pz)
                              :target (raylib:make-vector3 :x ,tx :y ,ty :z ,tz)
                              :up (raylib:make-vector3 :x ,ux :y ,uy :z ,uz)
                              :fovy +camera-default-fovy+
                              :projection #.(foreign-enum-value 'raylib:camera-projection :perspective)))))

(cobj:define-global-cobject +camera-default+ (raylib:make-camera-3d
                                              :position (raylib:make-vector3 :x 0.0 :y 10.0 :z 5.0)
                                              :target (raylib:vector3-zero)
                                              :up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0)
                                              :fovy (/ 0.03) :projection #.(cffi:foreign-enum-value 'raylib:camera-projection :perspective)))

(defun game-scene-camera-look-at (camera target)
  (let ((tween (ute:start (ute:tween :from (#.(camera-3d-parameters-form 'camera) #.(camera-3d-parameters-form 'camera)) :duration 0.2))))
    (let ((offset (raylib:vector3-subtract target (raylib:camera-target camera))))
      (setf (raylib:camera-target camera) target
            (raylib:camera-position camera) (raylib:vector3-add (raylib:camera-position camera) offset)))
    (ute::base-tween-update tween single-float-epsilon)))

(defstruct game-scene-screen
  (camera (raylib:copy-camera +camera-default+) :type raylib:camera-3d)
  (map-renderer (eon:tiled-map-renderer (tiled:load-map (game-asset #P"maps/map-1.tmx"))) :type eon:tiled-renderer)
  (base-focus-manager (eon:make-scene2d-focus-manager) :type eon:scene2d-focus-manager)
  (towers nil :type list)
  (enemies nil :type list)
  (attacks nil :type list))

(defconstant +viewport-width+ 640)
(defconstant +viewport-height+ 384)

(defconstant +tile-width+ 400)
(defconstant +tile-height+ 400)

(defmethod eon:screen-render ((screen game-scene-screen))
  (raylib:clear-background raylib:+white+)
  (raylib:with-mode-3d (game-scene-screen-camera screen)
    (raylib:draw-grid 100 1.0)
    (raylib:draw-line-3d eon::+vector3-zeros+ eon::+vector3-unit-x+ raylib:+red+)
    (raylib:draw-line-3d eon::+vector3-zeros+ eon::+vector3-unit-y+ raylib:+green+)
    (raylib:draw-line-3d eon::+vector3-zeros+ eon::+vector3-unit-z+ raylib:+blue+)
    (rlgl:push-matrix)
    (rlgl:rotatef 90.0 1.0 0.0 0.0)
    (rlgl:scalef (/ (coerce +tile-width+ 'single-float)) (/ (coerce +tile-height+ 'single-float)) 0.0)
    (funcall (game-scene-screen-map-renderer screen))
    (rlgl:pop-matrix)
    (eon:scene3d-draw-simple (game-scene-screen-towers screen))
    (eon:scene3d-draw-simple (game-scene-screen-enemies screen))
    (eon:scene3d-draw-simple (game-scene-screen-attacks screen))))

(defstruct (game-scene-tower (:include eon:scene3d-container)
                             (:constructor %make-game-scene-tower))
  (selectedp nil :type boolean)
  (type :default :type symbol))

(defun make-game-scene-tower (&rest args &key (model nil) &allow-other-keys)
  (apply #'%make-game-scene-tower :content (list model) (remove-from-plist args :model)))

(defun game-scene-tower-model (tower)
  (first (game-scene-tower-content tower)))

(defun (setf game-scene-tower-model) (value tower)
  (setf (first (game-scene-tower-content tower)) value))

(defmethod eon:scene3d-draw ((tower game-scene-tower) position origin scale rotation tint)
  (when (game-scene-tower-selectedp tower)
    (raylib:draw-cube position 1.0 0.25 1.0 (raylib:fade raylib:+white+ 0.5)))
  (call-next-method))

(defun main ()
  (raylib:set-config-flags (cffi:foreign-bitfield-value 'raylib:config-flags '(:window-resizable)))
  (raylib:with-window ("Spring Lisp Game Jam 2024" ((* +viewport-width+ 2) (* +viewport-height+ 2)))
    (raylib:set-target-fps 60)
    (eon:with-game-context
      (let* ((map (tiled:load-map (game-asset #P"maps/map-1.tmx")))
             (screen (make-game-scene-screen :map-renderer (eon:tiled-map-renderer map))))
        (loop :with group := (eon:scene2d-construct (eon:scene2d-group))
              :for cell :in (tiled:layer-cells (find "ground" (tiled:map-layers map) :key #'tiled:layer-name :test #'string=))
              :when (gethash "base" (tiled:properties (tiled:cell-tile cell)))
                :do (push (make-game-scene-tower 
                           :position (raylib:make-vector3
                                      :x (+ (coerce (tiled:cell-column cell) 'single-float) 0.5)
                                      :y 0.0
                                      :z (+ (coerce (tiled:cell-row cell) 'single-float) 0.5)))
                          (game-scene-screen-towers screen))
                :and :collect (eon::make-scene2d-focusable
                               :focus-point (raylib:make-vector2
                                             :x (coerce (tiled:cell-column cell) 'single-float)
                                             :y (coerce (tiled:cell-row cell) 'single-float))
                               :content (first (game-scene-screen-towers screen)))
                       :into focusables
              :finally (setf (game-scene-screen-base-focus-manager screen) (eon:make-scene2d-focus-manager :focusables focusables)))
        (flet ((unselect-tower (tower)
                 (setf (game-scene-tower-selectedp tower) nil))
               (select-tower (tower)
                 (setf (game-scene-tower-selectedp tower) t)
                 (game-scene-camera-look-at (game-scene-screen-camera screen) (game-scene-tower-position tower))))
          (select-tower (lastcar (game-scene-screen-towers screen)))
          (async
            (loop :with focus-manager := (game-scene-screen-base-focus-manager screen)
                  :for key := (await (eon:promise-pressed-key))
                  :do (case key
                        ((:left :right :up :down)
                         (unselect-tower (eon::scene2d-focusable-content (eon:scene2d-focus-manager-focused focus-manager)))
                         (eon:scene2d-focus-manager-handle-key focus-manager key)
                         (select-tower (eon::scene2d-focusable-content (eon:scene2d-focus-manager-focused focus-manager))))
                        (:b (return))))))
        (setf (eon:current-screen) screen))
      (eon:do-screen-loop (eon:make-fit-viewport :width +viewport-width+ :height +viewport-height+)))))
