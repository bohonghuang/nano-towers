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

(defstruct (game-scene-enemy (:include eon:scene3d-container)
                             (:constructor %make-game-scene-enemy))
  (type :slime :type symbol)
  (speed 0.5 :type single-float)
  (path nil)
  (animations nil :type list))

(defgeneric game-scene-enemy-type-asset (type))

(defmethod game-scene-enemy-type-asset ((type symbol))
  (list :model (game-asset (pathname (format nil "models/enemies/~A.glb" (string-downcase (symbol-name type)))))))

(defun make-game-scene-enemy (&rest args &key (type :slime) &allow-other-keys)
  (apply #'%make-game-scene-enemy
         (destructuring-bind (&key model (model-animations model))
             (game-scene-enemy-type-asset type)
           (list* :content (list (eon:load-asset 'raylib:model model))
                  :scale (raylib:vector3-scale (raylib:vector3-one) (/ 3.0))
                  :animations (cobj:ccoerce (eon:load-asset 'raylib:model-animations model-animations) 'list)
                  args))))

(defun game-scene-enemy-model (enemy)
  (first (game-scene-enemy-content enemy)))

(defun (setf game-scene-enemy-model) (value enemy)
  (setf (first (game-scene-enemy-content enemy)) value))

(defun game-scene-enemy-active-animation (enemy)
  (cobj:ccoerce (raylib:model-animation-name (first (game-scene-enemy-animations enemy))) 'string))

(defun (setf game-scene-enemy-active-animation) (name enemy)
  (rotatef
   (first (game-scene-enemy-animations enemy))
   (nth (position
         name (game-scene-enemy-animations enemy)
         :test #'string= :key (compose (rcurry #'cobj:ccoerce 'string) #'raylib:model-animation-name))
        (game-scene-enemy-animations enemy)))
  name)

(declaim (ftype (function (single-float single-float) (values single-float)) absmin)
         (inline absmin))
(defun absmin (a b)
  (if (< (abs a) (abs b)) a b))

(define-modify-macro absminf (value) absmin)

(defun game-scene-enemy-updater (enemy)
  (with-accessors ((path game-scene-enemy-path)) enemy
    (lambda ()
      (when-let ((target (first path)))
        (etypecase target
          (raylib:vector3
           (let ((position (game-scene-enemy-position enemy))
                 (rotation (game-scene-enemy-rotation enemy)))
             (clet* ((offset (raylib:vector3-subtract target position))
                     (direction (raylib:vector3-normalize offset))
                     (delta (raylib:vector3-scale direction (* (game-scene-enemy-speed enemy) (eon:game-loop-delta-time)))))
               (declare (dynamic-extent offset direction delta))
               (absminf (raylib:vector3-x delta) (raylib:vector3-x offset))
               (absminf (raylib:vector3-y delta) (raylib:vector3-y offset))
               (absminf (raylib:vector3-z delta) (raylib:vector3-z offset))
               (raylib:%vector3-add (& position) (& position) (& delta))
               (if (< (abs (+ (raylib:vector3-dot-product direction eon::+vector3-unit-z+) 1.0)) single-float-epsilon)
                   (raylib:%quaternion-from-axis-angle (& rotation) (& eon::+vector3-unit-y+) (eon::degree-radian 180.0))
                   (raylib:%quaternion-from-vector3-to-vector3
                    (& rotation)
                    (& eon::+vector3-unit-z+)
                    (& direction)))
               (when (< (raylib:vector3-distance position target) single-float-epsilon)
                 (pop path)))))
          (function (funcall (pop path)))))
      path)))

(declaim (ftype (function (non-negative-fixnum &optional single-float) (values non-negative-fixnum)) game-loop-counter))
(defun game-loop-counter (max &optional (speed 60.0))
  (values (floor (mod (* (eon:game-loop-time) (coerce speed 'double-float)) (coerce max 'double-float)))))

(defmethod eon:scene3d-draw ((enemy game-scene-enemy) position origin scale rotation tint)
  (let ((animation (first (game-scene-enemy-animations enemy))))
    (raylib:update-model-animation
     (game-scene-enemy-model enemy) animation
     (game-loop-counter (raylib:model-animation-frame-count animation))))
  (call-next-method))

(defun game-scene-map-enemy-paths (map)
  (mapcar
   (lambda (object)
     (mapcar
      (lambda (point)
        (raylib:vector3-add
         (raylib:make-vector3
          :x (/ (coerce (tiled:object-x object) 'single-float) (coerce +tile-width+ 'single-float))
          :y 0.0
          :z (/ (coerce (tiled:object-y object) 'single-float) (coerce +tile-height+ 'single-float)))
         (raylib:make-vector3
          :x (/ (coerce (car point) 'single-float) (coerce +tile-width+ 'single-float))
          :y 0.0
          :z (/ (coerce (cdr point) 'single-float) (coerce +tile-height+ 'single-float)))))
      (tiled:polyline-points object)))
   (tiled:object-group-objects
    (find "enemy" (tiled:map-layers map) :key #'tiled:layer-name :test #'string=))))

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
        (let ((path (random-elt (game-scene-map-enemy-paths map))))
          (eon:add-game-loop-hook
           (lambda ()
             (push
              (let ((enemy (make-game-scene-enemy
                            :position (first path)
                            :type :dragon
                            :path (rest path))))
                (eon:add-game-loop-hook (game-scene-enemy-updater enemy) :after #'identity)
                (setf (game-scene-enemy-active-animation enemy) "Dragon_Flying")
                enemy)
              (game-scene-screen-enemies screen)))
           :after nil))
        (setf (eon:current-screen) screen))
      (eon:do-screen-loop (eon:make-fit-viewport :width +viewport-width+ :height +viewport-height+)))))
