(defpackage spring-lisp-game-jam-2024
  (:use #:cl #:cffi #:cffi-ops #:alexandria #:promise-async-await))

(in-package #:spring-lisp-game-jam-2024)

(defun game-asset (pathname)
  (merge-pathnames pathname #.(merge-pathnames #P"assets/" (asdf:component-pathname (asdf:find-system '#:spring-lisp-game-jam-2024)))))

(defmethod eon:scene3d-draw :around ((emitter eon:scene3d-particle-emitter) position origin scale rotation tint)
  (rlgl:disable-depth-mask)
  (call-next-method)
  (rlgl:enable-depth-mask)
  (rlgl:draw-render-batch-active))

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

(declaim (inline position-2d->3d))
(defun position-2d->3d (position)
  (raylib:make-vector3
   :x (raylib:vector2-x position) :y 0.0
   :z (raylib:vector2-y position)))

(declaim (inline position-3d->2d))
(defun position-3d->2d (position)
  (raylib:make-vector2
   :x (raylib:vector3-x position)
   :y (raylib:vector3-z position)))

(cobj:define-global-cobject +camera-default+ (raylib:make-camera-3d
                                              :position (raylib:make-vector3 :x 0.0 :y 10.0 :z 5.0)
                                              :target (raylib:vector3-zero)
                                              :up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0)
                                              :fovy (/ 0.03) :projection #.(cffi:foreign-enum-value 'raylib:camera-projection :perspective)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +viewport-width+ 640)
  (defconstant +viewport-height+ 384))

(defconstant +tile-width+ 400)
(defconstant +tile-height+ 400)

(eon:define-scene2d-constructed game-scene-ui
    (eon:scene2d-group
     :name group
     :children ((eon:scene2d-cell
                 :size (#.(float +viewport-width+) #.(float +viewport-height+))
                 :alignment (:end :start)
                 :child (eon:scene2d-window
                         :child (eon:scene2d-margin
                                 :top 2.0 :bottom 2.0 :left 2.0 :right 2.0
                                 :child (eon:scene2d-box
                                         :orientation :horizontal
                                         :children ((eon:scene2d-label :string "$ ")
                                                    (eon:scene2d-label :string "0" :name label-money)))))))))

(defstruct (game-scene-screen (:constructor %make-game-scene-screen))
  (camera (raylib:copy-camera +camera-default+) :type raylib:camera-3d)
  (light-camera (raylib:make-camera) :type raylib:camera-3d)
  (shadow (eon:make-shadow-map-renderer) :type eon:shadow-map-renderer)
  (shader (eon:load-asset 'raylib:shader (game-asset #P"scene")) :type raylib:shader)
  (shader-uniforms (make-game-scene-screen-shader-uniforms) :type cobj:cobject)
  (map-renderer (eon:tiled-map-renderer (tiled:load-map (game-asset #P"maps/map-1.tmx"))) :type eon:tiled-renderer)
  (tower-base-focus-manager (eon:make-scene2d-focus-manager) :type eon:scene2d-focus-manager)
  (ui (make-game-scene-ui) :type eon:scene2d-constructed)
  (-money 0 :type non-negative-fixnum)
  (towers nil :type list)
  (enemies nil :type list)
  (objects nil :type list))

(defvar *game-scene-screen*)

(defvar *game-scene-shader-uniforms-shadow-map*)

(cobj:define-global-cobject +light-position-default+ (raylib:make-vector3 :x -1.0 :y 4.0 :z 2.0))

(eon:define-shaderable-uniforms game-scene-screen
  ("colDiffuse" raylib:+white+ :type raylib:color)
  ("colEmission" raylib:+black+ :type raylib:color)
  ("colAmbient" (raylib:color-brightness raylib:+white+ -0.5) :type raylib:color)
  ("colSpecular" raylib:+black+ :type raylib:color)
  ("colLight" raylib:+lightgray+ :type raylib:color)
  ("lightVector" (raylib:vector3-negate (raylib:vector3-normalize +light-position-default+)) :type raylib:vector3)
  ("shadowLightMatrix" (raylib:make-matrix) :type raylib:matrix)
  ("shadowIntensity" (/ 3.0) :type single-float)
  ("shadowMap" *game-scene-shader-uniforms-shadow-map* :type raylib:texture))

(cobj:define-global-cobject +shadow-light-camera-default+
    (raylib:make-camera-3d :position +light-position-default+
                           :target (raylib:make-vector3 :x 0.0 :y 0.0 :z 0.0)
                           :up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0) :fovy 10.0
                           :projection (cffi:foreign-enum-value 'raylib:camera-projection :orthographic)))

(defun make-game-scene-screen (&rest args)
  (let* ((light-camera (raylib:copy-camera +shadow-light-camera-default+))
         (shadow (eon:make-shadow-map-renderer :camera light-camera :size (raylib:make-vector2 :x 1024.0 :y 512.0)))
         (screen (let ((*game-scene-shader-uniforms-shadow-map*
                         (eon:shadow-map-renderer-texture shadow)))
                   (apply #'%make-game-scene-screen :shadow shadow :light-camera light-camera args))))
    (initialize-game-scene-screen-shader-uniforms screen)
    (setf (eon:shadow-map-renderer-matrix shadow) (game-scene-screen-shadow-light-matrix screen))
    screen))

(defun game-scene-screen-look-at (screen target)
  (let ((camera (game-scene-screen-camera screen))
        (light-camera (game-scene-screen-light-camera screen)))
    (let ((tween (ute:start
                  (ute:timeline
                   (:parallel
                    (:from (#.(camera-3d-parameters-form 'camera) #.(camera-3d-parameters-form 'camera)) :duration 0.2)
                    (:from (#.(camera-3d-parameters-form 'light-camera) #.(camera-3d-parameters-form 'light-camera)) :duration 0.2))))))
      (let ((offset (raylib:vector3-subtract target (raylib:camera-target camera))))
        (setf (raylib:camera-target camera) target
              (raylib:camera-position camera) (raylib:vector3-add (raylib:camera-position camera) offset)
              (raylib:camera-target light-camera) target
              (raylib:camera-position light-camera) (raylib:vector3-add (raylib:camera-position light-camera) offset)))
      (ute::base-tween-update tween single-float-epsilon))))

(defun game-scene-screen-money (screen)
  (game-scene-screen--money screen))

(defun (setf game-scene-screen-money) (value screen)
  (setf (eon:scene2d-label-string (game-scene-ui-label-money (game-scene-screen-ui screen))) (princ-to-string value))
  (eon:scene2d-layout (game-scene-screen-ui screen))
  (setf (game-scene-screen--money screen) value))

(defmethod eon:screen-render ((screen game-scene-screen))
  (mapc (rcurry #'game-scene-tower-try-attack (game-scene-screen-enemies screen)) (game-scene-screen-towers screen))
  (raylib:clear-background raylib:+white+)
  (flet ((render-objects ()
           (let ((eon:*scene3d-camera* (game-scene-screen-camera screen)))
             (eon:scene3d-draw-simple (game-scene-screen-towers screen))
             (eon:scene3d-draw-simple (game-scene-screen-enemies screen))
             (rlgl:normal3f 0.0 1.0 0.0)
             (eon:scene3d-draw-simple (game-scene-screen-objects screen)))))
    (eon:shadow-map-renderer-render (game-scene-screen-shadow screen) #'render-objects)
    (raylib:with-mode-3d (game-scene-screen-camera screen)
      (raylib:with-shader-mode (game-scene-screen-shader screen)
        (raylib:copy-color raylib:+white+ (game-scene-screen-col-diffuse screen))
        (update-game-scene-screen-shader-uniforms screen)
        (rlgl:push-matrix)
        (rlgl:rotatef 90.0 1.0 0.0 0.0)
        (rlgl:scalef (/ (coerce +tile-width+ 'single-float)) (/ (coerce +tile-height+ 'single-float)) single-float-epsilon)
        (funcall (game-scene-screen-map-renderer screen))
        (rlgl:pop-matrix)
        (rlgl:draw-render-batch-active)
        (let ((shadow-intensity (game-scene-screen-shadow-intensity screen)))
          (setf (game-scene-screen-shadow-intensity screen) 0.0)
          (update-game-scene-screen-shader-uniforms screen)
          (setf (game-scene-screen-shadow-intensity screen) shadow-intensity))
        (render-objects))))
  (eon:scene2d-draw-simple (game-scene-screen-ui screen)))

(defstruct (game-scene-tower (:include eon:scene3d-container)
                             (:constructor %make-game-scene-tower))
  (shader nil :type raylib:shader)
  (selectedp nil :type boolean)
  (type nil :type symbol)
  (level 0 :type (integer 0 3))
  (target nil :type t)
  (charging 0.0 :type single-float)
  (projectile nil :type (or list eon:scene3d-particle-emitter)))

(defparameter *tower-types*
  '((:round-1 :cost ((1 . 1000) (2 . 2000) (3 . 4000))
              :rate t
              :radius ((1 . 2.0) (2 . 3.0) (3 . 4.0))
              :power ((1 . 10) (2 . 15) (3 . 20))
              :model ((1 . "towerRound_sampleA")
                      (2 . "towerRound_sampleC")
                      (3 . "towerRound_sampleE")))
    (:round-2 :cost ((1 . 2000) (2 . 4000) (3 . 8000))
              :rate t
              :radius ((1 . 3.0) (2 . 4.0) (3 . 5.0))
              :power ((1 . 15) (2 . 25) (3 . 35))
              :model ((1 . "towerRound_sampleB")
                      (2 . "towerRound_sampleD")
                      (3 . "towerRound_sampleF")))
    (:square-1 :cost ((1 . 800) (2 . 1600) (3 . 3200))
               :rate 1.0
               :radius ((1 . 2.0) (2 . 3.0) (3 . 4.0))
               :power ((1 . 20) (2 . 30) (3 . 40))
               :model ((1 . "towerSquare_sampleF")
                       (2 . "towerSquare_sampleD")
                       (3 . "towerSquare_sampleE")))
    (:square-2 :cost ((1 . 1600) (2 . 3200) (3 . 6400))
               :rate 1.5
               :radius ((1 . 2.0) (2 . 3.0) (3 . 4.0))
               :power ((1 . 40) (2 . 60) (3 . 80))
               :model ((1 . "towerSquare_sampleC")
                       (2 . "towerSquare_sampleA")
                       (3 . "towerSquare_sampleB")))))

(defun game-scene-tower-type-level-asset (type level)
  (when-let ((model-name (assoc-value (getf (assoc-value *tower-types* type) :model) level)))
    (list :model (game-asset (format nil "models/towers/~A.glb" model-name)))))

(defun make-game-scene-tower (&rest args &key (model nil) &allow-other-keys)
  (apply #'%make-game-scene-tower :content (list model) (remove-from-plist args :model)))

(defun game-scene-tower-model (tower)
  (first (game-scene-tower-content tower)))

(defun (setf game-scene-tower-model) (value tower)
  (setf (first (game-scene-tower-content tower)) value))

(declaim (ftype (function (single-float single-float) (values single-float)) absmin)
         (inline absmin))
(defun absmin (a b)
  (if (< (abs a) (abs b)) a b))

(define-modify-macro absminf (value) absmin)

(defun update-position-toward-target (position target &optional (speed 1.0))
  (clet* ((offset (raylib:vector3-subtract target position))
          (delta (raylib:vector3-scale (raylib:vector3-normalize offset) (* speed (eon:game-loop-delta-time)))))
    (declare (dynamic-extent offset delta))
    (absminf (raylib:vector3-x delta) (raylib:vector3-x offset))
    (absminf (raylib:vector3-y delta) (raylib:vector3-y offset))
    (absminf (raylib:vector3-z delta) (raylib:vector3-z offset))
    (raylib:%vector3-add (& position) (& position) (& delta))))

(defun game-scene-tower-attack-rate (tower)
  (getf (assoc-value *tower-types* (game-scene-tower-type tower)) :rate))

(defun game-scene-tower-attack-radius (tower)
  (assoc-value (getf (assoc-value *tower-types* (game-scene-tower-type tower)) :radius) (game-scene-tower-level tower)))

(defun game-scene-tower-attack-power (tower)
  (assoc-value (getf (assoc-value *tower-types* (game-scene-tower-type tower)) :power) (game-scene-tower-level tower)))

(defun apply-model-shader (model shader)
  (dolist (material (cobj:ccoerce (cobj:cpointer-carray (raylib:model-materials model) (raylib:model-material-count model)) 'list))
    (setf (raylib:material-shader material) shader)))

(defun game-scene-tower-update-model (tower)
  (setf (game-scene-tower-model tower)
        (destructuring-bind (&key model)
            (game-scene-tower-type-level-asset
             (game-scene-tower-type tower)
             (game-scene-tower-level tower))
          (when model
            (let ((model (eon:load-asset 'raylib:model model)))
              (apply-model-shader model (game-scene-tower-shader tower))
              model)))))

(defun game-scene-tower-update (tower)
  (game-scene-tower-update-model tower)
  (game-scene-tower-update-projectile tower))

(defmethod eon:scene3d-draw ((tower game-scene-tower) position origin scale rotation tint)
  (when (game-scene-tower-selectedp tower)
    (raylib:draw-cube position 1.0 0.25 1.0 (raylib:fade raylib:+white+ 0.5)))
  (call-next-method)
  (eon:scene3d-draw-simple (game-scene-tower-projectile tower)))

(defstruct (game-scene-enemy (:include eon:scene3d-container)
                             (:constructor %make-game-scene-enemy))
  (type :slime :type symbol)
  (speed 0.5 :type single-float)
  (path nil)
  (animations nil :type list)
  (hp 100.0 :type single-float)
  (total-hp 100.0 :type single-float)
  (hp-bar nil :type (or eon:scene3d-node null))
  (blood-emitter (eon:make-scene3d-particle-emitter
                  :rate 0.0
                  :capacity 512
                  :updater (lambda (position)
                             (let ((velocity (raylib:make-vector3))
                                   (acceleration (raylib:make-vector3 :x 0.0 :y -10.0 :z 0.0)))
                               (lambda (particle)
                                 (if (zerop (eon:particle-3d-age particle))
                                     (progn
                                       (eon:particle-3d-initialize-default particle position)
                                       (setf (raylib:vector3-x velocity) (1- (random 2.0))
                                             (raylib:vector3-y velocity) (1- (random 2.0))
                                             (raylib:vector3-z velocity) (1- (random 2.0)))
                                       (raylib:%vector3-normalize (& velocity) (& velocity))
                                       (setf (raylib:vector3-y velocity) 5.0)
                                       (setf (eon:particle-3d-velocity particle) velocity
                                             (eon:particle-3d-acceleration particle) acceleration))
                                     (eon:particle-3d-update-motion particle)))))
                  :renderer (eon:particle-3d-cube-renderer 0.05 (eon:particle-3d-interpolate-color-over-age raylib:+red+ (raylib:fade raylib:+red+ 0.0) #'ute:circ-in)))
   :type eon:scene3d-particle-emitter)
  (animation-tween (ute:tween :to ()) :type ute:tween)
  (level 1 :type positive-fixnum))

(defstruct (game-scene-enemy-hp-bar (:include eon:scene2d-layout)
                                    (:constructor %make-game-scene-enemy-hp-bar))
  (target nil :type game-scene-enemy))

(defmethod eon:scene2d-draw ((bar game-scene-enemy-hp-bar) position origin scale rotation tint)
  (let* ((target (game-scene-enemy-hp-bar-target bar))
         (ratio (/ (game-scene-enemy-hp target) (game-scene-enemy-total-hp target))))
    (clet ((size (raylib:vector2-scale (game-scene-enemy-hp-bar-size bar) ratio)))
      (declare (dynamic-extent size))
      (setf (raylib:vector2-y size) (raylib:vector2-y (game-scene-enemy-hp-bar-size bar)))
      (raylib:draw-rectangle-v position (game-scene-enemy-hp-bar-size bar) raylib:+gray+)
      (raylib:draw-rectangle-v position size (cond
                                               ((< ratio 0.15) raylib:+red+)
                                               ((< ratio 0.5) raylib:+gold+)
                                               (t raylib:+green+))))))

(defun make-game-scene-enemy-hp-bar (&key target)
  (%make-game-scene-enemy-hp-bar :target target :size (raylib:make-vector2 :x 96.0 :y 12.0)))

(defparameter *enemy-types* '((:dragon :animation (:idle "Dragon_Flying" :dead "Dragon_Death") :base-hp 150.0 :speed 1.0 :base-bounty 5000)
                              (:slime :animation (:idle "Slime_Walk" :dead "Slime_Death") :base-hp 50.0 :speed 0.5 :base-bounty 750)
                              (:bat :animation (:idle "Bat_Flying" :dead "Bat_Death") :base-hp 25.0 :speed 1.5 :base-bounty 500)
                              (:skeleton :animation (:idle "Skeleton_Running" :dead "Skeleton_Death") :base-hp 50.0 :speed 0.75 :base-bounty 1500)))

(defun game-scene-enemy-type-asset (type)
  (list :model (game-asset
                (pathname
                 (format nil "models/enemies/~A.glb"
                         (or (getf (assoc-value *enemy-types* type) :model)
                             (string-downcase (symbol-name type))))))))

(defun make-game-scene-enemy (&rest args
                              &key
                                (type :slime)
                                shader
                                (level 1)
                                (hp (* (getf (assoc-value *enemy-types* type) :base-hp) (/ (1+ level) 2.0)))
                                (speed (getf (assoc-value *enemy-types* type) :speed))
                              &allow-other-keys)
  (apply #'%make-game-scene-enemy
           (destructuring-bind (&key model (model-animations model))
               (game-scene-enemy-type-asset type)
             (list* :content (list (let ((model (eon:load-asset 'raylib:model model)))
                                     (when shader (apply-model-shader model shader))
                                     model))
                    :scale (raylib:vector3-scale (raylib:vector3-one) (/ 3.0))
                    :animations (cobj:ccoerce (eon:load-asset 'raylib:model-animations model-animations) 'list)
                    :hp hp :total-hp hp :speed speed
                    (remove-from-plistf args :shader :hp :total-hp :speed)))))

(defun game-scene-enemy-base-bounty (enemy)
  (getf (assoc-value *enemy-types* (game-scene-enemy-type enemy)) :base-bounty))

(defun game-scene-enemy-find-animation (enemy name)
  (getf (getf (assoc-value *enemy-types* (game-scene-enemy-type enemy)) :animation) name))

(defun game-scene-enemy-model (enemy)
  (first (game-scene-enemy-content enemy)))

(defun (setf game-scene-enemy-model) (value enemy)
  (setf (first (game-scene-enemy-content enemy)) value))

(defun game-scene-enemy-active-animation (enemy)
  (cobj:ccoerce (raylib:model-animation-name (first (game-scene-enemy-animations enemy))) 'string))

(defmacro letrec (bindings &body body)
  `(let ,(mapcar #'car bindings)
     (setf . ,(mappend #'identity  bindings))
     (locally . ,body)))

(defun (setf game-scene-enemy-active-animation) (name enemy)
  (ute:kill (game-scene-enemy-animation-tween enemy))
  (when name
    (rotatef
     (first (game-scene-enemy-animations enemy))
     (nth (position
           name (game-scene-enemy-animations enemy)
           :test #'string= :key (compose (rcurry #'cobj:ccoerce 'string) #'raylib:model-animation-name))
          (game-scene-enemy-animations enemy)))
    (let ((model (game-scene-enemy-model enemy))
          (model-animation (first (game-scene-enemy-animations enemy))))
      (labels ((tween ()
                 (let ((frame-index 0)
                       (frame-count (raylib:model-animation-frame-count model-animation)))
                   (flet ((frame-index () frame-index)
                          ((setf frame-index) (value)
                            (setf frame-index value)
                            (raylib:update-model-animation model model-animation frame-index)
                            frame-index))
                     (letrec ((tween (ute:tween
                                      :to (((eon:integer-float (frame-index)))
                                           ((eon:integer-float (1- frame-count))))
                                      :ease #'ute:linear-inout
                                      :duration (* frame-count (/ 60.0))
                                      :callback (lambda ()
                                                  (when (eq (game-scene-enemy-animation-tween enemy) tween)
                                                    (tween))))))
                       (setf (game-scene-enemy-animation-tween enemy) (ute:start tween)))))))
        (tween))))
  name)

(defun game-scene-enemy-promise-finish-animation (enemy)
  (promise:with-promise (succeed)
    (setf (ute:callback (game-scene-enemy-animation-tween enemy)) #'succeed)))

(defun game-scene-enemy-promise-die (enemy)
  (async
    (await (game-scene-enemy-promise-finish-animation enemy))
    (setf (game-scene-enemy-active-animation enemy) (game-scene-enemy-find-animation enemy :dead))
    (await (game-scene-enemy-promise-finish-animation enemy))
    (setf (game-scene-enemy-active-animation enemy) nil)
    (await (eon:promise-tween (ute:tween :to (((eon:integer-float (raylib:color-a (game-scene-enemy-color enemy)))) (0.0))
                                         :duration 0.5)))))

(defun game-scene-screen-add-enemy (screen enemy)
  (push enemy (game-scene-screen-enemies screen)))

(defun game-scene-screen-remove-enemy (screen enemy)
  (deletef (game-scene-screen-enemies screen) enemy)
  (loop :for tower :in (game-scene-screen-towers screen)
        :when (eq (game-scene-tower-target tower) enemy)
          :do (setf (game-scene-tower-target tower) nil)))

(defun game-scene-enemy-updater (enemy screen)
  (with-accessors ((path game-scene-enemy-path)) enemy
    (lambda ()
      (when-let ((target (first path)))
        (etypecase target
          (raylib:vector3
           (let ((position (game-scene-enemy-position enemy))
                 (rotation (game-scene-enemy-rotation enemy)))
             (clet ((direction (raylib:vector3-normalize (raylib:vector3-subtract target position))))
               (declare (dynamic-extent direction))
               (if (< (abs (+ (raylib:vector3-dot-product direction eon::+vector3-unit-z+) 1.0)) single-float-epsilon)
                   (clet ((quaternion (raylib:quaternion-from-axis-angle eon::+vector3-unit-y+ (eon::degree-radian 180.0))))
                     (declare (dynamic-extent quaternion))
                     (raylib:copy-quaternion quaternion rotation))
                   (clet ((quaternion (raylib:quaternion-from-vector3-to-vector3 eon::+vector3-unit-z+ direction)))
                     (declare (dynamic-extent quaternion))
                     (raylib:copy-quaternion quaternion rotation))))
             (update-position-toward-target position target (game-scene-enemy-speed enemy))
             (when (< (raylib:vector3-distance position target) single-float-epsilon)
               (pop path))))
          (function (funcall (pop path)))))
      (cond
        ((null path)
         (setf (game-scene-enemy-active-animation enemy) nil)
         (game-scene-screen-remove-enemy screen enemy)
         nil)
        ((not (plusp (game-scene-enemy-hp enemy)))
         (async
           (await (game-scene-enemy-promise-die enemy))
           (incf (game-scene-screen-money screen) (* (game-scene-enemy-base-bounty enemy) (floor (+ (game-scene-enemy-level enemy) 2) 3)))
           (setf (game-scene-enemy-active-animation enemy) nil)
           (game-scene-screen-remove-enemy screen enemy))
         nil)
        (t t)))))

(defmethod eon:scene3d-draw ((enemy game-scene-enemy) position origin scale rotation tint)
  (call-next-method)
  (with-accessors ((hp-bar game-scene-enemy-hp-bar)) enemy
    (unless hp-bar
      (eon:add-game-loop-hook
       (let ((camera eon:*scene3d-camera*))
         (lambda ()
           (setf hp-bar (eon:ensure-scene3d-node
                         (make-game-scene-enemy-hp-bar :target enemy)
                         :scale (raylib:vector3-scale (raylib:vector3-one) (/ 128.0))
                         :up (raylib:vector3-cross-product (raylib:get-camera-right camera) (raylib:get-camera-forward camera))))))
       :after nil))
    (rlgl:draw-render-batch-active)
    (eon:scene3d-draw-simple (game-scene-enemy-blood-emitter enemy) :position position)
    (rlgl:disable-depth-test)
    (rlgl:normal3f 0.0 1.0 0.0)
    (eon:scene3d-draw-simple hp-bar :position position :tint tint)
    (rlgl:draw-render-batch-active)
    (rlgl:enable-depth-test)))

(defun game-scene-map-enemy-paths (map)
  (mapcar
   (lambda (object)
     (cons (mapcar
            (lambda (point)
              (raylib:vector3-add
               (position-2d->3d
                (raylib:make-vector2
                 :x (/ (coerce (tiled:object-x object) 'single-float) (coerce +tile-width+ 'single-float))
                 :y (/ (coerce (tiled:object-y object) 'single-float) (coerce +tile-height+ 'single-float))))
               (position-2d->3d
                (raylib:make-vector2
                 :x (/ (coerce (car point) 'single-float) (coerce +tile-width+ 'single-float))
                 :y (/ (coerce (cdr point) 'single-float) (coerce +tile-height+ 'single-float))))))
            (tiled:polyline-points object))
           (read-from-string (gethash "waves" (tiled:properties object)))))
   (tiled:object-group-objects
    (find "enemy" (tiled:map-layers map) :key #'tiled:layer-name :test #'string=))))

(defun game-scene-tower-target-in-range-p (tower &optional (enemy (game-scene-tower-target tower)))
  (<= (raylib:vector3-distance
       (game-scene-tower-position tower)
       (game-scene-enemy-position enemy))
      (game-scene-tower-attack-radius tower)))

(defun game-scene-tower-attack (tower &optional enemies)
  (let ((main-enemy (game-scene-tower-target tower))
        (damage (* (game-scene-tower-attack-power tower)
                   (etypecase (game-scene-tower-attack-rate tower)
                     ((eql t) (eon:game-loop-delta-time))
                     (single-float 1.0)))))
    (flet ((damage (target)
             (eon:scene3d-particle-emitter-burst
              (game-scene-enemy-blood-emitter target)
              (etypecase (game-scene-tower-attack-rate tower)
                ((eql t) 1)
                (single-float 60)))
             (with-accessors ((hp game-scene-enemy-hp)) target
               (decf hp damage)
               (when (minusp hp) (setf hp 0.0)))))
      (etypecase (game-scene-tower-attack-rate tower)
        ((eql t)
         (cobj:with-monotonic-buffer-allocator (:size 64)
           (loop :with source := (position-3d->2d (game-scene-tower-position tower))
                 :and target := (position-3d->2d (game-scene-enemy-position main-enemy))
                 :for enemy :in (or enemies (list main-enemy))
                 :for point := (position-3d->2d (game-scene-enemy-position enemy))
                 :when (and (game-scene-tower-target-in-range-p tower enemy)
                            (plusp (game-scene-enemy-hp enemy))
                            (raylib:check-collision-point-line
                             (raylib:vector2-scale point 128.0)
                             (raylib:vector2-scale source 128.0)
                             (raylib:vector2-scale target 128.0)
                             128))
                   :do (damage enemy))))
        (single-float
         (push (letrec ((missile (make-game-scene-tower-missile
                                  :position (raylib:vector3-add
                                             (game-scene-tower-position tower)
                                             (raylib:make-vector3 :x 0.0 :y 0.5 :z 0.0))
                                  :target main-enemy
                                  :callback (lambda (hitp)
                                              (when hitp
                                                (damage main-enemy))
                                              (deletef (game-scene-tower-projectile tower) missile))
                                  :color raylib:+blue+)))
                 missile)
               (game-scene-tower-projectile tower)))))))

(defun game-scene-tower-try-attack (tower &optional enemies)
  (flet ((attack ()
           (when (loop :for enemy :in (if (game-scene-tower-target tower) (cons (game-scene-tower-target tower) enemies) enemies)
                       :when (and (game-scene-tower-target-in-range-p tower enemy)
                                  (plusp (game-scene-enemy-hp enemy)))
                         :return (setf (game-scene-tower-target tower) enemy)
                       :finally (setf (game-scene-tower-target tower) nil))
             (game-scene-tower-attack tower enemies))))
    (declare (dynamic-extent #'attack))
    (let ((rate (game-scene-tower-attack-rate tower)))
      (etypecase rate
        (boolean (when rate (attack)))
        (single-float
         (loop :initially (incf (game-scene-tower-charging tower) (eon:game-loop-delta-time))
               :while (>= (game-scene-tower-charging tower) rate)
               :do (decf (game-scene-tower-charging tower) rate)
                   (attack)))))))

(defstruct (game-scene-tower-missile (:include eon:scene3d-node))
  (speed 2.0)
  (target nil :type t)
  (callback #'values :type (function (boolean))))

(defmethod eon:scene3d-draw ((missile game-scene-tower-missile) position origin scale rotation tint)
  (eon:game-loop-once-only (missile)
    (when-let ((target (game-scene-tower-missile-target missile)))
      (setf (game-scene-tower-missile-target missile) nil)
      (eon:add-game-loop-hook
       (lambda ()
         (if (plusp (game-scene-enemy-hp target))
             (let ((source (game-scene-tower-missile-position missile))
                   (target (game-scene-enemy-position target))
                   (speed (game-scene-tower-missile-speed missile)))
               (update-position-toward-target source target speed)
               (not (when (<= (abs (raylib:vector3-distance source target)) single-float-epsilon)
                      (funcall (game-scene-tower-missile-callback missile) t)
                      t)))
             (progn (funcall (game-scene-tower-missile-callback missile) nil) nil)))
       :after #'identity)))
  (raylib:draw-cube-v
   (game-scene-tower-missile-position missile)
   (raylib:vector3-scale (raylib:vector3-one) 0.25)
   (game-scene-tower-missile-color missile)))

(defun game-scene-tower-update-projectile (tower)
  (setf (game-scene-tower-projectile tower)
        (ecase (game-scene-tower-type tower)
          ((:round-1 :round-2)
           (eon:make-scene3d-particle-emitter
            :position (raylib:vector3-add
                       (game-scene-tower-position tower)
                       (raylib:make-vector3 :x 0.0 :y 0.5 :z 0.0))
            :rate (lambda () (if (game-scene-tower-target tower) 120.0 0.0))
            :capacity 512
            :updater (eon:scene3d-particle-emitter-laser-updater
                      (lambda (particle)
                        (declare (ignore particle))
                        (if-let ((target (game-scene-tower-target tower)))
                          (game-scene-enemy-position target)
                          (game-scene-tower-position tower)))
                      :axial-velocity 4.0
                      :axial-acceleration 1.0
                      :normal-velocity (raylib:vector2-zero)
                      :normal-offset (eon:make-particle-3d-vector2-generator 0.1))
            :renderer (eon:particle-3d-cube-renderer 0.1 (eon:particle-3d-interpolate-color-over-age raylib:+green+ (raylib:fade raylib:+blue+ 0.0) #'ute:sine-in))))
          ((:square-1 :square-2 nil) nil))))

(defmacro with-popped-ui ((group ui) &body body)
  (once-only (group ui)
    `(prog2 (eon:scene2d-group-add-child ,group ,ui)
         (progn . ,body)
       (eon:scene2d-group-remove-child ,group ,ui))))

(eon:define-scene2d-constructed message-window
    (eon:scene2d-cell
     :size (#.(float +viewport-width+) #.(float +viewport-height+))
     :child (eon:scene2d-coordinate-truncator
             :child (eon:scene2d-window
                     :child (eon:scene2d-box
                             :orientation :vertical
                             :children ((eon:scene2d-margin
                                         :left 2.0 :right 2.0 :top 2.0 :bottom 2.0
                                         :child (eon:scene2d-label :string "Title" :name label-title :style (eon:scene2d-label-style :color raylib:+red+)))
                                        (eon:scene2d-margin
                                         :left 2.0 :right 2.0 :top 2.0 :bottom 8.0
                                         :child (eon:scene2d-label :string "Message" :name label-message))
                                        (eon:scene2d-margin
                                         :left 2.0 :right 2.0 :top 2.0 :bottom 2.0
                                         :child (eon:select-box
                                                 :layout (T 1)
                                                 :children ()
                                                 :name select-box)))))))
  (:constructor (&key (title "Title") (message "Message") (choices '("OK")))
      (let ((window (%make-message-window)))
        (setf (eon:scene2d-label-string (message-window-label-title window)) title
              (eon:scene2d-label-string (message-window-label-message window)) message)
        (dolist (choice choices)
          (eon:select-box-add-child
           (message-window-select-box window)
           (eon:scene2d-construct (eon:scene2d-max-cell :size (64.0 0.0) :child (eon:scene2d-label :string choice)))))
        window)))

(defun promise-confirm-message (title message group)
  (let ((window (make-message-window :title title :message message)))
    (eon:scene2d-layout window)
    (async
      (with-popped-ui (group window)
        (await (eon:select-box-promise-index (message-window-select-box window)))))))

(defun promise-yes-or-no-p (title message group)
  (let ((window (make-message-window :title title :message message :choices '("YES" "NO"))))
    (eon:scene2d-layout window)
    (async
      (with-popped-ui (group window)
        (eql (await (eon:select-box-promise-index (message-window-select-box window))) 0)))))

(defun main ()
  (raylib:set-config-flags (cffi:foreign-bitfield-value 'raylib:config-flags '(:window-resizable)))
  (raylib:with-window ("Spring Lisp Game Jam 2024" (+viewport-width+ +viewport-height+))
    (raylib:set-target-fps 60)
    (eon:with-game-context
      (let* ((map (tiled:load-map (game-asset #P"maps/map-1.tmx")))
             (screen (make-game-scene-screen :map-renderer (eon:tiled-map-renderer map))))
        (eon:scene2d-layout (game-scene-screen-ui screen))
        (with-accessors ((money game-scene-screen-money)) screen
          (setf (game-scene-screen-money screen) 5000)
          (loop :with group := (eon:scene2d-construct (eon:scene2d-group))
                :for cell :in (tiled:layer-cells (find "ground" (tiled:map-layers map) :key #'tiled:layer-name :test #'string=))
                :when (gethash "base" (tiled:properties (tiled:cell-tile cell)))
                  :do (push (make-game-scene-tower
                             :shader (game-scene-screen-shader screen)
                             :position (position-2d->3d
                                        (raylib:make-vector2
                                         :x (+ (coerce (tiled:cell-column cell) 'single-float) 0.5)
                                         :y (+ (coerce (tiled:cell-row cell) 'single-float) 0.5))))
                            (game-scene-screen-towers screen))
                  :and :collect (eon::make-scene2d-focusable
                                 :focus-point (raylib:make-vector2
                                               :x (coerce (tiled:cell-column cell) 'single-float)
                                               :y (coerce (tiled:cell-row cell) 'single-float))
                                 :content (first (game-scene-screen-towers screen)))
                         :into focusables
                :finally (setf (game-scene-screen-tower-base-focus-manager screen) (eon:make-scene2d-focus-manager :focusables focusables)))
          (let ((selected-tower nil))
            (flet ((unselect-tower (tower)
                     (setf (game-scene-tower-selectedp tower) nil))
                   (select-tower (tower)
                     (setf (game-scene-tower-selectedp (setf selected-tower tower)) t)
                     (game-scene-screen-look-at screen (game-scene-tower-position tower)))
                   (tower-screen-position (&optional (tower selected-tower))
                     (raylib:get-world-to-screen-ex
                      (game-scene-tower-position tower)
                      (game-scene-screen-camera screen)
                      +viewport-width+ +viewport-height+)))
              (select-tower (lastcar (game-scene-screen-towers screen)))
              (async
                (loop :with focus-manager := (game-scene-screen-tower-base-focus-manager screen)
                      :with ui-group := (game-scene-ui-group (game-scene-screen-ui screen))
                      :for key := (await (eon:promise-pressed-key))
                      :do (case key
                            ((:left :right :up :down)
                             (unselect-tower (eon::scene2d-focusable-content (eon:scene2d-focus-manager-focused focus-manager)))
                             (eon:scene2d-focus-manager-handle-key focus-manager key)
                             (select-tower (eon::scene2d-focusable-content (eon:scene2d-focus-manager-focused focus-manager))))
                            (:a
                             (let* ((operations (append
                                                 (cond
                                                   ((null (game-scene-tower-type selected-tower)) '(build))
                                                   ((< (game-scene-tower-level selected-tower) 3) '(upgrade)))
                                                 (when (game-scene-tower-type selected-tower) '(demolish))
                                                 '(cancel)))
                                    (select-box (eon:scene2d-construct
                                                 (eon:select-box
                                                  :entries (mapcar #'symbol-name operations))))
                                    (operation-selector (eon:scene2d-construct (eon:scene2d-window :child select-box))))
                               (raylib:copy-vector2
                                (tower-screen-position)
                                (eon:scene2d-position operation-selector))
                               (eon:scene2d-layout operation-selector)
                               (with-popped-ui (ui-group operation-selector)
                                 (when-let ((index (await (eon:select-box-promise-index select-box))))
                                   (ecase (nth index operations)
                                     (build
                                      (let* ((tower-select-box (let ((table (eon:scene2d-construct (eon:scene2d-table))))
                                                                 (dolist (tower-type *tower-types*)
                                                                   (destructuring-bind (type &key cost &allow-other-keys) tower-type
                                                                     (eon:scene2d-table-newline table)
                                                                     (eon:scene2d-table-add-child
                                                                      table
                                                                      (eon:scene2d-construct
                                                                       (eon:scene2d-label :string (symbol-name type))))
                                                                     (eon:scene2d-table-add-child
                                                                      table
                                                                      (eon:scene2d-construct
                                                                       (eon:scene2d-margin
                                                                        :left 16.0
                                                                        :right 2.0
                                                                        :top 1.0
                                                                        :bottom 1.0
                                                                        :child (eon:scene2d-label :string "$"))))
                                                                     (eon:scene2d-table-add-child
                                                                      table
                                                                      (eon:scene2d-construct
                                                                       (eon:scene2d-label :string (princ-to-string (assoc-value cost 1)))))))
                                                                 (eon:table-select-box table)))
                                             (tower-selector (eon:scene2d-construct
                                                              (eon:scene2d-window :child tower-select-box))))
                                        (eon:scene2d-layout tower-selector)
                                        (raylib:copy-vector2
                                         (tower-screen-position)
                                         (eon:scene2d-position tower-selector))
                                        (with-popped-ui (ui-group tower-selector)
                                          (when-let ((index (await (eon:select-box-promise-index tower-select-box))))
                                            (destructuring-bind (type &key cost &allow-other-keys) (nth index *tower-types*)
                                              (setf cost (assoc-value cost 1))
                                              (if (<= cost money)
                                                  (progn
                                                    (decf money cost)
                                                    (setf (game-scene-tower-type selected-tower) type
                                                          (game-scene-tower-level selected-tower) 1)
                                                    (game-scene-tower-update selected-tower))
                                                  (await (promise-confirm-message "WARNING" "You don't have enough money to build this tower!" ui-group))))))))
                                     (upgrade
                                      (let ((cost (assoc-value
                                                   (getf (assoc-value *tower-types* (game-scene-tower-type selected-tower)) :cost)
                                                   (1+ (game-scene-tower-level selected-tower)))))
                                        (when (await
                                               (promise-yes-or-no-p
                                                "CONFIRMATION"
                                                (format nil "Do you want to spend $~D to upgrade this tower?" cost)
                                                ui-group))
                                          (if (<= cost money)
                                              (progn
                                                (decf money cost)
                                                (incf (game-scene-tower-level selected-tower))
                                                (game-scene-tower-update selected-tower))
                                              (await (promise-confirm-message "WARNING" "You don't have enough money to upgrade this tower!" ui-group))))))
                                     (demolish
                                      (let ((refund (let* ((level-cost (getf (assoc-value *tower-types* (game-scene-tower-type selected-tower)) :cost))
                                                           (end (position (game-scene-tower-level selected-tower) level-cost :key #'car)))
                                                      (floor (reduce #'+ (subseq level-cost 0 (1+ end)) :key #'cdr) 2))))
                                        (when (await
                                               (promise-yes-or-no-p
                                                "CONFIRMATION"
                                                (format nil "Do you want to demolish this tower to receive $~D?" refund)
                                                ui-group))
                                          (incf money refund)
                                          (setf (game-scene-tower-level selected-tower) 0
                                                (game-scene-tower-type selected-tower) nil)
                                          (game-scene-tower-update selected-tower))))
                                     (cancel))))))))))))
        (let ((paths (game-scene-map-enemy-paths map)))
          (loop :with sprites := (eon:array-vector (eon:split-texture (eon:load-asset 'raylib:texture (game-asset #P"flag.png")) '(1 5)))
                :for (path . nil) :in paths
                :for billboard := (eon:make-scene3d-billboard
                                   :content (first-elt sprites)
                                   :position (raylib:copy-vector3 (lastcar path))
                                   :origin (raylib:make-vector3
                                            :x (/ (eon:texture-region-width (first-elt sprites)) 8.0)
                                            :y (eon:texture-region-height (first-elt sprites))
                                            :z 0.0)
                                   :scale (raylib:vector3-scale (raylib:vector3-one) (/ 2.0 (eon:texture-region-height (first-elt sprites)))))
                :do (letrec ((timeline (ute:timeline
                                        (:sequence
                                         (:tween (eon:scene3d-billboard-tween-frames billboard sprites :duration 0.5))
                                         (:call (lambda ()
                                                  (unless (find billboard (game-scene-screen-objects screen))
                                                    (ute:kill timeline))))
                                         :repeat t))))
                      (ute:start timeline))
                    (push billboard (game-scene-screen-objects screen)))
          (async
            (loop :for wave-index :below (reduce #'max paths :key (compose #'length #'cdr))
                  :do (await (apply #'ajoin (loop :for (path . waves-desc) :in paths
                                                  :collect (let ((path path)
                                                                 (waves-desc waves-desc))
                                                             (async
                                                               (loop :for enemy-desc :in (nth wave-index waves-desc)
                                                                     :do (destructuring-bind (type &key (interval 1.0) (count 1) (level 1)) enemy-desc
                                                                           (loop :repeat count
                                                                                 :when type
                                                                                   :do (push
                                                                                        (let ((enemy (make-game-scene-enemy
                                                                                                      :shader (game-scene-screen-shader screen)
                                                                                                      :position (raylib:copy-vector3 (first path))
                                                                                                      :type type
                                                                                                      :level level
                                                                                                      :path (rest path))))
                                                                                          (eon:add-game-loop-hook (game-scene-enemy-updater enemy screen) :after #'identity)
                                                                                          (setf (game-scene-enemy-active-animation enemy) (game-scene-enemy-find-animation enemy :idle))
                                                                                          enemy)
                                                                                        (game-scene-screen-enemies screen))
                                                                                 :do (await (eon:promise-sleep interval)))))))))))))
        (setf (eon:current-screen) screen))
      (eon:do-screen-loop (eon:make-fit-viewport :width +viewport-width+ :height +viewport-height+)))))
