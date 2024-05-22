(in-package #:spring-lisp-game-jam-2024)

(defstruct (game-scene-enemy (:include eon:scene3d-container)
                             (:constructor %make-game-scene-enemy))
  (scene nil :type basic-scene)
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

(defparameter *enemy-types* '((:dragon :animation (:idle "Dragon_Flying" :death "Dragon_Death" :attack "Dragon_Attack") :base-hp 150.0 :speed 1.0 :base-bounty 1000)
                              (:slime :animation (:idle "Slime_Walk" :death "Slime_Death" :attack "Slime_Attack") :base-hp 50.0 :speed 0.5 :base-bounty 250)
                              (:bat :animation (:idle "Bat_Flying" :death "Bat_Death" :attack "Bat_Attack") :base-hp 25.0 :speed 1.5 :base-bounty 250)
                              (:skeleton :animation (:idle "Skeleton_Running" :death "Skeleton_Death" :attack "Skeleton_Attack") :base-hp 50.0 :speed 0.75 :base-bounty 500)))

(defun game-scene-enemy-type-asset (type)
  (list :model (game-asset
                (pathname
                 (format nil "models/enemies/~A.glb"
                         (or (getf (assoc-value *enemy-types* type) :model)
                             (string-downcase (symbol-name type))))))))

(defun make-game-scene-enemy (&rest args
                              &key
                                (type :slime)
                                (scene nil)
                                (level 1)
                                (hp (* (getf (assoc-value *enemy-types* type) :base-hp) (/ (1+ level) 2.0)))
                                (speed (getf (assoc-value *enemy-types* type) :speed))
                              &allow-other-keys
                              &aux (shader (basic-scene-shader scene)))
  (apply #'%make-game-scene-enemy
           (destructuring-bind (&key model (model-animations model))
               (game-scene-enemy-type-asset type)
             (list* :content (list (let ((model (eon:load-asset 'raylib:model model)))
                                     (when shader (apply-model-shader model shader))
                                     model))
                    :scale (raylib:vector3-scale (raylib:vector3-one) (/ 3.0))
                    :animations (cobj:ccoerce (eon:load-asset 'raylib:model-animations model-animations) 'list)
                    :hp hp :total-hp hp :speed speed
                    (remove-from-plistf args :hp :total-hp :speed)))))

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

(defun game-scene-enemy-promise-attack (enemy)
  (async
    (await (game-scene-enemy-promise-finish-animation enemy))
    (setf (game-scene-enemy-active-animation enemy) (game-scene-enemy-find-animation enemy :attack))
    (await (game-scene-enemy-promise-finish-animation enemy))
    (setf (game-scene-enemy-active-animation enemy) nil)))

(defun game-scene-enemy-promise-die (enemy)
  (async
    (await (game-scene-enemy-promise-finish-animation enemy))
    (setf (game-scene-enemy-active-animation enemy) (game-scene-enemy-find-animation enemy :death))
    (await (game-scene-enemy-promise-finish-animation enemy))
    (setf (game-scene-enemy-active-animation enemy) nil)
    (await (eon:promise-tween (ute:tween :to (((eon:integer-float (raylib:color-a (game-scene-enemy-color enemy)))) (0.0))
                                         :duration 0.5)))))

(defun game-context-add-enemy (context enemy)
  (push enemy (game-context-enemies context)))

(defun game-context-remove-enemy (context enemy)
  (deletef (game-context-enemies context) enemy)
  (loop :for tower :in (game-context-towers context)
        :when (eq (game-scene-tower-target tower) enemy)
          :do (setf (game-scene-tower-target tower) nil)))

(defun game-scene-enemy-updater (enemy &aux (scene (game-scene-enemy-scene enemy)))
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
      (let ((context (game-scene-context scene)))
        (cond
          ((null path)
           (async
             (push :b eon::*key-queue*)
             (basic-scene-look-at scene (game-scene-enemy-position enemy))
             (setf (game-context-game-over-p context) t)
             (loop :for enemy :in (game-context-enemies context)
                   :do (setf (game-scene-enemy-speed enemy) 0.0))
             (await (game-scene-enemy-promise-attack enemy))
             (setf (game-scene-enemy-active-animation enemy) nil)
             (game-context-remove-enemy context enemy)
             (game-over))
           nil)
          ((not (plusp (game-scene-enemy-hp enemy)))
           (async
             (await (game-scene-enemy-promise-die enemy))
             (incf (game-context-money context) (* (game-scene-enemy-base-bounty enemy) (floor (+ (game-scene-enemy-level enemy) 7) 8)))
             (setf (game-scene-enemy-active-animation enemy) nil)
             (game-context-remove-enemy context enemy))
           nil)
          (t t))))))

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
