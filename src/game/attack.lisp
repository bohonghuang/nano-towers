(in-package #:nano-towers)

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
