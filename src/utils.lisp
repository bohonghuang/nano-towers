(in-package #:nano-towers)

(defun game-asset (pathname)
  (merge-pathnames pathname #.(merge-pathnames #P"assets/" (asdf:component-pathname (asdf:find-system '#:nano-towers)))))

(defmethod eon:scene3d-draw :around ((emitter eon:scene3d-particle-emitter) position origin scale rotation tint)
  (rlgl:disable-depth-mask)
  (call-next-method)
  (rlgl:enable-depth-mask)
  (rlgl:draw-render-batch-active))

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

(defun promise-cancel-all-input ()
  (promise:with-promise (succeed)
    (eon:add-game-loop-hook
     (lambda ()
       (if eon::*key-queue*
           (prog1 (setf eon::*key-queue* nil) (succeed))
           (push :b eon::*key-queue*)))
     :after #'identity)))

(defun bounding-box-sample (bounding-box &optional (origin (raylib:vector3-zero)) (scale (raylib:vector3-one)))
  (let ((min (raylib:bounding-box-min bounding-box))
        (max (raylib:bounding-box-max bounding-box)))
    (incf (raylib:vector3-x origin) (* (raylib:vector3-x scale) (+ (raylib:vector3-x min) (random (- (raylib:vector3-x max) (raylib:vector3-x min))))))
    (incf (raylib:vector3-y origin) (* (raylib:vector3-y scale) (+ (raylib:vector3-y min) (random (- (raylib:vector3-y max) (raylib:vector3-y min))))))
    (incf (raylib:vector3-z origin) (* (raylib:vector3-z scale) (+ (raylib:vector3-z min) (random (- (raylib:vector3-z max) (raylib:vector3-z min))))))
    origin))

(declaim (ftype (function (eon:particle-3d &optional single-float)) particle-3d-bounce))
(defun particle-3d-bounce (particle &optional (coefficient 1.0))
  (clet ((particle (cthe (:pointer (:struct eon:particle-3d)) (& particle))))
    (when (minusp (-> particle eon::position raylib:y))
      (setf (-> particle eon::position raylib:y) 0.0
            (-> particle eon::position-velocity raylib:y) (* (-> particle eon::position-velocity raylib:y) (- coefficient))))))
