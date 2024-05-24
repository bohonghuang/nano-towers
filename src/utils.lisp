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
