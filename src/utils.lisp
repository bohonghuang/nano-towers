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
