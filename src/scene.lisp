(in-package #:spring-lisp-game-jam-2024)

(cobj:define-global-cobject +camera-default+ (raylib:make-camera-3d
                                              :position (raylib:make-vector3 :x 0.0 :y 10.0 :z 5.0)
                                              :target (raylib:vector3-zero)
                                              :up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0)
                                              :fovy (/ 0.03) :projection #.(cffi:foreign-enum-value 'raylib:camera-projection :perspective)))

(defclass basic-scene ()
  ((camera :initarg :camera :initform (raylib:copy-camera +camera-default+) :type raylib:camera-3d :accessor basic-scene-camera)
   (light-camera :initarg :light-camera :initform (raylib:make-camera) :type raylib:camera-3d :accessor basic-scene-light-camera)
   (shadow :initarg :shadow :initform (eon:make-shadow-map-renderer) :type eon:shadow-map-renderer :accessor basic-scene-shadow)
   (shader :initarg :shader :initform (eon:load-asset 'raylib:shader (game-asset #P"scene")) :type raylib:shader :accessor basic-scene-shader)
   (shader-uniforms :initarg :shader-uniforms :initform (make-basic-scene-shader-uniforms) :type cobj:cobject :accessor basic-scene-shader-uniforms)))

(defvar *basic-scene-shader-uniforms-shadow-map*)

(cobj:define-global-cobject +light-position-default+ (raylib:make-vector3 :x -1.0 :y 4.0 :z 2.0))

(eon:define-shaderable-uniforms basic-scene
  ("colDiffuse" raylib:+white+ :type raylib:color)
  ("colEmission" raylib:+black+ :type raylib:color)
  ("colAmbient" (raylib:color-brightness raylib:+white+ -0.5) :type raylib:color)
  ("colSpecular" raylib:+black+ :type raylib:color)
  ("colLight" raylib:+lightgray+ :type raylib:color)
  ("lightVector" (raylib:vector3-negate (raylib:vector3-normalize +light-position-default+)) :type raylib:vector3)
  ("shadowLightMatrix" (raylib:make-matrix) :type raylib:matrix)
  ("shadowIntensity" (/ 3.0) :type single-float)
  ("shadowMap" *basic-scene-shader-uniforms-shadow-map* :type raylib:texture))

(cobj:define-global-cobject +shadow-light-camera-default+
    (raylib:make-camera-3d :position +light-position-default+
                           :target (raylib:make-vector3 :x 0.0 :y 0.0 :z 0.0)
                           :up (raylib:make-vector3 :x 0.0 :y 1.0 :z 0.0) :fovy 10.0
                           :projection (cffi:foreign-enum-value 'raylib:camera-projection :orthographic)))

(defmethod initialize-instance :around ((scene basic-scene) &rest args)
  (declare (ignore args))
  (let* ((light-camera (raylib:copy-camera +shadow-light-camera-default+))
         (shadow (eon:make-shadow-map-renderer :camera light-camera :size (raylib:make-vector2 :x 1024.0 :y 512.0))))
    (let ((*basic-scene-shader-uniforms-shadow-map* (eon:shadow-map-renderer-texture shadow)))
      (call-next-method))
    (setf (basic-scene-shadow scene) shadow (basic-scene-light-camera scene) light-camera)
    (initialize-basic-scene-shader-uniforms scene)
    (setf (eon:shadow-map-renderer-matrix shadow) (basic-scene-shadow-light-matrix scene))))

(defgeneric basic-scene-draw-map (scene)
  (:method (scene) (declare (ignore scene))))

(defgeneric basic-scene-draw-objects (scene)
  (:method (scene) (declare (ignore scene))))

(defmethod eon:scene3d-draw ((scene basic-scene) position origin scale rotation tint)
  (declare (ignore position origin scale rotation tint))
  (flet ((render-objects (&aux (eon:*scene3d-camera* (basic-scene-camera scene))) (basic-scene-draw-objects scene)))
    (eon:shadow-map-renderer-render (basic-scene-shadow scene) #'render-objects)
    (raylib:with-mode-3d (basic-scene-camera scene)
      (raylib:with-shader-mode (basic-scene-shader scene)
        (raylib:copy-color raylib:+white+ (basic-scene-col-diffuse scene))
        (update-basic-scene-shader-uniforms scene)
        (basic-scene-draw-map scene)
        (rlgl:draw-render-batch-active)
        (let ((shadow-intensity (basic-scene-shadow-intensity scene)))
          (setf (basic-scene-shadow-intensity scene) 0.0)
          (update-basic-scene-shader-uniforms scene)
          (setf (basic-scene-shadow-intensity scene) shadow-intensity))
        (render-objects)))))

(defun basic-scene-look-at (scene target)
  (let ((camera (basic-scene-camera scene))
        (light-camera (basic-scene-light-camera scene)))
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
