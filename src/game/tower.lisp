(in-package #:nano-towers)

(defstruct (game-scene-tower (:include eon:scene3d-container)
                             (:constructor %make-game-scene-tower))
  (scene nil :type basic-scene)
  (selectedp nil :type boolean)
  (type nil :type symbol)
  (level 0 :type (integer 0 3))
  (target nil :type t)
  (charging 0.0 :type single-float)
  (projectile nil :type (or list eon:scene3d-particle-emitter)))

(defparameter *tower-types*
  '((:square-1 :cost ((1 . 800) (2 . 1200) (3 . 1600))
               :rate 0.75
               :radius ((1 . 2.0) (2 . 3.0) (3 . 4.0))
               :power ((1 . 15) (2 . 25) (3 . 35))
               :model ((1 . "towerSquare_sampleF")
                       (2 . "towerSquare_sampleD")
                       (3 . "towerSquare_sampleE")))
    (:square-2 :cost ((1 . 1600) (2 . 2000) (3 . 2400))
               :rate 1.0
               :radius ((1 . 2.0) (2 . 3.0) (3 . 4.0))
               :power ((1 . 30) (2 . 50) (3 . 70))
               :model ((1 . "towerSquare_sampleC")
                       (2 . "towerSquare_sampleA")
                       (3 . "towerSquare_sampleB")))
    (:round-1 :cost ((1 . 1000) (2 . 1500) (3 . 2000))
              :rate t
              :radius ((1 . 2.0) (2 . 3.0) (3 . 4.0))
              :power ((1 . 4) (2 . 8) (3 . 12))
              :model ((1 . "towerRound_sampleA")
                      (2 . "towerRound_sampleC")
                      (3 . "towerRound_sampleE")))
    (:round-2 :cost ((1 . 1500) (2 . 2000) (3 . 2500))
              :rate t
              :radius ((1 . 2.5) (2 . 3.5) (3 . 4.5))
              :power ((1 . 6) (2 . 12) (3 . 16))
              :model ((1 . "towerRound_sampleB")
                      (2 . "towerRound_sampleD")
                      (3 . "towerRound_sampleF")))))

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
  (dolist (material (cobj:ccoerce (cobj:cpointer-carray (raylib:model-materials model) (raylib:model-material-count model)) 'list) model)
    (setf (raylib:material-shader material) shader)))

(defun game-scene-tower-update-model (tower)
  (setf (game-scene-tower-model tower)
        (destructuring-bind (&key model)
            (game-scene-tower-type-level-asset
             (game-scene-tower-type tower)
             (game-scene-tower-level tower))
          (when model
            (let ((model (eon:load-asset 'raylib:model model)))
              (apply-model-shader model (basic-scene-shader (game-scene-tower-scene tower)))
              model)))))

(defun game-scene-tower-update (tower)
  (game-scene-tower-update-model tower)
  (game-scene-tower-update-projectile tower))

(defmethod eon:scene3d-draw ((tower game-scene-tower) position origin scale rotation tint)
  (when (game-scene-tower-selectedp tower)
    (raylib:draw-cube position 1.0 0.25 1.0 (raylib:fade raylib:+white+ 0.5)))
  (call-next-method)
  (eon:scene3d-draw-simple (game-scene-tower-projectile tower)))
