(in-package #:nano-towers)

(defclass main-menu-scene (basic-scene)
  ((map-renderer :type eon:tiled-renderer)
   (model :type eon:scene3d-node)
   (emitter :type eon:scene3d-particle-emitter)))

(defmethod initialize-instance :around ((scene main-menu-scene) &rest args)
  (declare (ignore args))
  (call-next-method)
  (with-slots (map-renderer model emitter) scene
    (let* ((map (tiled:load-map (game-asset #P"maps/menu.tmx")))
           (origin (let ((object (find "origin"
                                       (tiled:object-group-objects
                                        (find "object" (tiled:map-layers map)
                                              :key #'tiled:layer-name :test #'string=))
                                       :key #'tiled:object-name :test #'string=)))
                     (raylib:make-vector3
                      :x (/ (coerce (tiled:object-x object) 'single-float)
                            (coerce +tile-width+ 'single-float))
                      :y 0.0
                      :z (/ (coerce (tiled:object-y object) 'single-float)
                            (coerce +tile-height+ 'single-float))))))
      (setf map-renderer (eon:tiled-map-renderer map)
            model (eon:make-scene3d-container
                   :content (apply-model-shader
                             (eon:load-asset 'raylib:model (game-asset #P"models/towers/towerRound_sampleF.glb"))
                             (basic-scene-shader scene))
                   :position (raylib:copy-vector3 origin))
            emitter (eon:make-scene3d-particle-emitter
                     :rate 60.0
                     :capacity 512
                     :position (raylib:copy-vector3 origin)
                     :updater (lambda (position)
                                (let ((velocity (raylib:make-vector3 :x 0.75 :y 5.0 :z 0.0))
                                      (acceleration (raylib:make-vector3 :x 0.0 :y -10.0 :z 0.0)))
                                  (lambda (particle-object)
                                    (clet ((particle (cthe (:pointer (:struct eon:particle-3d)) (& particle-object)))
                                           (velocity (cthe (:pointer (:struct raylib:vector3)) (& velocity)))
                                           (acceleration (cthe (:pointer (:struct raylib:vector3)) (& acceleration))))
                                      (if (zerop (-> particle eon::age))
                                          (progn
                                            (eon:particle-3d-initialize-default particle-object position)
                                            (raylib:%vector3-rotate-by-axis-angle velocity velocity (& eon::+vector3-unit-y+) -0.1)
                                            (csetf (-> particle eon::position-velocity) velocity
                                                   (-> particle eon::position-acceleration) acceleration)
                                            (setf (-> particle eon::lifetime) 3.0))
                                          (progn
                                            (eon:particle-3d-update-motion particle-object)
                                            (maxf (-> particle eon::position-velocity raylib:y) -1.0)
                                            (particle-3d-bounce particle-object 4.0)))))))
                     :renderer (eon:particle-3d-cube-renderer 0.05 (eon:particle-3d-interpolate-color-over-age raylib:+red+ (raylib:fade raylib:+red+ 0.0) #'ute:circ-in))))
      (let ((camera (basic-scene-camera scene)))
        (setf (raylib:camera-target camera) (raylib:vector3-add origin (raylib:make-vector3 :x 0.0 :y 0.4 :z 0.0))
              (raylib:camera-position camera) (raylib:vector3-add origin (raylib:make-vector3 :x 0.0 :y 3.0 :z 4.0)))))))

(defmethod basic-scene-draw-map ((scene main-menu-scene))
  (rlgl:push-matrix)
  (rlgl:rotatef 90.0 1.0 0.0 0.0)
  (rlgl:scalef (/ (coerce +tile-width+ 'single-float)) (/ (coerce +tile-height+ 'single-float)) (- single-float-epsilon))
  (funcall (slot-value scene 'map-renderer))
  (rlgl:pop-matrix))

(defmethod basic-scene-draw-objects ((scene main-menu-scene))
  (with-slots (model emitter) scene
    (eon:scene3d-draw-simple model)
    (eon:scene3d-draw-simple emitter)))

(defmethod eon:scene3d-draw ((scene main-menu-scene) position origin scale rotation tint)
  (let ((camera (basic-scene-camera scene)))
    (clet* ((view (raylib:vector3-transform
                   (raylib:vector3-subtract
                    (raylib:camera-position camera)
                    (raylib:camera-target camera))
                   (raylib:matrix-rotate (raylib:get-camera-up camera) (/ (eon:game-loop-delta-time) 8.0))))
            (position (raylib:vector3-add (raylib:camera-target camera) view)))
      (declare (dynamic-extent view position))
      (setf (raylib:camera-position camera) position)))
  (call-next-method))

(defstruct (select-box-transparency-entry (:include eon:scene2d-focusable))
  (selectedp nil :type boolean)
  (alpha 1.0 :type single-float))

(defun select-box-transparency-entry (child)
  (make-select-box-transparency-entry :content child))

(defmethod eon:select-box-entry-focused-p ((entry select-box-transparency-entry))
  (select-box-transparency-entry-selectedp entry))

(defmethod (setf eon:select-box-entry-focused-p) (value (entry select-box-transparency-entry))
  (setf (select-box-transparency-entry-selectedp entry) value)
  (let ((alpha (if value 1.0 (/ 3.0))))
    (ute:start (ute:tween :to (((select-box-transparency-entry-alpha entry)) (alpha)) :duration 0.25))))

(defmethod eon:scene2d-draw ((entry select-box-transparency-entry) position origin scale rotation tint)
  (clet ((color (raylib:get-color #xFFFFFF)))
    (declare (dynamic-extent color))
    (raylib:copy-color tint color)
    (setf (raylib:color-a color) (floor (* (coerce (raylib:color-a color) 'single-float)
                                           (select-box-transparency-entry-alpha entry))))
    (call-next-method entry position origin scale rotation color)))

(define-constant +game-title+ "Nano tOwErs" :test #'string=)

(eon:define-scene2d-constructed main-menu-ui
    (eon:scene2d-group
     :name group
     :children ((eon:scene2d-cell
                 :name cell
                 :size (#.(float +viewport-width+) #.(float +viewport-height+))
                 :child (eon:scene2d-box
                         :children ((eon:scene2d-margin
                                     :top 80.0
                                     :bottom 40.0
                                     :child (eon:scene2d-label
                                             :string +game-title+
                                             :style (eon:scene2d-label-style
                                                     :text-style (eon:text-style :size 60.0 :spacing 8.0)
                                                     :color raylib:+white+
                                                     :shadow nil :outline raylib:+darkgray+)))
                                    (eon:scene2d-margin
                                     :top 80.0
                                     :bottom 80.0
                                     :child (eon:scene2d-window
                                             :style (eon:scene2d-window-style
                                                     :background (eon:scene2d-rectangle :color (raylib:fade raylib:+black+ 0.5)))
                                             :child (eon:scene2d-margin
                                                     :all 2.0
                                                     :child (eon:select-box
                                                             :name select-box
                                                             :layout (1 T)
                                                             :style (eon:select-box-style :entry-type 'select-box-transparency-entry)
                                                             :children (flet ((label (string)
                                                                                (eon:scene2d-construct
                                                                                 (eon:scene2d-max-cell
                                                                                  :size (256.0 40.0)
                                                                                  :child (eon:scene2d-label
                                                                                          :string string
                                                                                          :style (eon:scene2d-label-style
                                                                                                  :text-style (eon:text-style :size 30.0 :spacing 4.0)
                                                                                                  :color raylib:+raywhite+
                                                                                                  :shadow nil :outline raylib:+darkgray+))))))
                                                                         (mapcar #'label '("START" "CREDIT" "EXIT")))))))))))))

(eon:define-scene2d-constructed credit-page
    (eon:scene2d-max-cell
     :size (#.(float +viewport-width+) 0.0)
     :child (eon:scene2d-box
             :children ((eon:scene2d-label :string "Presented by bohonghuang, for the submission of Spring Lisp Game Jam 2024.

Powered by the EON framework based on Raylib.

Game assets provided by:

* Lowpoly Animated Monsters by quaternius
* Tower Defense Kit by Kenney
* Flag with Animation by ankousse26
* 2D Tile Map by inScope"
                                           :style (default-label-style))
                        (eon:scene2d-margin
                         :top 40.0
                         :child (eon:scene2d-label :string "Thanks for playing!"
                                                   :style (eon:scene2d-label-style
                                                           :color raylib:+raywhite+
                                                           :text-style (eon:text-style :size 20.0 :spacing 4.0)
                                                           :shadow nil :outline raylib:+darkgray+)))))))

(defstruct main-menu-screen
  (scene (make-instance 'main-menu-scene) :type main-menu-scene)
  (ui (make-main-menu-ui) :type eon:scene2d-constructed)
  (credit (make-credit-page) :type eon:scene2d-constructed))

(defmethod eon:screen-render ((screen main-menu-screen))
  (raylib:clear-background raylib:+white+)
  (eon:scene3d-draw-simple (main-menu-screen-scene screen))
  (eon:scene2d-draw-simple (main-menu-screen-ui screen))
  (eon:scene2d-draw-simple (main-menu-screen-credit screen)))

(defmacro save-screen-excursion (&body body)
  (with-gensyms (screen)
    `(let ((,screen (eon:current-screen)))
       (prog1 (progn . ,body)
         (unless (eq (eon:current-screen) ,screen)
           (await (eon:promise-transition-screen ,screen)))))))

(defun main ()
  (catch 'exit
    (raylib:set-config-flags (cffi:foreign-bitfield-value 'raylib:config-flags '(:window-resizable :vsync-hint)))
    (raylib:with-window (+game-title+ (+viewport-width+ +viewport-height+))
      (raylib:set-target-fps 60)
      (eon:with-game-context
        (let ((screen (make-main-menu-screen)))
          (eon:scene2d-layout (main-menu-screen-ui screen))
          (setf (raylib:color-a (eon:scene2d-color (main-menu-ui-cell (main-menu-screen-ui screen)))) 0)
          (eon:scene2d-layout (main-menu-screen-credit screen))
          (raylib:copy-vector2
           (raylib:make-vector2 :x 0.0 :y #.(float +viewport-height+))
           (eon:scene2d-position (main-menu-screen-credit screen)))
          (setf (eon:current-screen) screen)
          (flet ((promise-display-credit ()
                   (async
                     (let* ((credit (main-menu-screen-credit screen))
                            (credit-src-y (raylib:vector2-y (eon:scene2d-position credit)))
                            (credit-dest-y (- (raylib:vector2-y (eon:scene2d-size credit)))))
                       (await (eon:promise-tween (ute:timeline
                                                  (:sequence
                                                   (:to (((raylib:vector2-y (eon:scene2d-position credit)))
                                                         (credit-dest-y))
                                                    :ease #'ute:linear-inout
                                                    :duration 20.0)
                                                   (:to (((raylib:vector2-y (eon:scene2d-position credit)))
                                                         (credit-src-y)))))))))))
            (async
              (loop :with max-level-reached := 1
                    :for index := (let ((color (eon:scene2d-color (main-menu-ui-cell (main-menu-screen-ui screen)))))
                                    (prog2 (await (eon:promise-tween
                                                   (ute:tween
                                                    :to (((eon:integer-float (raylib:color-a color))) (255.0))
                                                    :duration 0.5)))
                                        (await (eon:select-box-promise-index
                                                (main-menu-ui-select-box
                                                 (main-menu-screen-ui screen))
                                                (or index 0)))
                                      (await (eon:promise-tween
                                              (ute:tween
                                               :to (((eon:integer-float (raylib:color-a color))) (0.0))
                                               :duration 0.5)))))
                    :until (eql index 2)
                    :do (case index
                          (0 (when (save-screen-excursion
                                     (loop :for i :from (if (and (> max-level-reached 1)
                                                                 (await (promise-yes-or-no-p
                                                                         "CONFIRMATION" "Would you like to continue from the highest level you reached last time?"
                                                                         (main-menu-ui-group (main-menu-screen-ui screen)))))
                                                            max-level-reached 1)
                                             :to 2
                                           :do (maxf max-level-reached i)
                                           :always (await (promise-play-level i))))
                               (await (promise-display-credit))
                               (setf max-level-reached 1)))
                          (1 (await (promise-display-credit)))))
              (throw 'exit t))))
        (eon:do-screen-loop (eon:make-fit-viewport :width +viewport-width+ :height +viewport-height+))))))
