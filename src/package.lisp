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
                                         :children ((eon:scene2d-label :string "Money: ")
                                                    (eon:scene2d-label :string "0" :name label-money)))))))))

(defstruct game-scene-screen
  (camera (raylib:copy-camera +camera-default+) :type raylib:camera-3d)
  (map-renderer (eon:tiled-map-renderer (tiled:load-map (game-asset #P"maps/map-1.tmx"))) :type eon:tiled-renderer)
  (tower-base-focus-manager (eon:make-scene2d-focus-manager) :type eon:scene2d-focus-manager)
  (ui (make-game-scene-ui) :type eon:scene2d-constructed)
  (-money 0 :type non-negative-fixnum)
  (towers nil :type list)
  (enemies nil :type list)
  (attacks nil :type list))

(defun game-scene-screen-money (screen)
  (game-scene-screen--money screen))

(defun (setf game-scene-screen-money) (value screen)
  (setf (eon:scene2d-label-string (game-scene-ui-label-money (game-scene-screen-ui screen))) (princ-to-string value))
  (eon:scene2d-layout (game-scene-screen-ui screen))
  (setf (game-scene-screen--money screen) value))

(defmethod eon:screen-render ((screen game-scene-screen))
  (raylib:clear-background raylib:+white+)
  (let ((eon:*scene3d-camera* (game-scene-screen-camera screen)))
    (raylib:with-mode-3d eon:*scene3d-camera*
      (rlgl:push-matrix)
      (rlgl:rotatef 90.0 1.0 0.0 0.0)
      (rlgl:scalef (/ (coerce +tile-width+ 'single-float)) (/ (coerce +tile-height+ 'single-float)) 0.0)
      (funcall (game-scene-screen-map-renderer screen))
      (rlgl:pop-matrix)
      (eon:scene3d-draw-simple (game-scene-screen-towers screen))
      (eon:scene3d-draw-simple (game-scene-screen-enemies screen))
      (eon:scene3d-draw-simple (game-scene-screen-attacks screen))))
  (eon:scene2d-draw-simple (game-scene-screen-ui screen)))

(defstruct (game-scene-tower (:include eon:scene3d-container)
                             (:constructor %make-game-scene-tower))
  (selectedp nil :type boolean)
  (type nil :type symbol)
  (level 0 :type (integer 0 3))
  (target nil :type t))

(defparameter *tower-types*
  '((:round-1 :cost ((1 . 1000) (2 . 2000) (3 . 4000))
              :radius ((1 . 2.0) (2 . 3.0) (3 . 4.0))
              :power ((1 . 10) (2 . 15) (3 . 20))
              :model ((1 . "towerRound_sampleA")
                      (2 . "towerRound_sampleC")
                      (3 . "towerRound_sampleE")))
    (:round-2 :cost ((1 . 2000) (2 . 4000) (3 . 8000))
              :radius ((1 . 3.0) (2 . 4.0) (3 . 5.0))
              :power ((1 . 15) (2 . 25) (3 . 35))
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

(defun game-scene-tower-model-update-model (tower)
  (setf (game-scene-tower-model tower)
        (destructuring-bind (&key model)
            (game-scene-tower-type-level-asset
             (game-scene-tower-type tower)
             (game-scene-tower-level tower))
          (when model (eon:load-asset 'raylib:model model)))))

(defmethod eon:scene3d-draw ((tower game-scene-tower) position origin scale rotation tint)
  (when (game-scene-tower-selectedp tower)
    (raylib:draw-cube position 1.0 0.25 1.0 (raylib:fade raylib:+white+ 0.5)))
  (call-next-method))

(defstruct (game-scene-enemy (:include eon:scene3d-container)
                             (:constructor %make-game-scene-enemy))
  (type :slime :type symbol)
  (speed 0.5 :type single-float)
  (path nil)
  (animations nil :type list)
  (hp 10.0 :type single-float)
  (total-hp 10.0 :type single-float)
  (hp-bar nil :type (or eon:scene3d-node null)))

(defstruct (game-scene-enemy-hp-bar (:include eon:scene2d-layout)
                                    (:constructor %make-game-scene-enemy-hp-bar))
  (target nil :type game-scene-enemy))

(defmethod eon:scene2d-draw ((bar game-scene-enemy-hp-bar) position origin scale rotation tint)
  (let ((target (game-scene-enemy-hp-bar-target bar)))
    (clet ((size (raylib:vector2-scale
                  (game-scene-enemy-hp-bar-size bar)
                  (/ (game-scene-enemy-hp target) (game-scene-enemy-total-hp target)))))
      (declare (dynamic-extent size))
      (setf (raylib:vector2-y size) (raylib:vector2-y (game-scene-enemy-hp-bar-size bar)))
      (raylib:draw-rectangle-v position (game-scene-enemy-hp-bar-size bar) raylib:+gray+)
      (raylib:draw-rectangle-v position size raylib:+green+))))

(defun make-game-scene-enemy-hp-bar (&key target)
  (%make-game-scene-enemy-hp-bar :target target :size (raylib:make-vector2 :x 128.0 :y 12.0)))

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
  (call-next-method)
  (rlgl:draw-render-batch-active)
  (with-accessors ((hp-bar game-scene-enemy-hp-bar)) enemy
    (unless hp-bar
      (setf hp-bar (eon:ensure-scene3d-node
                    (make-game-scene-enemy-hp-bar :target enemy)
                    :scale (raylib:vector3-scale (raylib:vector3-one) (/ 128.0))
                    :up (raylib:vector3-cross-product
                         (raylib:get-camera-right eon:*scene3d-camera*)
                         (raylib:get-camera-forward eon:*scene3d-camera*)))))
    (rlgl:disable-depth-test)
    (eon:scene3d-draw-simple hp-bar :position position)
    (rlgl:draw-render-batch-active)
    (rlgl:enable-depth-test)))

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
                                                 :dimensions (T 1)
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
  (raylib:with-window ("Spring Lisp Game Jam 2024" ((* +viewport-width+ 2) (* +viewport-height+ 2)))
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
                :finally (setf (game-scene-screen-tower-base-focus-manager screen) (eon:make-scene2d-focus-manager :focusables focusables)))
          (let ((selected-tower nil))
            (flet ((unselect-tower (tower)
                     (setf (game-scene-tower-selectedp tower) nil))
                   (select-tower (tower)
                     (setf (game-scene-tower-selectedp (setf selected-tower tower)) t)
                     (game-scene-camera-look-at (game-scene-screen-camera screen) (game-scene-tower-position tower)))
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
                                                   ((< (game-scene-tower-level selected-tower) 3) '(upgrade demolish)))
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
                                                    (game-scene-tower-model-update-model selected-tower))
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
                                                (game-scene-tower-model-update-model selected-tower))
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
                                          (game-scene-tower-model-update-model selected-tower))))
                                     (cancel))))))))))))
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
