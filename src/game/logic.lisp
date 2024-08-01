(in-package #:nano-towers)

(defun promise-confirm-game-over ()
  (let* ((screenshot-1 (eon:take-screenshot))
         (screenshot-2 (eon:load-asset 'raylib:image screenshot-1)))
    (raylib:image-blur-gaussian screenshot-1 4)
    (raylib:image-color-grayscale screenshot-1)
    (let* ((screenshot-1 (eon:ensure-scene2d-node screenshot-1))
           (screenshot-2 (eon:ensure-scene2d-node screenshot-2))
           (prompt-label (eon:scene2d-construct (eon:scene2d-label :string "Press A to continue." 
                                                                   :style (eon:scene2d-label-style
                                                                           :text-style (eon:text-style :size 10.0 :spacing 2.0)
                                                                           :color raylib:+raywhite+
                                                                           :shadow nil :outline raylib:+darkgray+))))
           (prompt-tween (ute:tween
                          :to (((eon:integer-float (raylib:color-a (eon:scene2d-color prompt-label)))) (255.0))
                          :repeat (:count t :yoyop t)
                          :duration 0.5))
           (cell (eon:scene2d-construct (eon:scene2d-cell
                                         :size (#.(float +viewport-width+) #.(float +viewport-height+))
                                         :child (eon:scene2d-box
                                                 :orientation :vertical
                                                 :children ((eon:scene2d-label :string "Game Over" :style (eon:scene2d-label-style
                                                                                                           :text-style (eon:text-style :size 64.0 :spacing 8.0)
                                                                                                           :color raylib:+raywhite+
                                                                                                           :shadow nil :outline raylib:+darkgray+))
                                                            (eon:scene2d-margin :top 16.0 :child prompt-label))))))
           (bgm (eon:load-asset 'raylib:music (game-asset #P"audio/game-over.ogg"))))
      (setf (raylib:music-looping bgm) t
            (eon:audio-volume bgm) 1.0)
      (setf (raylib:color-a (eon:scene2d-color prompt-label)) 0)
      (eon:scene2d-layout cell)
      (setf (eon:current-screen) (lambda ()
                                   (raylib:clear-background raylib:+white+)
                                   (eon:scene2d-draw-simple screenshot-1)
                                   (eon:scene2d-draw-simple screenshot-2)
                                   (eon:scene2d-draw-simple cell)))
      (async
        (eon:play-audio bgm)
        (await (eon:promise-tween
                (ute:timeline
                 (:parallel
                  (:to (((eon:integer-float (raylib:color-a (eon:scene2d-color screenshot-2)))) (0.0))
                   :duration 0.5)
                  (:from (((raylib:vector2-y (eon:scene2d-position cell))) (#.(- (float +viewport-height+))))
                   :ease #'ute:bounce-out
                   :duration 1.0)))))
        (loop :initially (ute:start prompt-tween)
              :until (eq (await (eon:promise-pressed-controller-button)) :a)
              :finally (ute:kill prompt-tween))
        (await (eon:promise-fade-audio bgm 0.0))
        (eon:stop-audio bgm)))))

(defun promise-wait-for-all-enemies-dead (context)
  (promise:with-promise (succeed)
    (eon:add-game-loop-hook
     (lambda ()
       (if (game-context-enemies context)
           (when (game-context-result context)
             (succeed))
           (succeed)))
     :after #'not)))

(defun make-tower-upgrade-emitter (tower)
  (let ((emitter (eon:make-scene3d-particle-emitter
                  :rate 0.0
                  :capacity 512
                  :position (raylib:copy-vector3 (game-scene-tower-position tower))
                  :updater (lambda (position)
                             (let ((origin (raylib:make-vector3))
                                   (bounding-box (eon:scene3d-bound (game-scene-tower-model tower)))
                                   (bounding-box-scale (raylib:vector3-scale (raylib:vector3-one) 1.25))
                                   (acceleration (raylib:make-vector3)))
                               (lambda (particle)
                                 (if (zerop (eon:particle-3d-age particle))
                                     (progn
                                       (eon:particle-3d-initialize-default
                                        particle (bounding-box-sample bounding-box (raylib:copy-vector3 position origin) bounding-box-scale))
                                       (setf (eon:particle-3d-lifetime particle) 1.0))
                                     (progn
                                       (setf (raylib:vector3-x acceleration) (- 5.0 (random 10.0))
                                             (raylib:vector3-y acceleration) (if (< (eon:particle-3d-age particle) 0.25)
                                                                                 (- 5.0 (random 10.0))
                                                                                 5.0)
                                             (raylib:vector3-z acceleration) (- 5.0 (random 10.0))
                                             (eon:particle-3d-acceleration particle) acceleration)
                                       (eon:particle-3d-update-motion particle))))))
                  :renderer (eon:particle-3d-cube-renderer 0.15 (eon:particle-3d-interpolate-color-over-age (raylib:fade raylib:+white+ 0.0) raylib:+white+ #'eon::sine-yoyo-2)))))
    (eon:scene3d-particle-emitter-burst emitter 512)
    emitter))

(defun make-tower-demolish-emitter (tower)
  (let ((emitter (eon:make-scene3d-particle-emitter
                  :rate 0.0
                  :capacity 512
                  :position (raylib:copy-vector3 (game-scene-tower-position tower))
                  :updater (lambda (position)
                             (let ((origin (raylib:make-vector3))
                                   (bounding-box (eon:scene3d-bound (game-scene-tower-model tower)))
                                   (bounding-box-scale (raylib:vector3-scale (raylib:vector3-one) 1.25))
                                   (acceleration (raylib:make-vector3)))
                               (lambda (particle)
                                 (if (zerop (eon:particle-3d-age particle))
                                     (progn
                                       (eon:particle-3d-initialize-default
                                        particle (bounding-box-sample bounding-box (raylib:copy-vector3 position origin) bounding-box-scale))
                                       (setf (eon:particle-3d-lifetime particle) 1.5))
                                     (progn
                                       (setf (raylib:vector3-x acceleration) (- 5.0 (random 10.0))
                                             (raylib:vector3-y acceleration) (if (< (eon:particle-3d-age particle) 0.25)
                                                                                 (- 5.0 (random 10.0))
                                                                                 -10.0)
                                             (raylib:vector3-z acceleration) (- 5.0 (random 10.0))
                                             (eon:particle-3d-acceleration particle) acceleration)
                                       (eon:particle-3d-update-motion particle)
                                       (particle-3d-bounce particle))))))
                  :renderer (eon:particle-3d-cube-renderer 0.15 (eon:particle-3d-interpolate-color-over-age (raylib:fade raylib:+white+ 0.0) raylib:+white+ #'eon::sine-yoyo-2)))))
    (eon:scene3d-particle-emitter-burst emitter 512)
    emitter))

(defun make-tower-info-window (tower-type &optional (level 1))
  (eon:scene2d-construct
   (eon:scene2d-window
    :child (eon:scene2d-margin
            :all 4.0
            :child (flet ((property (name)
                            (let ((result (getf tower-type name)))
                              (etypecase result
                                (list (assoc-value result level))
                                (atom result)))))
                     (loop :with table := (eon:scene2d-construct (eon:scene2d-table))
                           :with rate := (property :rate)
                           :for name :in '("Attack Power:" "Attack Rate:" "Attack Radius:")
                           :for key :in '(:power :rate :radius)
                           :for value := (property key)
                           :do (eon:scene2d-table-newline table)
                               (let ((cell (eon:scene2d-table-add-child
                                            table
                                            (eon:scene2d-construct
                                             (eon:scene2d-label :string name :style (default-label-style))))))
                                 (setf (eon::scene2d-alignment-horizontal (eon::scene2d-cell-alignment cell)) :start))
                               (eon:scene2d-table-add-child table (eon:scene2d-construct (eon:scene2d-cell :size (8.0 0.0))))
                               (let ((cell (eon:scene2d-table-add-child
                                            table
                                            (eon:scene2d-construct
                                             (eon:scene2d-label
                                              :string (if (eql rate t)
                                                          (case key
                                                            (:rate "Continuous")
                                                            (:power (format nil "~A/s" value))
                                                            (t (princ-to-string value)))
                                                          (princ-to-string value))
                                              :style (default-label-style))))))
                                 (setf (eon::scene2d-alignment-horizontal (eon::scene2d-cell-alignment cell)) :end))
                           :finally (return table))))
    :style (default-window-style))))

(defun promise-play-level (&optional (level 1))
  #+sbcl (declare (sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
  (let* ((map (tiled:load-map (game-asset (format nil "maps/level-~D.tmx" level))))
         (scene (make-instance 'game-scene :map-renderer (eon:tiled-map-renderer map)))
         (screen (make-game-scene-screen :scene scene))
         (ui (game-scene-screen-ui screen))
         (ui-group (game-scene-ui-group ui))
         (audio (game-scene-audio scene))
         (context (game-scene-context scene)))
    (eon:scene2d-layout (game-scene-screen-ui screen))
    (let ((paths (game-scene-map-enemy-paths map))
          (wait-cancelers nil)
          (tower-info-window nil))
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
                                              (unless (find billboard (game-context-objects context))
                                                (ute:kill timeline))))
                                     :repeat t))))
                  (ute:start timeline))
                (push billboard (game-context-objects context)))
      (labels ((show-tower-info-window (&rest args)
                 (setf tower-info-window (eon:scene2d-construct
                                          (eon:scene2d-cell
                                           :size (0.0 0.0)
                                           :alignment (:end :start)
                                           :position (#.(/ +viewport-width+ 2.0) #.(/ +viewport-height+ 2.0))
                                           :child (apply #'make-tower-info-window args))))
                 (eon:scene2d-layout tower-info-window)
                 (eon:scene2d-group-add-child ui-group tower-info-window))
               (hide-tower-info-window ()
                 (eon:scene2d-group-remove-child ui-group (shiftf tower-info-window nil)))
               (promise-display-countdown (position interval wait-canceler)
                 (let* ((label (eon:scene2d-construct (eon:scene2d-label :string "" :style (default-label-style))))
                        (cell (eon:scene2d-construct (eon:scene2d-cell
                                                      :child (eon:scene2d-window
                                                              :child (eon:scene2d-margin :all 2.0 :child label)
                                                              :style (default-window-style))))))
                   (eon:add-game-loop-hook
                    (lambda ()
                      (cobj:with-monotonic-buffer-allocator ()
                        (let ((position (raylib:get-world-to-screen-ex position (basic-scene-camera scene) +viewport-width+ +viewport-height+))
                              (alignment (eon::scene2d-cell-alignment cell)))
                          (setf (eon::scene2d-alignment-horizontal alignment)
                                (cond
                                  ((< (raylib:vector2-x position) 0.0)
                                   (maxf (raylib:vector2-x position) 0.0)
                                   :start)
                                  ((> (raylib:vector2-x position) #.(float +viewport-width+))
                                   (minf (raylib:vector2-x position) #.(float +viewport-width+))
                                   :end)
                                  (t :center))
                                (eon::scene2d-alignment-vertical alignment)
                                (cond
                                  ((< (raylib:vector2-y position) 0.0)
                                   (maxf (raylib:vector2-y position) 0.0)
                                   :start)
                                  ((> (raylib:vector2-y position) #.(float +viewport-height+))
                                   (minf (raylib:vector2-y position) #.(float +viewport-height+))
                                   :end)
                                  (t :center)))
                          (raylib:copy-vector2 position (eon:scene2d-position cell))
                          (eon:scene2d-layout cell)))
                      (member wait-canceler wait-cancelers))
                    :after #'identity)
                   (async
                     (loop :initially (eon:scene2d-group-add-child ui-group cell)
                           :for time :from (floor interval) :downto 1
                           :do (setf (eon:scene2d-label-string label) (format nil "~Ds" time))
                               (eon:scene2d-layout cell)
                               (await (eon:promise-sleep 1.0))
                           :while (member wait-canceler wait-cancelers)
                           :finally (eon:scene2d-group-remove-child ui-group cell)))))
               (promise-spawn-enemy-wave (wave path)
                 (async
                   (loop :for enemy-desc :in wave
                         :for enemy-desc-index :from 0
                         :do (destructuring-bind (type &key (interval 1.0) (count 1) (level 1)) enemy-desc
                               (loop :repeat count
                                     :until (game-context-result context)
                                     :if type
                                       :do (push
                                            (let ((enemy (make-game-scene-enemy
                                                          :scene scene
                                                          :position (raylib:copy-vector3 (first path))
                                                          :type type
                                                          :level level
                                                          :path (rest path))))
                                              (eon:add-game-loop-hook (game-scene-enemy-updater enemy) :after #'identity)
                                              (setf (game-scene-enemy-active-animation enemy) (game-scene-enemy-find-animation enemy :idle))
                                              enemy)
                                            (game-context-enemies context))
                                     :do (if (and (null type) (zerop enemy-desc-index))
                                             (let ((promise (promise:make)))
                                               (letrec ((wait-canceler (lambda () (promise:succeed promise) (deletef wait-cancelers wait-canceler))))
                                                 (promise-display-countdown (raylib:copy-vector3 (first path)) interval wait-canceler)
                                                 (push wait-canceler wait-cancelers)
                                                 (await (aselect (eon:promise-sleep interval) promise))
                                                 (unless (promise:done-p promise)
                                                   (promise:succeed promise))
                                                 (when (member wait-canceler wait-cancelers)
                                                   (funcall wait-canceler))))
                                             (await (eon:promise-sleep interval))))))))
               (promise-spawn-enemies ()
                 (async
                   (loop :with wave-count := (reduce #'max paths :key (compose #'length #'cdr))
                         :initially (setf (eon:scene2d-label-string (game-scene-ui-label-wave-count ui)) (princ-to-string wave-count))
                         :for wave-index :below wave-count
                         :do (setf (eon:scene2d-label-string (game-scene-ui-label-wave-number ui)) (princ-to-string (1+ wave-index)))
                             (eon:scene2d-layout (game-scene-ui-cell-enemy-info ui))
                             (await (apply #'ajoin (loop :for (path . waves-desc) :in paths :collect (promise-spawn-enemy-wave (nth wave-index waves-desc) path))))
                             (await (promise-wait-for-all-enemies-dead context)))
                   (unless (game-context-result context)
                     (setf (game-context-result context) :success)
                     (await (promise-cancel-all-input))))))
        (with-accessors ((money game-context-money)) context
          (setf money (gethash "money" (tiled:properties map)))
          (let ((bgm (eon:load-asset 'raylib:music (merge-pathnames (or (gethash "music" (tiled:properties map)) "level.ogg") (game-asset #P"audio/"))))
                (focus-manager (loop :for cell :in (tiled:layer-cells (find "ground" (tiled:map-layers map) :key #'tiled:layer-name :test #'string=))
                                     :when (gethash "base" (tiled:properties (tiled:cell-tile cell)))
                                       :do (push (make-game-scene-tower
                                                  :scene scene
                                                  :position (position-2d->3d
                                                             (raylib:make-vector2
                                                              :x (+ (coerce (tiled:cell-column cell) 'single-float) 0.5)
                                                              :y (+ (coerce (tiled:cell-row cell) 'single-float) 0.5))))
                                                 (game-context-towers context))
                                       :and :collect (eon::make-scene2d-focusable
                                                      :focal-bound (cons #1=(raylib:make-vector2
                                                                             :x (coerce (tiled:cell-column cell) 'single-float)
                                                                             :y (coerce (tiled:cell-row cell) 'single-float))
                                                                         #1#)
                                                      :content (first (game-context-towers context)))
                                              :into focusables
                                     :finally (return (eon:make-scene2d-focus-manager :focusables focusables))))
                (selected-tower nil))
            (setf (raylib:music-looping bgm) t
                  (eon:audio-volume bgm) 1.0)
            (eon:play-audio bgm)
            (flet ((unselect-tower (tower)
                     (setf (game-scene-tower-selectedp tower) nil))
                   (select-tower (tower)
                     (setf (game-scene-tower-selectedp (setf selected-tower tower)) t)
                     (basic-scene-promise-look-at scene (game-scene-tower-position tower)))
                   (tower-screen-position (&optional (tower selected-tower))
                     (raylib:get-world-to-screen-ex
                      (game-scene-tower-position tower)
                      (basic-scene-camera scene)
                      +viewport-width+ +viewport-height+)))
              (select-tower (lastcar (game-context-towers context)))
              (async
                (setf (raylib:color-a (eon:scene2d-color (game-scene-ui-label-level ui))) 0
                      (eon:scene2d-label-string (game-scene-ui-label-level ui)) (format nil "Level ~D" level))
                (await (eon:promise-transition-screen screen))
                (await (eon:promise-tween
                        (let ((position (eon:scene2d-position (game-scene-ui-label-level ui)))
                              (color (eon:scene2d-color (game-scene-ui-label-level ui))))
                          (ute:timeline
                           (:sequence
                            (:to (((eon:integer-float (raylib:color-a color))) (255.0)))
                            (:from (((raylib:vector2-y position)) (#.(float +viewport-height+)))
                             :duration 1.0 :relativep t :ease #'ute:elastic-out)
                            (:pause 0.5)
                            (:to (((eon:integer-float (raylib:color-a color))) (0.0))
                             :duration 0.5))))))
                (promise-spawn-enemies)
                (loop :with sfx := (eon:load-asset 'raylib:sound (game-asset #P"audio/click-grid.wav"))
                      :for button := (prog2 (when-let ((tower-type (game-scene-tower-type selected-tower)))
                                              (show-tower-info-window (assoc-value *tower-types* tower-type) (game-scene-tower-level selected-tower)))
                                         (await (eon:promise-pressed-controller-button))
                                       (hide-tower-info-window))
                      :until (game-context-result context)
                      :do (play-sfx sfx)
                          (case button
                            ((:left :right :up :down)
                             (unselect-tower (eon::scene2d-focusable-content (eon:scene2d-focus-manager-focused focus-manager)))
                             (eon:scene2d-focus-manager-handle-input focus-manager button)
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
                                                  :entries (mapcar (compose
                                                                    (lambda (name)
                                                                      (eon:scene2d-construct
                                                                       (eon:scene2d-margin :all 2.0 :child (eon:scene2d-label :string name))))
                                                                    #'symbol-name)
                                                                   operations))))
                                    (operation-selector (eon:scene2d-construct
                                                         (eon:scene2d-window
                                                          :child (eon:scene2d-margin :all 1.0 :child select-box)))))
                               (raylib:copy-vector2
                                (tower-screen-position)
                                (eon:scene2d-position operation-selector))
                               (eon:scene2d-layout operation-selector)
                               (with-popped-ui (ui-group operation-selector)
                                 (when-let ((index (await (select-box-promise-index select-box))))
                                   (ecase (nth index operations)
                                     (build
                                      (let* ((tower-select-box
                                               (let ((table (eon:scene2d-construct (eon:scene2d-table :orientation :horizontal))))
                                                 (dolist (tower-type *tower-types*)
                                                   (destructuring-bind (type &key cost model &allow-other-keys) tower-type
                                                     (declare (ignore type))
                                                     (eon:scene2d-table-newline table)
                                                     (let ((cell (eon:scene2d-table-add-child
                                                                  table
                                                                  (eon:scene2d-construct
                                                                   (eon:scene2d-margin
                                                                    :all 2.0
                                                                    :child (eon:scene2d-image
                                                                            :drawable (eon:make-texture-region
                                                                                       :texture (eon:load-asset 'raylib:texture (game-asset (format nil "models/towers/~A.png" (assoc-value model 1)))))))))))
                                                       (setf (eon::scene2d-alignment-vertical
                                                              (eon::scene2d-cell-alignment cell))
                                                             :end))
                                                     (eon:scene2d-table-add-child
                                                      table
                                                      (eon:scene2d-construct
                                                       (eon:scene2d-margin
                                                        :all 2.0 :child (eon:scene2d-label :string (format nil "$ ~D" (assoc-value cost 1))))))))
                                                 (eon:table-select-box table)))
                                             (tower-selector (eon:scene2d-construct
                                                              (eon:scene2d-window :child tower-select-box))))
                                        (eon:scene2d-layout tower-selector)
                                        (raylib:copy-vector2
                                         (tower-screen-position)
                                         (eon:scene2d-position tower-selector))
                                        (with-popped-ui (ui-group tower-selector)
                                          (when-let ((index (await (select-box-promise-index
                                                                    tower-select-box 0
                                                                    (lambda (manager &optional key)
                                                                      (if key
                                                                          (hide-tower-info-window)
                                                                          (show-tower-info-window
                                                                           (cdr
                                                                            (nth
                                                                             (position
                                                                              (eon:scene2d-focus-manager-focused manager)
                                                                              (eon:select-box-entries tower-select-box))
                                                                             *tower-types*)))))))))
                                            (destructuring-bind (type &key cost &allow-other-keys) (nth index *tower-types*)
                                              (setf cost (assoc-value cost 1))
                                              (if (<= cost money)
                                                  (progn
                                                    (decf money cost)
                                                    (setf (game-scene-tower-type selected-tower) type
                                                          (game-scene-tower-level selected-tower) 1)
                                                    (game-scene-tower-update selected-tower)
                                                    (let ((position (game-scene-tower-position selected-tower)))
                                                      (setf (game-scene-tower-type selected-tower) nil)
                                                      (play-sfx (game-scene-audio-build audio))
                                                      (await
                                                       (eon:promise-tween
                                                        (ute:tween
                                                         :from (((raylib:vector3-y position)) (10.0))
                                                         :duration 1.0
                                                         :ease #'ute:bounce-out)))
                                                      (setf (game-scene-tower-type selected-tower) type)))
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
                                                (play-sfx (game-scene-audio-upgrade audio))
                                                (let ((emitter (make-tower-upgrade-emitter selected-tower)))
                                                  (push emitter (game-context-objects context))
                                                  (await (eon:promise-sleep 0.5))
                                                  (game-scene-tower-update selected-tower)
                                                  (await (eon:promise-sleep 0.5))
                                                  (deletef (game-context-objects context) emitter)))
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
                                          (play-sfx (game-scene-audio-demolish audio))
                                          (let ((emitter (make-tower-demolish-emitter selected-tower)))
                                            (push emitter (game-context-objects context))
                                            (await (eon:promise-sleep 0.5))
                                            (game-scene-tower-update selected-tower)
                                            (await (eon:promise-sleep 1.0))
                                            (deletef (game-context-objects context) emitter)))))
                                     (cancel))))))
                            (:b (when wait-cancelers
                                  (when (await (promise-yes-or-no-p "CONFIRMATION" "Do you want the next wave of enemies to come now?" ui-group))
                                    (mapc #'funcall wait-cancelers)))))
                      :finally (eon:unload-asset sfx))
                (await (eon:promise-fade-audio bgm 0.0))
                (eon:stop-audio bgm)
                (when (eq (game-context-result context) :failure)
                  (await (eon:promise-sleep 1.5))
                  (await (promise-cancel-all-input))
                  (await (promise-confirm-game-over)))
                (loop :for enemy :in (game-context-enemies context)
                      :do (setf (game-scene-enemy-active-animation enemy) nil))
                (setf (game-context-enemies context) nil
                      (game-context-objects context) nil
                      (game-context-towers context) nil)
                (eq (game-context-result context) :success)))))))))
