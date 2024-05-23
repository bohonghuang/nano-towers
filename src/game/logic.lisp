(in-package #:spring-lisp-game-jam-2024)

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
                                                            (eon:scene2d-margin :top 16.0 :child prompt-label)))))))
      (setf (raylib:color-a (eon:scene2d-color prompt-label)) 0)
      (eon:scene2d-layout cell)
      (setf (eon:current-screen) (lambda ()
                                   (raylib:clear-background raylib:+white+)
                                   (eon:scene2d-draw-simple screenshot-1)
                                   (eon:scene2d-draw-simple screenshot-2)
                                   (eon:scene2d-draw-simple cell)))
      (async
        (await (eon:promise-tween
                (ute:timeline
                 (:parallel
                  (:to (((eon:integer-float (raylib:color-a (eon:scene2d-color screenshot-2)))) (0.0))
                   :duration 0.5)
                  (:from (((raylib:vector2-y (eon:scene2d-position cell))) (#.(- (float +viewport-height+))))
                   :ease #'ute:bounce-out
                   :duration 1.0)))))
        (loop :initially (ute:start prompt-tween)
              :until (eq (await (eon:promise-pressed-key)) :a)
              :finally (ute:kill prompt-tween))))))

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
                                                 :layout (T 1)
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

(defun promise-wait-for-all-enemies-dead (context)
  (promise:with-promise (succeed)
    (eon:add-game-loop-hook
     (lambda ()
       (if (game-context-enemies context)
           (game-context-result context)
           (succeed)))
     :after #'not)))

(defun promise-play-level (&optional (level 1))
  #+sbcl (declare (sb-ext:muffle-conditions style-warning sb-ext:compiler-note))
  (let* ((map (tiled:load-map (game-asset (format nil "maps/level-~D.tmx" level))))
         (scene (make-instance 'game-scene :map-renderer (eon:tiled-map-renderer map)))
         (screen (make-game-scene-screen :scene scene))
         (ui (game-scene-screen-ui screen))
         (context (game-scene-context scene)))
    (eon:scene2d-layout (game-scene-screen-ui screen))
    (let ((paths (game-scene-map-enemy-paths map)))
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
      (labels ((promise-spawn-enemy-wave (wave path)
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
                                     :else :if (zerop enemy-desc-index)
                                             :do (let ((box (game-scene-ui-enemy-info-box ui))
                                                       (label (eon:scene2d-construct (eon:scene2d-label :string ""))))
                                                   (async
                                                     (loop :initially (eon:scene2d-box-add-child box label)
                                                           :for time :from (floor interval) :downto 1
                                                           :do (setf (eon:scene2d-label-string label) (format nil "Time Remaining for Next Wave of Enemies: ~Ds" time))
                                                               (eon:scene2d-layout (game-scene-ui-cell-enemy-info ui))
                                                               (await (eon:promise-sleep 1.0))
                                                           :finally
                                                              (eon:scene2d-box-remove-child box label)
                                                              (eon:scene2d-layout (game-scene-ui-cell-enemy-info ui)))))
                                     :do (await (eon:promise-sleep interval)))))))
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
          (let ((focus-manager (loop :for cell :in (tiled:layer-cells (find "ground" (tiled:map-layers map) :key #'tiled:layer-name :test #'string=))
                                     :when (gethash "base" (tiled:properties (tiled:cell-tile cell)))
                                       :do (push (make-game-scene-tower
                                                  :scene scene
                                                  :position (position-2d->3d
                                                             (raylib:make-vector2
                                                              :x (+ (coerce (tiled:cell-column cell) 'single-float) 0.5)
                                                              :y (+ (coerce (tiled:cell-row cell) 'single-float) 0.5))))
                                                 (game-context-towers context))
                                       :and :collect (eon::make-scene2d-focusable
                                                      :focus-point (raylib:make-vector2
                                                                    :x (coerce (tiled:cell-column cell) 'single-float)
                                                                    :y (coerce (tiled:cell-row cell) 'single-float))
                                                      :content (first (game-context-towers context)))
                                              :into focusables
                                     :finally (return (eon:make-scene2d-focus-manager :focusables focusables))))
                (selected-tower nil))
            (flet ((unselect-tower (tower)
                     (setf (game-scene-tower-selectedp tower) nil))
                   (select-tower (tower)
                     (setf (game-scene-tower-selectedp (setf selected-tower tower)) t)
                     (basic-scene-look-at scene (game-scene-tower-position tower)))
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
                (loop :with ui-group := (game-scene-ui-group (game-scene-screen-ui screen))
                      :for key := (await (eon:promise-pressed-key))
                      :until (game-context-result context)
                      :do (case key
                            ((:left :right :up :down)
                             (unselect-tower (eon::scene2d-focusable-content (eon:scene2d-focus-manager-focused focus-manager)))
                             (eon:scene2d-focus-manager-handle-key focus-manager key)
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
                                      (let* ((tower-select-box
                                               (let ((table (eon:scene2d-construct (eon:scene2d-table :orientation :horizontal))))
                                                 (dolist (tower-type *tower-types*)
                                                   (destructuring-bind (type &key cost model &allow-other-keys) tower-type
                                                     (declare (ignore type))
                                                     (eon:scene2d-table-newline table)
                                                     (let ((cell (eon:scene2d-table-add-child
                                                                  table
                                                                  (eon:scene2d-construct
                                                                   (eon:scene2d-image
                                                                    :drawable (eon:make-texture-region
                                                                               :texture (eon:load-asset 'raylib:texture (game-asset (format nil "models/towers/~A.png" (assoc-value model 1))))))))))
                                                       (setf (eon::scene2d-alignment-vertical
                                                              (eon::scene2d-cell-alignment cell))
                                                             :end))
                                                     (eon:scene2d-table-add-child
                                                      table
                                                      (eon:scene2d-construct
                                                       (eon:scene2d-margin
                                                        :left 2.0 :right 2.0 :top 2.0 :bottom 2.0
                                                        :child (eon:scene2d-label :string (format nil "$ ~D" (assoc-value cost 1))))))))
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
                                                    (game-scene-tower-update selected-tower))
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
                                                (game-scene-tower-update selected-tower))
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
                                          (game-scene-tower-update selected-tower))))
                                     (cancel))))))))
                (when (eq (game-context-result context) :failure)
                  (await (eon:promise-sleep 2.0))
                  (await (promise-confirm-game-over)))
                (loop :for enemy :in (game-context-enemies context)
                      :do (setf (game-scene-enemy-active-animation enemy) nil))
                (setf (game-context-enemies context) nil
                      (game-context-objects context) nil
                      (game-context-towers context) nil)
                (eq (game-context-result context) :success)))))))))
