(in-package #:nano-towers)

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
                                         :all 2.0 :child (eon:scene2d-label :string "Title" :name label-title :style (eon:scene2d-label-style :color raylib:+red+)))
                                        (eon:scene2d-margin
                                         :all 2.0 :child (eon:scene2d-label :string "Message" :name label-message))
                                        (eon:scene2d-margin
                                         :all 2.0 :child (eon:select-box
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
        (await (select-box-promise-index (message-window-select-box window)))))))

(defun promise-yes-or-no-p (title message group)
  (let ((window (make-message-window :title title :message message :choices '("YES" "NO"))))
    (eon:scene2d-layout window)
    (async
      (with-popped-ui (group window)
        (eql (await (select-box-promise-index (message-window-select-box window))) 0)))))

(defun default-window-style ()
  (eon:scene2d-construct (eon:scene2d-window-style :background (eon:scene2d-rectangle :color (raylib:fade raylib:+black+ 0.5)))))

(defun default-label-style ()
  (eon:scene2d-construct (eon:scene2d-label-style :color raylib:+raywhite+ :shadow nil :outline raylib:+darkgray+)))

(defun default-select-box-label (string)
  (eon:scene2d-construct (eon:scene2d-margin :all 2.0 :child (eon:scene2d-label :string string :style (default-label-style)))))
