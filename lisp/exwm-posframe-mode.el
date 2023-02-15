(defvar exwm--main-frame-width (frame-width))
(defvar exwm--main-frame-height (frame-height))
(defvar exwm--main-frame-pixel-width (frame-pixel-width))
(defvar exwm--main-frame-pixel-height (frame-pixel-height))

(cl-defun exwm-posframe-show (buffer-or-name
                              &key
                              string
                              position
                              poshandler
                              poshandler-extra-info
                              width
                              height
                              max-width
                              max-height
                              min-width
                              min-height
                              x-pixel-offset
                              y-pixel-offset
                              left-fringe
                              right-fringe
                              border-width
                              border-color
                              internal-border-width
                              internal-border-color
                              font
                              foreground-color
                              background-color
                              respect-header-line
                              respect-mode-line
                              initialize
                              no-properties
                              keep-ratio
                              lines-truncate
                              override-parameters
                              timeout
                              refresh
                              accept-focus
                              hidehandler
                              refposhandler
                              &allow-other-keys)

  (let* ((position (or position (point)))
         (max-width (if (numberp max-width)
                        max-width
                      (frame-width)))
         (max-height (if (numberp max-height)
                         max-height
                       (frame-height)))
         (min-width (min (or min-width 1) max-width))
         (min-height (min (or min-height 1) max-height))
         (width (when width
                  (min (max width min-width) max-width)))
         (height (when height
                   (min (max height min-height) max-height)))
         (x-pixel-offset (or x-pixel-offset 0))
         (y-pixel-offset (or y-pixel-offset 0))
         ;;-----------------------------------------------------
         (buffer (get-buffer-create buffer-or-name))
         (parent-window (selected-window))
         (parent-window-top (window-pixel-top parent-window))
         (parent-window-left (window-pixel-left parent-window))
         (parent-window-width (window-pixel-width parent-window))
         (parent-window-height (window-pixel-height parent-window))
         (parent-frame (window-frame parent-window))
         (parent-frame-width (frame-pixel-width parent-frame))
         (parent-frame-height (frame-pixel-height parent-frame))
         (ref-position
          (when (functionp refposhandler)
            (ignore-errors
              (funcall refposhandler parent-frame))))
         (font-width (default-font-width))
         (font-height (with-current-buffer (window-buffer parent-window)
                        (posframe--get-font-height position)))
         (mode-line-height (window-mode-line-height))
         (minibuffer-height (window-pixel-height (minibuffer-window)))
         (header-line-height (window-header-line-height parent-window))
         (tab-line-height (if (functionp 'window-tab-line-height)
                              (window-tab-line-height)
                            0))
         (mouse-position (cdr (mouse-pixel-position)))
         (frame-resize-pixelwise t)
         posframe)

    (with-current-buffer buffer

      ;; Initialize
      (unless posframe--initialized-p
        (let ((func initialize))
          (when (functionp func)
            (funcall func)
            (setq posframe--initialized-p t))))

      ;; Create posframe
      (setq posframe
            (posframe--create-posframe
             buffer
             :position position
             :font font
             :parent-frame
             (unless ref-position
               parent-frame)
             :left-fringe left-fringe
             :right-fringe right-fringe
             :border-width border-width
             :border-color border-color
             :internal-border-width internal-border-width
             :internal-border-color internal-border-color
             :foreground-color foreground-color
             :background-color background-color
             :keep-ratio keep-ratio
             :lines-truncate lines-truncate
             :respect-header-line respect-header-line
             :respect-mode-line respect-mode-line
             :override-parameters override-parameters
             :accept-focus accept-focus))

      ;; Insert string into the posframe buffer
      (posframe--insert-string string no-properties)

      (let ((size-info
             (list :posframe posframe
                   :width width
                   :height height
                   :max-width max-width
                   :max-height max-height
                   :min-width min-width
                   :min-height min-height)))
        ;; Set posframe's size
        (posframe--set-frame-size size-info)
        ;; Re-adjust posframe's size when buffer's content has changed.
        (posframe--run-refresh-timer refresh size-info))

      ;; Get new position of posframe.
      (setq position
            (posframe-run-poshandler
             ;; All poshandlers will get info from this plist.
             `(,@poshandler-extra-info
               ,@(list :position position
                       :poshandler poshandler
                       :font-height font-height
                       :font-width font-width
                       :posframe posframe
                       :posframe-width (frame-pixel-width posframe)
                       :posframe-height (frame-pixel-height posframe)
                       :posframe-buffer buffer
                       :parent-frame parent-frame
                       :parent-frame-width parent-frame-width
                       :parent-frame-height parent-frame-height
                       :ref-position ref-position
                       :parent-window parent-window
                       :parent-window-top parent-window-top
                       :parent-window-left parent-window-left
                       :parent-window-width parent-window-width
                       :parent-window-height parent-window-height
                       :mouse-x (car mouse-position)
                       :mouse-y (cdr mouse-position)
                       :mode-line-height mode-line-height
                       :minibuffer-height minibuffer-height
                       :header-line-height header-line-height
                       :tab-line-height tab-line-height
                       :x-pixel-offset x-pixel-offset
                       :y-pixel-offset y-pixel-offset))))

      ;; Move posframe
      (posframe--set-frame-position
       posframe position parent-frame-width parent-frame-height)

      ;; Delay hide posframe when timeout is a number.
      (posframe--run-timeout-timer posframe timeout)

      ;; Make sure not hide buffer's content for scroll down.
      (let ((window (frame-root-window posframe--frame)))
        (when (window-live-p window)
          (set-window-point window 0)))

      ;; Hide posframe when switch buffer
      (let* ((parent-buffer (window-buffer parent-window))
             (parent-buffer-name (buffer-name parent-buffer)))
        (set-frame-parameter posframe--frame 'posframe-hidehandler hidehandler)
        (set-frame-parameter posframe--frame 'posframe-parent-buffer
                             (cons parent-buffer-name parent-buffer)))

      ;; Mouse banish
      (funcall
       posframe-mouse-banish-function
       (list :parent-frame parent-frame
             :mouse-x (when (car mouse-position)
                        (+ (or (car ref-position) 0)
                           (car mouse-position)))
             :mouse-y (when (cdr mouse-position)
                        (+ (or (cdr ref-position) 0)
                           (cdr mouse-position)))
             :posframe-x
             (if (>= (car position) 0)
                 (car position)
               (- (frame-pixel-width parent-frame)
                  (frame-pixel-width posframe)))
             :posframe-y
             (if (>= (cdr position) 0)
                 (cdr position)
               (- (frame-pixel-height parent-frame)
                  (frame-pixel-height posframe)))
             :posframe-width (frame-pixel-width posframe)
             :posframe-height (frame-pixel-height posframe)
             :parent-frame-width parent-frame-width
             :parent-frame-height parent-frame-height))

      ;; Return posframe
      posframe)))

(defun exwm-vertico-posframe-get-size (buffer)
  (list
   :width exwm--main-frame-width
   :max-height exwm--main-frame-height
   :max-width exwm--main-frame-width
   :min-height (or (buffer-local-value 'vertico-posframe-min-height buffer)
                   (let ((height (+ vertico-count 1)))
                     (min height (or (buffer-local-value 'vertico-posframe-height buffer) height))))
   :min-width exwm--main-frame-width))

(defun exwm-posframe-poshandler-frame-bottom-center (info)
  (cons (/ (- exwm--main-frame-pixel-width
              (plist-get info :posframe-width))
           2)
        (- exwm--main-frame-pixel-height
           (plist-get info :posframe-height)
           (plist-get info :mode-line-height)
           (plist-get info :minibuffer-height))))

(define-minor-mode exwm-posframe-mode
  "Auto hide exwm float windows."
  :global t
  (if exwm-posframe-mode
      (progn
        (advice-add 'posframe-show :override 'exwm-posframe-show)
        (advice-add 'vertico-posframe-get-size :override 'exwm-vertico-posframe-get-size)
        (advice-add 'posframe-poshandler-frame-bottom-center :override 'exwm-posframe-poshandler-frame-bottom-center))
    (progn
      (advice-remove 'posframe-show 'exwm-posframe-show)
      (advice-remove 'vertico-posframe-get-size 'exwm-vertico-posframe-get-size)
      (advice-remove 'posframe-poshandler-frame-bottom-center 'exwm-posframe-poshandler-frame-bottom-center))))

(provide 'exwm-posframe-mode)
