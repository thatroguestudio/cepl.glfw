;;;; cepl.glfw.lisp

(in-package #:cepl.glfw)

(declaim (optimize (debug 3)))
;;======================================================================
;; api v1

(defgeneric glfw-init (&rest init-flags)
  (:method (&rest init-flags)
    (declare (ignore init-flags))
    (glfw:initialize)))

;;----------------------------------------------------------------------

(defun glfw-shutdown ()
  (when glfw:*window* (glfw:set-window-should-close))
  (glfw:terminate)
  t)

;;----------------------------------------------------------------------

(defparameter *listeners* '())
(defparameter *cached-events* '())

(defun glfw-register-listener (func)
  (unless (find func *listeners*)
    (push func *listeners*)))

(defun glfw-step-v1 (surface)
  (declare (ignore surface))
  (setf *cached-events* (reverse *cached-events*))
  (loop :for event := (pop *cached-events*)
        :while event
        :do (loop :for listener :in *listeners*
                  :do (funcall listener event))))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window))
  (push (list :key key scancode action mod-keys) *cached-events*))

(glfw:def-framebuffer-size-callback framebuffer-size-callback (window w h)
  (declare (ignore window))
  (push (list :framebuffer-size w h) *cached-events*))
    


;;----------------------------------------------------------------------

(defun glfw-swap (handle)
  (glfw:swap-buffers handle)
  (glfw:poll-events))

;;----------------------------------------------------------------------

(defun make-glfw-context (surface version double-buffer
                          alpha-size depth-size stencil-size buffer-size
                          red-size green-size blue-size)

  (declare (ignore version double-buffer
                   alpha-size depth-size stencil-size buffer-size
                   red-size green-size blue-size))
  surface)

(defvar *core-context* t)

(defun glfw-make-current (context surface)
  (declare (ignore context))
  (glfw:make-context-current surface))

;;----------------------------------------------------------------------

(defun make-glfw-surface (width height title fullscreen
                          no-frame alpha-size depth-size stencil-size
                          red-size green-size blue-size buffer-size
                          double-buffer hidden resizable)
  (declare (ignore fullscreen buffer-size)
           (optimize debug))
  (macrolet ((double-on-darwin (value)
               `(if (uiop:featurep :darwin)
                    (* 2 , value)
                    ,value)))
    (let ((cl-glfw3::prev-error-fun
           (cl-glfw3:set-error-callback 'cl-glfw3::default-error-fun))
          (red-size   (double-on-darwin red-size))
          (green-size (double-on-darwin green-size))
          (blue-size  (double-on-darwin blue-size))
          (depth-size (if (uiop:featurep :darwin)
                          24
                          depth-size)))
     (unless (cffi-sys:null-pointer-p cl-glfw3::prev-error-fun)
       (%cl-glfw3:set-error-callback cl-glfw3::prev-error-fun))
     (labels
         ((create-window (major minor)
            (%glfw:window-hint #X00021010 (if double-buffer 1 0))
            (glfw:create-window :width width
                                :height height
                                :title title
                                :resizable resizable
                                :visible (not hidden)
                                :decorated (not no-frame)
                                :red-bits red-size
                                :green-bits green-size
                                :blue-bits blue-size
                                :depth-bits depth-size
                                :stencil-bits stencil-size
                                :alpha-bits alpha-size
                                :opengl-profile :opengl-core-profile
                                #+darwin :opengl-forward-compat
                                #+darwin t
                                :context-version-major major
                                :context-version-minor minor)
            (glfw:get-current-context))
          (search-for-context ()
            (let ((context nil)
                  (versions #-darwin`((4 6) (4 5) (4 4) (4 3) (4 2) (4 1) (4 0) (3 3))
                            #+darwin`((4 1) (4 0) (3 3))))
              (loop :for (major minor) :in versions
                 :until context
                 :do (setf context (create-window major minor)))
              (assert context)
              (glfw:set-framebuffer-size-callback 'framebuffer-size-callback)
              (glfw:set-key-callback 'key-callback)
              context)))
       ;; https://www.glfw.org/docs/latest/group__init.html#ga317aac130a235ab08c6db0834907d85e
       ;; This function initializes the GLFW library. Before most GLFW functions can be used,
       ;; GLFW must be initialized, and before an application terminates GLFW should be terminated
       ;; in order to free any resources allocated during or after initialization.
       ;; If this function fails, it calls glfwTerminate before returning. If it succeeds, you should
       ;; call glfwTerminate before the application exits.))
       ;; Additional calls to this function after successful initialization but before termination will return
       ;; GLFW_TRUE immediately.
       ;;
       ;; So, according to the documentation, we are safe by doing this and not leaking memory. Most importantly
       ;; We can keep CEPL happy. And do more than one initialization.
       (glfw:initialize)
       (search-for-context)))))

(defun destroy-glfw-surface (surface)
  t)

(defun glfw-surface-size (win-handle)
  (glfw:get-framebuffer-size win-handle))

(defun glfw-set-surface-size (win-handle width height)
  (glfw:set-window-size win-handle width height))

(defun glfw-surface-fullscreen-p (surface)
  (declare (ignore surface))
  nil)

(defun glfw-set-surface-fullscreen (surface state)
  (declare (ignore surface state))
  t)

(defun glfw-surface-title (surface)
  (declare (ignore surface))
  nil)

(defun glfw-set-surface-title (surface title)
  (glfw:set-window-title title surface))

;;----------------------------------------------------------------------

(defclass glfw-api (cepl.host:api-1)
  (;;
   (supports-multiple-contexts-p :initform nil)
   ;;
   (supports-multiple-surfaces-p :initform t)
   ;;
   (init-function :initform #'glfw-init)
   ;;
   (shutdown-function :initform #'glfw-shutdown)
   ;;
   (make-surface-function :initform #'make-glfw-surface)
   ;;
   (destroy-surface-function :initform #'destroy-glfw-surface)
   ;;
   (make-context-function :initform #'make-glfw-context)
   ;;
   (step-function :initform #'glfw-step-v1)
   ;;
   (register-event-callback-function :initform #'glfw-register-listener)
   ;;
   (swap-function :initform #'glfw-swap)
   ;;
   (surface-size-function :initform #'glfw-surface-size)
   ;;
   (make-context-current-function :initform #'glfw-make-current)
   ;;
   (set-surface-size-function :initform #'glfw-set-surface-size)
   ;;
   (surface-fullscreen-p-function :initform #'glfw-surface-fullscreen-p)
   ;;
   (set-surface-fullscreen-function :initform #'glfw-set-surface-fullscreen)
   ;;
   (surface-title-function :initform #'glfw-surface-title)
   ;;
   (set-surface-title-function :initform #'glfw-set-surface-title)))

(register-host 'glfw-api)

;;----------------------------------------------------------------------

(defun (setf vsync) (boolean)
  (warn "Sorry setting vsync is not supported")
  boolean)
