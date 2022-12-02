(ql:quickload "cepl.glfw")
(ql:quickload :livesupport)
(ql:quickload :rtg-math)
(ql:quickload :rtg-math.vari)
(ql:quickload :temporal-functions)

(defpackage #:cepl.glfw.triangle
  (:use #:cl #:livesupport #:rtg-math #:vari #:temporal-functions #:cepl)
  (:export run-demo run-loop stop-loop))

(in-package :cepl.glfw.triangle)

(defparameter *array* nil)
(defparameter *stream* nil)

;; Lisp data for triangle vertices
(defparameter *triangle-data*
   (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
         (list (v!    0   0.5 0) (v! 1 0 0 1))
         (list (v! -0.5 -0.36 0) (v! 0 0 1 1))))

;; A struct that works on gpu and cpu
(defstruct-g pos-col
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

;; A GPU vertex shader using a Lisp syntax (see Varjo)
(defun-g vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

;; A GPU fragment shader
(defun-g frag ((color :vec4))
  color)

;; Composing those gpu functions into a pipeline
(defpipeline-g prog-1 ()
  (vert pos-col)
  (frag :vec4))

;; Here is what we do each frame:
(defun step-demo ()
  (step-host)        ;; Advance the host environment frame
  (update-repl-link) ;; Keep the REPL responsive while running
  (clear)            ;; Clear the drawing buffer
  (map-g #'prog-1 *stream*) ;; Render data from GPU datastream
  (swap))            ;; Display newly rendered buffer


(defun run-loop ()
  (setf
    ;; Create a gpu array from our Lisp vertex data
        *array* (make-gpu-array *triangle-data* :element-type 'pos-col)
    ;; Create a GPU datastream
        *stream* (make-buffer-stream *array*))
  ;; continue rendering frames until *running* is set to nil
  (livesupport:setup-lisp-repl)
  (loop :while (not (shutting-down-p))
        :do (continuable (step-demo))))

(defun run-demo ()
  (cepl:repl)
  (cepl:register-event-listener
   (lambda (event)
     (if (and (eql (first event) :key)
              (eql (second event) :escape))
         (cepl:quit)
         (print event))))
  (run-loop))
