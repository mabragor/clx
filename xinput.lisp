;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Implementation of the XInput 2.2 extension as described by
;;; http://www.x.org/releases/X11R7.7/doc/inputproto/XI2proto.txt
;;; 
;;; Written by Alexandr Popolitov <popolit@gmail.com> in December 2013
;;; and placed in the public domain.

(defpackage :xinput
  (:nicknames :xi)
  (:use :common-lisp :xlib)
  (:import-from :xlib
		:define-accessor :def-clx-class :declare-event
                :allocate-resource-id :deallocate-resource-id
                :print-display-name
                :with-buffer-request :with-buffer-request-and-reply
                :read-card32 :write-card32 :card32-get :card32-put :card8-get :card16-get :write-card16 :read-card16
                :sequence-get :sequence-put
                :data

                ;; types
                :array-index :buffer-bytes

                :with-display
                :buffer-flush :buffer-write :buffer-force-output
                :aset-card8 :aset-card16 :aset-card32)
  (:export ;; constants
           #:+major-version+
           #:+minor-version+

	   ;; requests
	   #:query-pointer #:warp-pointer #:change-cursor #:change-hierarchy
	   #:set-client-pointer #:get-client-pointer #:select-events
	   #:query-version #:query-device
	   #:set-focus #:get-focus
	   #:grab-device #:ungrab-device
	   #:allow-events #:passive-grab-device #:passive-ungrab-device
	   #:list-properties
	   #:change-property #:delete-property #:get-property
	   #:get-selected-events #:barrier-release-pointer))

(in-package xinput)

;; (declaim (optimize (debug 3) (safety 3)))

(define-extension "XInputExtension")

(defconstant +major-version+ 2)
(defconstant +minor-version+ 2)

;; Fake device ID's for event selection */
(defconstant +all-devices+ 0)
(defconstant +all-master-devices+ 1)

;;; Request opcodes.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +query-pointer+ 40)
  (defconstant +warp-pointer+ 41)
  (defconstant +change-cursor+ 42)
  (defconstant +change-hierarchy+ 43)
  (defconstant +set-client-pointer+ 44)
  (defconstant +get-client-pointer+ 45)
  (defconstant +select-events+ 46)
  (defconstant +query-version+ 47)
  (defconstant +query-device+ 48)
  (defconstant +set-focus+ 49)
  (defconstant +get-focus+ 50)
  (defconstant +grab-device+ 51)
  (defconstant +ungrab-device+ 52)
  (defconstant +allow-events+ 53)
  (defconstant +passive-grab-device+ 54)
  (defconstant +passive-ungrab-device+ 55)
  (defconstant +list-properties+ 56)
  (defconstant +change-property+ 57)
  (defconstant +delete-property+ 58)
  (defconstant +get-property+ 59)
  (defconstant +get-selected-events+ 60)
  (defconstant +barrier-release-pointer+ 61))

;; Device use types
(defconstant +master-pointer+ 1)
(defconstant +master-keyboard+ 2)
(defconstant +slave-pointer+ 3)
(defconstant +slave-keyboard+ 4)
(defconstant +floating-slave+ 5)


(defun query-version (display)
  (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
    ((data +query-version+)
     (card16 +major-version+)
     (card16 +minor-version+))
    (values
     (card16-get 8)
     (card16-get 10))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun xi-intern (&rest parts)
    (intern (apply #'concatenate 'string (mapcar #'string parts)) (find-package "XI")))

  (defun maybe-expand-spec (spec)
    (if (atom spec)
	(cond ((keywordp spec) `(,spec ,(xi-intern "+" spec "+")))
	      ((symbolp spec) `(,(xlib::kintern (string-trim '(#\+) (string spec))) ,spec))
	      (t (error "If spec is an atom, it should be a var-name or a keyword")))
	spec)))

(defmacro define-kw-map-field (name (width low-level-type &key allow-other-values) &body specs)
  (let ((specs (mapcar #'maybe-expand-spec specs))
	(ll-reader (xi-intern "READ-" low-level-type))
	(ll-writer (xi-intern "WRITE-" low-level-type))
	(g!-it (gensym "IT"))
	(g!-thing (gensym "THING")))
    `(progn (deftype ,name () ,(if allow-other-values
				`'(or ,low-level-type keyword)
				''keyword))
 	    (define-accessor ,name (,width)
	      ((index) `(let ((it (,',ll-reader ,index)))
			  (cond ,@',(mapcar (lambda (spec)
					      `((equal it ,(cadr spec)) ,(car spec)))
					    specs)
				(t ,',(if allow-other-values
					  'it
					  '(error "Unanticipated value ~a aquired" it))))))
	      ((index thing) `(let* ((,',g!-thing ,thing)
				     (,',g!-it (cond ,@',(mapcar (lambda (spec)
								   `((eq ,g!-thing ,(car spec)) ,(cadr spec)))
								 specs)
						     (t ,',(if allow-other-values
							       g!-thing
							       `(error "Attempt to write unanticipated value ~a"
								       ,g!-thing))))))
				(,',ll-writer ,index ,',g!-it)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-kw-map-field device-id (16 card16 :allow-other-values t)
    :all-devices
    :all-master-devices))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-kw-map-field device-use (16 card16 :allow-other-values t)
    :master-pointer
    :master-keyboard
    :slave-pointer
    :slave-keyboard
    :floating-slave))
    

(defstruct device-info
  (device-id 0 :type device-id)
  (use 0 :type device-use)
  (attachment 0 :type device-id)
  (enabled nil :type boolean)
  (num-classes 0 :type card16)
  (name-len 0 :type card16)
  (name "" :type string)
  (classes nil :type list))
  
(define-accessor device-info (48)
  ((index) `(make-device-info :device-id (xlib::device-id-get ,index)
	     :use (xlib::device-use-get (+ 2 ,index))
	     :attachment (xlib::device-id-get (+ 4 ,index))))
  ((index thing) `(progn (xlib::device-id-put ,index (slot-value ,thing 'device-id))
			 (xlib::device-use-put (+ 2 ,index) (slot-value ,thing 'use))
			 (xlib::device-id-put (+ 4 ,index) (slot-value ,thing 'attachment)))))

(defun query-device (display device-id)
  "DEVICE-ID can be any number or +ALL-DEVICES+ or +ALL-MASTER-DEVICES+"
  (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") size)
    ((data +query-device+)
     (device-id device-id))
    (let* ((num-devices (card16-get 8)))
      (if (< 0 num-devices)
	  ;; (xlib::device-info-get (+ 2 8))
	  (list size
		(card32-get 0)
		(card16-get 8)
		(card16-get 10)
		(card16-get 12)
		(card16-get 14)
		(card16-get 16)
		(card16-get 18)
		(card16-get 20))
	  num-devices))))
  
(define-accessor fp16-16 (32)
  ((index) `(let ((integral-part (xlib::int16-get ,index))
		  (fractional-part (xlib::card16-get (+ 2 ,index))))
	      (+ integral-part (/ fractional-part (ash 1 16)))))
  ((index thing) `(multiple-value-bind (integral-part fractional-part)
		      (floor ,thing)
		    (xlib::int16-put ,index integral-part)
		    (xlib::card16-put (+ 2 ,index) (floor (* fractional-part (ash 1 16)))))))

(define-accessor fp32-32 (32)
  ((index) `(let ((integral-part (xlib::int32-get ,index))
		  (fractional-part (xlib::card32-get (+ 4 ,index))))
	      (+ integral-part (/ fractional-part (ash 1 32)))))
  ((index thing) `(multiple-value-bind (integral-part fractional-part)
		      (floor ,thing)
		    (xlib::int32-put ,index integral-part)
		    (xlib::card32-put (+ 4 ,index) (floor (* fractional-part (ash 1 32)))))))

(define-accessor modifier-info (128) ; (* 4 32)
  ((index) `(list :base-mods (card32-get ,index)
		  :latched-mods (card32-get (+ 4 ,index))
		  :locked-mods (card32-get (+ 4 4 ,index))
		  :effective-mods (card32-get (+ 4 4 4 ,index))))
  ((index thing) `(progn (card32-put ,index (getf ,thing :base-mods))
			 (card32-put (+ 4 ,index) (getf ,thing :latched-mods))
			 (card32-put (+ 4 4 ,index) (getf ,thing :locked-mods))
			 (card32-put (+ 4 4 4 ,index) (getf ,thing :effective-mods)))))

(define-accessor group-info (32) ; (* 4 8)
  ((index) `(list :base-group (card8-get ,index)
		  :latched-group (card8-get (+ 4 ,index))
		  :locked-group (card8-get (+ 4 4 ,index))
		  :effective-group (card8-get (+ 4 4 4 ,index))))
  ((index thing) `(progn (card8-put ,index (getf ,thing :base-group))
			 (card8-put (+ 4 ,index) (getf ,thing :latched-group))
			 (card8-put (+ 4 4 ,index) (getf ,thing :locked-group))
			 (card8-put (+ 4 4 4 ,index) (getf ,thing :effective-group)))))

(defun nmake-huge-number (list-of-card32s)
  (let ((i 0)
	(res 0))
    (dolist (elt (nreverse list-of-card32s))
      (incf res (* elt (ash 1 i)))
      (incf i 32))
    res))

(defun query-pointer (display window device-id)
  (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
      ((data +query-pointer+)
       (window window)
       (device-id device-id))
    ;; compiler will surely wrap this up into nice numbers, but this may they are easier to read
    (let ((offset 8))
      (declare (fixnum offset))
      (macrolet ((frob (getter offset-incr)
		   `(prog1 (,getter offset) (incf offset ,offset-incr))))
	(let* ((res (list :root (frob xlib::window-get 4)
			  :child (frob xlib::window-get 4)
			  :root-x (frob xlib::fp16-16-get 4)
			  :root-y (frob xlib::fp16-16-get 4)
			  :win-x (frob xlib::fp16-16-get 4)
			  :win-y (frob xlib::fp16-16-get 4)
			  :same-screen (frob xlib::boolean-get 1)
			  :mods (frob xlib::modifier-info-get 128)
			  :group (frob xlib::group-info-get 32)))
	       ;;BUTTONS is a variable-length field, so this zoo is necessary
	       (buttons-len (frob xlib::card16-get 2))
	       (buttons (nmake-huge-number (sequence-get :length buttons-len :result-type 'list))))
	  `(,.res :buttons-len ,buttons-len :buttons ,buttons))))))


(defun warp-pointer (display src-win dst-win src-x src-y src-width src-height dst-x dst-y device-id)
  (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
    ((data +warp-pointer+)
     (window src-win)
     (window dst-win)
     (fp16-16 src-x)
     (fp16-16 src-y)
     (int16 src-width)
     (int16 src-height)
     (fp16-16 dst-x)
     (fp16-16 dst-y)
     (device-id device-id))))


