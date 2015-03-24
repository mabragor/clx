;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Implementation of the XInput 2.2 extension as described by
;;; http://www.x.org/releases/X11R7.7/doc/inputproto/XI2proto.txt
;;; 
;;; Written by Alexandr Popolitov <popolit@gmail.com> in December 2013
;;; and placed in the public domain.

;; (defpackage :xinput
;;   (:nicknames :xi)
;;   (:use :common-lisp :iterate)
;;   (:shadow :allow-events)
;;   (:import-from :xlib
;; 		:define-accessor :def-clx-class :declare-event
;;                 :allocate-resource-id :deallocate-resource-id
;;                 :print-display-name
;;                 :with-buffer-request :with-buffer-request-and-reply
;;                 :read-card32 :write-card32 :card32-get :card32-put :card8-get :card16-get :write-card16 :read-card16
;; 		:read-card8 :write-card8
;;                 :sequence-get :sequence-put
;;                 :data

;;                 ;; types
;;                 :array-index :buffer-bytes

;;                 :with-display
;;                 :buffer-flush :buffer-write :buffer-force-output
;;                 :aset-card8 :aset-card16 :aset-card32)
;;   (:export ;; constants
;;            #:+major-version+
;;            #:+minor-version+

;; 	   ;; requests
;; 	   #:query-pointer #:warp-pointer #:change-cursor #:change-hierarchy
;; 	   #:set-client-pointer #:get-client-pointer #:select-events
;; 	   #:query-version #:query-device
;; 	   #:set-focus #:get-focus
;; 	   #:grab-device #:ungrab-device
;; 	   #:allow-events #:passive-grab-device #:passive-ungrab-device
;; 	   #:list-properties
;; 	   #:change-property #:delete-property #:get-property
;; 	   #:get-selected-events #:barrier-release-pointer))

;; (in-package xinput)

;; ;; (declaim (optimize (debug 3) (safety 3)))

;; (define-extension "XInputExtension")

;; (defconstant +major-version+ 2)
;; (defconstant +minor-version+ 2)

;; ;; Fake device ID's for event selection */
;; (defconstant +all-devices+ 0)
;; (defconstant +all-master-devices+ 1)

;; ;;; Request opcodes.
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defconstant +query-pointer+ 40)
;;   (defconstant +warp-pointer+ 41)
;;   (defconstant +change-cursor+ 42)
;;   (defconstant +change-hierarchy+ 43)
;;   (defconstant +set-client-pointer+ 44)
;;   (defconstant +get-client-pointer+ 45)
;;   (defconstant +select-events+ 46)
;;   (defconstant +query-version+ 47)
;;   (defconstant +query-device+ 48)
;;   (defconstant +set-focus+ 49)
;;   (defconstant +get-focus+ 50)
;;   (defconstant +grab-device+ 51)
;;   (defconstant +ungrab-device+ 52)
;;   (defconstant +allow-events+ 53)
;;   (defconstant +passive-grab-device+ 54)
;;   (defconstant +passive-ungrab-device+ 55)
;;   (defconstant +list-properties+ 56)
;;   (defconstant +change-property+ 57)
;;   (defconstant +delete-property+ 58)
;;   (defconstant +get-property+ 59)
;;   (defconstant +get-selected-events+ 60)
;;   (defconstant +barrier-release-pointer+ 61))

;; ;; Device use types
;; (defconstant +master-pointer+ 1)
;; (defconstant +master-keyboard+ 2)
;; (defconstant +slave-pointer+ 3)
;; (defconstant +slave-keyboard+ 4)
;; (defconstant +floating-slave+ 5)


;; (defun query-version (display)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;     ((data +query-version+)
;;      (card16 +major-version+)
;;      (card16 +minor-version+))
;;     (values
;;      (card16-get 8)
;;      (card16-get 10))))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defun xi-intern (&rest parts)
;;     (intern (apply #'concatenate 'string (mapcar #'string parts)) (find-package "XI")))

;;   (defun maybe-expand-spec (spec)
;;     (if (atom spec)
;; 	(cond ((keywordp spec) `(,spec ,(xi-intern "+" spec "+")))
;; 	      ((symbolp spec) `(,(xlib::kintern (string-trim '(#\+) (string spec))) ,spec))
;; 	      (t (error "If spec is an atom, it should be a var-name or a keyword")))
;; 	spec)))

;; (defmacro define-kw-map-field (name (width low-level-type &key allow-other-values) &body specs)
;;   (let ((specs (mapcar #'maybe-expand-spec specs))
;; 	(ll-reader (xi-intern "READ-" low-level-type))
;; 	(ll-writer (xi-intern "WRITE-" low-level-type))
;; 	(g!-it (gensym "IT"))
;; 	(g!-thing (gensym "THING")))
;;     `(progn (deftype ,name () ,(if allow-other-values
;; 				`'(or ,low-level-type keyword)
;; 				''keyword))
;;  	    (define-accessor ,name (,width)
;; 	      ((index) `(let ((it (,',ll-reader ,index)))
;; 			  (cond ,@',(mapcar (lambda (spec)
;; 					      `((equal it ,(cadr spec)) ,(car spec)))
;; 					    specs)
;; 				(t ,',(if allow-other-values
;; 					  'it
;; 					  '(error "Unanticipated value ~a aquired" it))))))
;; 	      ((index thing) `(let* ((,',g!-thing ,thing)
;; 				     (,',g!-it (cond ,@',(mapcar (lambda (spec)
;; 								   `((eq ,g!-thing ,(car spec)) ,(cadr spec)))
;; 								 specs)
;; 						     (t ,',(if allow-other-values
;; 							       g!-thing
;; 							       `(error "Attempt to write unanticipated value ~a"
;; 								       ,g!-thing))))))
;; 				(,',ll-writer ,index ,',g!-it)))))))


;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (define-kw-map-field device-id (16 card16 :allow-other-values t)
;;     :all-devices
;;     :all-master-devices))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (define-kw-map-field device-use (16 card16 :allow-other-values t)
;;     :master-pointer
;;     :master-keyboard
;;     :slave-pointer
;;     :slave-keyboard
;;     :floating-slave))
    

;; (defstruct device-info
;;   (device-id 0 :type device-id)
;;   (use 0 :type device-use)
;;   (attachment 0 :type device-id)
;;   (enabled nil :type boolean)
;;   (num-classes 0 :type card16)
;;   (name-len 0 :type card16)
;;   (name "" :type string)
;;   (classes nil :type list))
  
;; (define-accessor device-info (48)
;;   ((index) `(make-device-info :device-id (xlib::device-id-get ,index)
;; 	     :use (xlib::device-use-get (+ 2 ,index))
;; 	     :attachment (xlib::device-id-get (+ 4 ,index))))
;;   ((index thing) `(progn (xlib::device-id-put ,index (slot-value ,thing 'device-id))
;; 			 (xlib::device-use-put (+ 2 ,index) (slot-value ,thing 'use))
;; 			 (xlib::device-id-put (+ 4 ,index) (slot-value ,thing 'attachment)))))

;; (defun query-device (display device-id)
;;   "DEVICE-ID can be any number or +ALL-DEVICES+ or +ALL-MASTER-DEVICES+"
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") size)
;;     ((data +query-device+)
;;      (device-id device-id))
;;     (let* ((num-devices (card16-get 8)))
;;       (if (< 0 num-devices)
;; 	  ;; (xlib::device-info-get (+ 2 8))
;; 	  (list size
;; 		(card32-get 0)
;; 		(card16-get 8)
;; 		(card16-get 10)
;; 		(card16-get 12)
;; 		(card16-get 14)
;; 		(card16-get 16)
;; 		(card16-get 18)
;; 		(card16-get 20))
;; 	  num-devices))))

;; (eval-when (:compile-toplevel :load-toplevel :execute)  
;;   (define-accessor fp16-16 (32)
;;     ((index) `(let ((integral-part (xlib::int16-get ,index))
;; 		    (fractional-part (xlib::card16-get (+ 2 ,index))))
;; 		(+ integral-part (/ fractional-part (ash 1 16)))))
;;     ((index thing) `(multiple-value-bind (integral-part fractional-part)
;; 			(floor ,thing)
;; 		      (xlib::int16-put ,index integral-part)
;; 		      (xlib::card16-put (+ 2 ,index) (floor (* fractional-part (ash 1 16)))))))

;;   (define-accessor fp32-32 (32)
;;     ((index) `(let ((integral-part (xlib::int32-get ,index))
;; 		    (fractional-part (xlib::card32-get (+ 4 ,index))))
;; 		(+ integral-part (/ fractional-part (ash 1 32)))))
;;     ((index thing) `(multiple-value-bind (integral-part fractional-part)
;; 			(floor ,thing)
;; 		      (xlib::int32-put ,index integral-part)
;; 		      (xlib::card32-put (+ 4 ,index) (floor (* fractional-part (ash 1 32)))))))

;;   (define-accessor modifier-info (128) ; (* 4 32)
;;     ((index) `(list :base-mods (card32-get ,index)
;; 		    :latched-mods (card32-get (+ 4 ,index))
;; 		    :locked-mods (card32-get (+ 4 4 ,index))
;; 		    :effective-mods (card32-get (+ 4 4 4 ,index))))
;;     ((index thing) `(progn (card32-put ,index (getf ,thing :base-mods))
;; 			   (card32-put (+ 4 ,index) (getf ,thing :latched-mods))
;; 			   (card32-put (+ 4 4 ,index) (getf ,thing :locked-mods))
;; 			   (card32-put (+ 4 4 4 ,index) (getf ,thing :effective-mods)))))

;;   (define-accessor group-info (32) ; (* 4 8)
;;     ((index) `(list :base-group (card8-get ,index)
;; 		    :latched-group (card8-get (+ 4 ,index))
;; 		    :locked-group (card8-get (+ 4 4 ,index))
;; 		    :effective-group (card8-get (+ 4 4 4 ,index))))
;;     ((index thing) `(progn (card8-put ,index (getf ,thing :base-group))
;; 			   (card8-put (+ 4 ,index) (getf ,thing :latched-group))
;; 			   (card8-put (+ 4 4 ,index) (getf ,thing :locked-group))
;; 			   (card8-put (+ 4 4 4 ,index) (getf ,thing :effective-group)))))

;;   (defun nmake-huge-number (list-of-card32s)
;;     (let ((i 0)
;; 	  (res 0))
;;       (dolist (elt (nreverse list-of-card32s))
;; 	(incf res (* elt (ash 1 i)))
;; 	(incf i 32))
;;       res)))

;; (defmacro with-auto-offset ((start) &body body)
;;   `(let ((offset ,start))
;;      (declare (fixnum offset))
;;      (macrolet ((autoft (getter offset-incr)
;; 		  `(prog1 (,getter offset) (incf offset ,offset-incr))))
;;        ,@body)))
  

;; (defun query-pointer (display window device-id)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;       ((data +query-pointer+)
;;        (window window)
;;        (device-id device-id))
;;     (with-auto-offset (8)
;;       (let* ((res (list :root (autoft xlib::window-get 4)
;; 			:child (autoft xlib::window-get 4)
;; 			:root-x (autoft xlib::fp16-16-get 4)
;; 			:root-y (autoft xlib::fp16-16-get 4)
;; 			:win-x (autoft xlib::fp16-16-get 4)
;; 			:win-y (autoft xlib::fp16-16-get 4)
;; 			:same-screen (autoft xlib::boolean-get 1)
;; 			:mods (autoft xlib::modifier-info-get 128)
;; 			:group (autoft xlib::group-info-get 32)))
;; 	     ;;BUTTONS is a variable-length field, so this zoo is necessary
;; 	     (buttons-len (autoft xlib::card16-get 2))
;; 	     (buttons (nmake-huge-number (sequence-get :length buttons-len :result-type 'list))))
;; 	`(,.res :buttons-len ,buttons-len :buttons ,buttons)))))


;; (defun warp-pointer (display src-win dst-win src-x src-y src-width src-height dst-x dst-y device-id)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;     ((data +warp-pointer+)
;;      (window src-win)
;;      (window dst-win)
;;      (fp16-16 src-x)
;;      (fp16-16 src-y)
;;      (int16 src-width)
;;      (int16 src-height)
;;      (fp16-16 dst-x)
;;      (fp16-16 dst-y)
;;      (device-id device-id))))

;; (defun change-cursor (display win cursor device-id)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;       ((data +change-cursor+)
;;        (window win)
;;        (cursor cursor)
;;        (device-id device-id))))

;; (defun set-client-pointer (display win device-id)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;       ((data +set-client-pointer+)
;;        (window win)
;;        (device-id device-id))))

;; ;; TODO: by misterious reason this code throws WINDOW-ERROR, when invoked on root window.
;; (defun get-client-pointer (display window)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;       ((data +get-client-pointer+)
;;        (window window))
;;     (list :set (xlib::boolean-get 8)
;; 	  :device-id (xlib::device-id-get 9))))
  

;; (defun set-focus (display focus device-id time)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;       ((data +set-focus+)
;;        (window focus)
;;        (device-id device-id)
;;        (card32 time))))

;; (defun get-focus (display device-id)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;       ((data +get-focus+)
;;        (device-id device-id))
;;     (xlib::window-get 8)))


;; ;; grab modes
;; (defconstant +grab-mode-sync+ 0)
;; (defconstant +grab-mode-async+ 1)
;; (defconstant +grab-mode-touch+ 2)

;; ;; grab reply status codes
;; (defconstant +grab-success+ 0)
;; (defconstant +already-grabbed+ 1)
;; (defconstant +grab-invalid-time+ 2)
;; (defconstant +grab-not-viewable+ 3)
;; (defconstant +grab-frozen+ 4)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (define-kw-map-field grab-mode (8 card8)
;;     (:sync +grab-mode-sync+)
;;     (:async +grab-mode-async+)
;;     (:touch +grab-mode-touch+))
;;   (define-kw-map-field grab-status (8 card8)
;;     (:success +grab-success+)
;;     (:already-grabbed +already-grabbed+)
;;     (:invalid-time +grab-invalid-time+)
;;     (:not-viewable +grab-not-viewable+)
;;     (:frozen +grab-frozen+)))



;; (defun grab-device (display
;; 		    device-id grab-window owner-events
;; 		    grab-mode paired-device-mode time cursor
;; 		    mask-len masks)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;       ((data +grab-device+)
;;        (device-id device-id)
;;        (window grab-window)
;;        (boolean owner-events)
;;        (grab-mode grab-mode)
;;        (grab-mode paired-device-mode)
;;        (card32 time)
;;        (cursor cursor)
;;        (card16 mask-len)
;;        (mask masks)) ; now, this is probably wrong
;;     (xlib::grab-status-get 8)))

;; (defun ungrab-device (display device-id time)
;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;;       ((data +ungrab-device+)
;;        (device-id device-id)
;;        (card32 time))))

;; ;; XIAllowEvents event-modes
;; (defconstant +async-device+ 0)
;; (defconstant +sync-device+ 1)
;; (defconstant +replay-device+ 2)
;; (defconstant +async-paired-device+ 3)
;; (defconstant +async-pair+ 4)
;; (defconstant +sync-pair+ 5)
;; (defconstant +accept-touch+ 6)
;; (defconstant +reject-touch+ 7)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (define-kw-map-field event-mode (8 card8)
;;     :async-device
;;     :sync-device
;;     :replay-device
;;     :async-paired-device
;;     :async-pair
;;     :sync-pair
;;     :accept-touch
;;     :reject-touch))

;; ;; (defun allow-events (display device-id time event-mode)
;; ;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;; ;;       ((data +ungrab-device+)
;; ;;        (device-id device-id)
;; ;;        (card32 time)
;; ;;        (event-mode event-mode))))

;; ;; (eval-when (:compile-toplevel :load-toplevel :execute)
;; ;;   (define-accessor xatom (16)
;; ;;     ((index &optional display) `(xlib::atom-name ,(or display (intern "DISPLAY")) (card16-get ,index)))
;; ;;     ((index thing &optional display) `(xlib::card16-put ,index (xlib::find-atom ,(or display (intern "DISPLAY"))
;; ;; 										,thing)))))

;; ;; (defun list-properties (display device-id)
;; ;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;; ;;       ((data +list-properties+)
;; ;;        (device-id device-id))
;; ;;     (let ((nprops (xlib::int16-get 8)))
;; ;;       (let (res)
;; ;; 	;; trial-and-error method leads to the conclusion that atom-id is CARD16
;; ;; 	;; TODO: also, quite a lot atoms resolve to NIL. Is there some way to register them?
;; ;; 	(do ((i 0 (1+ i))
;; ;; 	     (offset 10 (+ 2 offset)))
;; ;; 	    ((>= i nprops))
;; ;; 	  (push (xlib::xatom-get offset) res))
;; ;; 	(nreverse res)))))

;; ;; (defun delete-property (display device-id property)
;; ;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;; ;;       ((data +delete-property+)
;; ;;        (device-id device-id)
;; ;;        ;; TODO: this request should not fail if the property does not exist
;; ;;        (xatom property))))


;; ;; (defun get-property (display device-id property type offset len delete)
;; ;;   (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
;; ;;       ((data +get-property+)
;; ;;        (device-id device-id)
;; ;;        (xatom property)
;; ;;        (xatom type)
;; ;;        (card32 offset)
;; ;;        (card32 len)
;; ;;        (boolean delete))
;; ;;     (with-auto-offset (8)
;; ;;       (let* ((type (autoft xlib::xatom-get 2))
;; ;; 	     (bytes-after (autoft card32-get 4))
;; ;; 	     (num-items (autoft card32-get 4))
;; ;; 	     (format (autoft card8-get 1)))
;; ;; 	(list :type type
;; ;; 	      :bytes-after bytes-after
;; ;; 	      :num-items num-items
;; ;; 	      :format format
;; ;; 	      :data (iter (for i from 1 to (/ format 8))
;; ;; 			  (collect (cond ((equal format 8) (autoft card8-get 1))
;; ;; 					 ((equal format 16) (autoft card16-get 2))
;; ;; 					 ((equal format 32) (autoft card32-get 4))
;; ;; 					 (t (error "Unrecognized property format ~a" format))))))))))

;; ;; Events

;; ;; (eval-when (:compile-toplevel :load-toplevel :execute)
;; ;;   (define-accessor event-header (???)
;; ;;     ((index) `(list :type 
;; ;;     ((index thing &optional display) `(xlib::card16-put ,index (xlib::find-atom ,(or display (intern "DISPLAY"))
;; ;; 										,thing)))))
