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
                :read-card32 :write-card32 :card32-get :card8-get :card16-get :write-card16
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

(declaim (optimize (debug 3) (safety 3)))

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

(defun query-version (display)
  (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
    ((data +query-version+)
     (card16 +major-version+)
     (card16 +minor-version+))
    (values
     (card16-get 8)
     (card16-get 10))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype device-id () 'card16)
  (define-accessor device-id (16)
    ((index) `(read-card16 ,index))
    ((index thing) `(write-card16 ,index ,thing))))


(defun query-device (display device-id)
  "DEVICE-ID can be any number or +ALL-DEVICES+ or +ALL-MASTER-DEVICES+"
  (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
    ((data +query-device+)
     (device-id device-id))
    (let* ((num-devices (card16-get 8)))
      num-devices)))
  
