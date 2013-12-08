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
                :read-card32 :write-card32 :card32-get :card8-get :card16-get
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
	   #:query-version))

(in-package xinput)

(declaim (optimize (debug 3) (safety 3)))

(define-extension "XInputExtension")

(defconstant +major-version+ 2)
(defconstant +minor-version+ 2)

;;; Opcodes.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +query-version+ 47))

(defun query-version (display)
  (with-buffer-request-and-reply (display (extension-opcode display "XInputExtension") nil)
    ((data +query-version+)
     (card16 +major-version+)
     (card16 +minor-version+))
    (values
     (card16-get 8)
     (card16-get 10))))

