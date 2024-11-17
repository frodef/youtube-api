;;;; package.lisp

(defpackage #:youtube-api
  (:use #:cl)
  (:export
   #:*api-key*
   #:activities
   #:channels
   #:channel-id
   #:videos-list
   #:with-quota-report
   #:youtube-search
   #:decode-duration
   ))
