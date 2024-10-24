;;;; package.lisp

(defpackage #:youtube-api
  (:use #:cl)
  (:export
   #:*api-key*
   #:youtube-search
   #:channels
   #:channel-id
   #:videos-list
   ))
