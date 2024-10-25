
(in-package #:youtube-api)

(defvar *api-key* nil "TODO: OAuth tokens.")

(define-condition quota-impact ()
  ((impact
    :initarg :impact
    :reader quota-impact)
   (account
    :initarg :account
    :reader quota-account))
  (:report (lambda (c s)
	     (format s "~S of ~D unit~:P."
		     (class-name c)
		     (quota-impact c)))))

(defun impact-quota (impact)
  "Notify that we've spent IMPACT units of the API quota."
  (check-type impact (integer 0 *))
  (signal 'quota-impact :impact impact :account *api-key*))

(defmacro with-quota-impact ((impact) &body body)
  (let ((var (make-symbol "with-quota-impact")))
    `(let ((,var ,impact))
       (multiple-value-prog1 (progn ,@body)
	 (impact-quota ,var)))))

(defmacro with-quota-report ((&key account tag) &body body)
  "Print a report to *TRACE-OUTPUT* of API quota usage incurred by BODY."
  (let ((quota-var (make-symbol "quota-var"))
	(account-var (make-symbol "account-var")))
    `(let ((,quota-var 0)
	   (,account-var ,(case account
			    ((nil) nil)
			    ((t) *api-key*)
			    (t account))))
       (unwind-protect
	    (handler-bind
		((quota-impact (lambda (c)
				 (when (or (not ,account-var)
					   (equal ,account-var (quota-account c)))
				   (incf ,quota-var (quota-impact c))))))
	      ,@body)
	 (format *trace-output* "~&;; ~S~@[ ~S~]: ~D units."
		 'with-quota-report
		 ,tag
		 ,quota-var)))))

(defun format-url* (base &rest parameters)
  (with-standard-io-syntax
    (apply #'format nil "~A~^?~@{~A=~A~^&~}"
	   base
	   (loop with previous = nil
		 for (k v) on parameters by #'cddr
		 when (and v (not (member k previous)))
		   collect (str:camel-case k)
		   and collect (etypecase v
				 (integer
				  (format nil "~D" v))
				 (string
				  (quri:url-encode v)))
		 do (pushnew k previous)))))

(defun camel-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is a non-NIL SYMBOL, then make it camel-case. Otherwise keep string."
  (etypecase string-or-symbol
    (null
     nil)
    (symbol
     (str:camel-case string-or-symbol))
    (string
     string-or-symbol)))

(defun comma-list (list &optional (map-entry #'camel-symbol))
  "Format non-empty LIST into a (camel-cased) and comma-separated string."
  (when list
    (format nil "~{~A~^,~}" (mapcar map-entry list))))

(defun api (api &rest args &key &allow-other-keys)
  "Low-level YouTube API access."
  (assert  *api-key*)
  (jonathan:parse (dex:get (apply #'format-url*
				  (format nil "https://www.googleapis.com/youtube/v3/~A" api)
				  :key *api-key*
				  args)
			   :headers '((:user-agent . "Common-Lisp API")))
		  :keyword-normalizer (lambda (camel-string)
					(str:upcase (str:header-case camel-string)))
		  :normalize-all t))

(defun api-unpage (api &rest args &key (max-results 50) &allow-other-keys)
  "Wrapper around API that combines any 'pages' into a single result list."
  (loop with num-results = 0
	for page-token = nil then next-page-token
	for page = (apply #'api api :page-token page-token
				    :max-results (min 50 (if max-results
							     (- max-results num-results)
							     50))
				    args)
	for next-page-token = (getf page :next-page-token)
	for result-page = (getf page :items)
	do (incf num-results (length result-page))
	append result-page
	while (and next-page-token
		   (or (not max-results)
		       (< num-results max-results)))))

(defun youtube-search (&rest args
		       &key channel-type event-type order safe-search type
			 video-caption video-definition video-dimension video-duration
			 (video-only-embeddable-p nil voep-p) (video-embeddable (when voep-p (if video-only-embeddable-p :true :any)))
			 video-license video-paid-product-placement video-syndicated video-type
		       &allow-other-keys)
  "https://developers.google.com/youtube/v3/docs/search"
  (with-quota-impact (100)
    (apply #'api-unpage "search"
	   :part "snippet"		; only choice.
	   :channel-type (camel-symbol channel-type)
	   :event-type (camel-symbol event-type)
	   :order (camel-symbol order)
	   :safe-search (camel-symbol safe-search)
	   :type (camel-symbol type)
	   :video-caption (camel-symbol video-caption)
	   :video-definition (camel-symbol video-definition)
	   :video-dimension (camel-symbol video-dimension)
	   :video-duration (camel-symbol video-duration)
	   :video-embeddable (camel-symbol video-embeddable)
	   :video-license (camel-symbol video-license)
	   :video-paid-product-placement (camel-symbol video-paid-product-placement)
	   :video-syndicated (camel-symbol video-syndicated)
	   :video-type (camel-symbol video-type)
	   args)))

(defun videos-by-channel (channel &key since)
  (youtube-search :channel-id (channel-id channel)
		  :type "video"
		  :order "date"
		  :since since
		  :event-type :upcoming))

(defun videos-list (&rest args
		    &key (parts '(:snippet)) (part (comma-list parts))
		      ids (id (comma-list ids #'identity))
		      chart my-rating
		    &allow-other-keys)
  (with-quota-impact (1)
    (apply #'api-unpage "videos"
	   :ids nil
	   :id id
	   :parts nil
	   :part (camel-symbol part)
	   :chart (camel-symbol chart)
	   :my-rating (camel-symbol my-rating)
	   args)))

(defun channels (&key (parts '(:snippet)) (part (comma-list parts)) for-handle id for-username)
  "https://developers.google.com/youtube/v3/docs/channels/list"
  (with-quota-impact (1)
    (api-unpage "channels"
		:part (camel-symbol part)
		:|id| id
		:|forHandle| for-handle
		:|forUsername| for-username)))

(defun channel-id (id-or-handle)
  "Return a channel-id for ID-OR-HANDLE."
  (when (plusp (length id-or-handle))
    (cond
      ((char= #\@ (char id-or-handle 0))
       (getf (first (channels :part "id" :for-handle id-or-handle))
	     :id))
      (t id-or-handle))))

(defun activities (&rest args
		   &key (parts '(:snippet)) (part (comma-list parts))
		     channel-id
		     (mine-p nil mine-p-p) (mine (when mine-p-p (if mine-p "true" "false")))
		   &allow-other-keys)
  "An activity resource contains information about an action that a
particular channel, or user, has taken on YouTube.
   https://developers.google.com/youtube/v3/docs/activities"
  (with-quota-impact (1)
    (apply #'api-unpage "activities"
	   :parts nil
	   :part (camel-symbol part)
	   :mine-p nil
	   :mine mine
	   :channel-id channel-id
	   args)))
