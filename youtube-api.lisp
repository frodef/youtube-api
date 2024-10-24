
(in-package #:youtube-api)

(defvar *api-key* nil "TODO: OAuth tokens.")

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
  "Format LIST into a (camel-cased) and comma-separated string."
  (format nil "~{~A~^,~}" (mapcar map-entry list)))

(defun api (api &rest args &key &allow-other-keys)
  "Low-level YouTube API access."
  (assert  *api-key*)
  (jonathan:parse (dex:get (apply #'format-url*
				  (format nil "https://www.googleapis.com/youtube/v3/~A" api)
				  :|key| *api-key*
				  args)
			   :headers '((:user-agent . "Common-Lisp Api")))
		  :keyword-normalizer (lambda (camel-string)
					(str:upcase (str:header-case camel-string)))
		  :normalize-all t))

(defun api-unpage (api &rest args &key  &allow-other-keys)
  "Wrapper around API that combines any 'pages' into a single result list."
  (loop for page-token = nil then next-page-token
	for page = (apply #'api api :page-token page-token :max-results 50 args)
	for next-page-token = (getf page :next-page-token)
	append (getf page :items)
	while next-page-token))

(defun youtube-search (&rest args
		       &key channel-type event-type order safe-search type
			 video-caption video-definition video-dimension video-duration
			 (video-only-embeddable-p nil voep-p) (video-embeddable (when voep-p (if video-only-embeddable-p :true :any)))
			 video-license video-paid-product-placement video-syndicated video-type
		       &allow-other-keys)
  "https://developers.google.com/youtube/v3/docs/search"
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
	 args))

(defun videos-by-channel (channel &key since)
  (youtube-search :channel-id (channel-id channel)
		  :type "video"
		  :order "date"
		  :since since
		  :event-type :upcoming))

(defun videos-list (&rest args
		    &key (parts '(:snippet)) (part (comma-list parts))
		      chart my-rating
		    &allow-other-keys)
  (apply #'api-unpage "videos"
	 :parts nil
	 :part (camel-symbol part)
	 :chart (camel-symbol chart)
	 :my-rating (camel-symbol my-rating)
	 args))

(defun channels (&key (parts '(:snippet)) (part (comma-list parts)) for-handle id for-username)
  "https://developers.google.com/youtube/v3/docs/channels/list"
  (api-unpage "channels"
	      :part (camel-symbol part)
	      :|id| id
	      :|forHandle| for-handle
	      :|forUsername| for-username))

(defun channel-id (id-or-handle)
  "Return a channel-id for ID-OR-HANDLE."
  (when (plusp (length id-or-handle))
    (cond
      ((char= #\@ (char id-or-handle 0))
       (getf (first (channels :part "id" :for-handle id-or-handle))
	     :id))
      (t id-or-handle))))
