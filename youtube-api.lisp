
(in-package #:youtube-api)

(defvar *api-key* nil "TODO: OAuth tokens.")

(defun format-url* (base &rest parameters)
  (with-standard-io-syntax
    (apply #'format nil "~A~^?~@{~A=~A~^&~}"
	   base
	   (loop for (k v) on parameters by #'cddr
		 when v
		   collect (str:camel-case k)
		   and collect (etypecase v
				 (integer
				  (format nil "~D" v))
				 (string
				  (quri:url-encode v)))))))

(defun comma-list (list)
  "Format LIST into a downcased and comma-separated string."
  (format nil "~{~(~A~)~^,~}" list))

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

(defun videos-by-channel (channel &key (order "date") since (event-type "upcoming"))
  "https://developers.google.com/youtube/v3/docs/search"
  (api-unpage "search"
	      :channel-id (channel-id channel)
	      :part "snippet" ; only choice.
	      :type "video"
	      :order order
	      :published-after since
	      :event-type event-type))

(defun list-video (video-id)
  (api-unpage "videos"
	      :part "snippet"
	      :|id| video-id))

(defun channels (&key (parts '(:snippet)) (part (comma-list parts)) for-handle id for-username)
  "https://developers.google.com/youtube/v3/docs/channels/list"
  (api-unpage "channels"
	      :part part
	      :|id| id
	      :|forHandle| for-handle
	      :|forUsername| for-username))

(defun channel-handle-id (handle)
  (getf (first (channels :part "id" :for-handle handle))
	:id))

(defun channel-id (id-or-handle)
  "Return a channel-id for ID-OR-HANDLE."
  (when (plusp (length id-or-handle))
    (cond
      ((char= #\@ (char id-or-handle 0))
       (channel-handle-id id-or-handle))
      (t id-or-handle))))
