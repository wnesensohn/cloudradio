(in-package :cloudradio)

;;; These values are obtained (for Google) from here: https://code.google.com/apis/console
(defparameter *client-id* "f9acccf1e22d0a3b34b4ed42e260052b")
(defparameter *client-secret* "849059ac0d5373f85b9f77af59dc0853")
(defparameter *oauth2-callback* "http://78.142.146.150/ThePerfectStream")
(defparameter *oauth2-scopes* "http://www.blogger.com/feeds/ https://www.google.com/m8/feeds")
(defparameter *oauth2-endpoint* "https://soundcloud.com/connect")

#| To debug locally
- run localtunnel to get id
- (localtunnel-setup <4-char-id>)
- verify that it works
- in API console, set callback appropriately |#
(defun localtunnel-setup (id)
  (setf *oauth2-callback* (format nil "http://~A.localtunnel.com/oauth2callback" id)))

(defvar *access-token* "d85d7c110769a69e57ff52cf71ba3629")

(def-session-variable *access-token* nil)
(def-session-variable *refresh-token* nil) ;+++ not used yet

;;; This URI gets passed to client through a redirect
(defun get-auth-code-uri ()
  (let ((endpoint (puri:uri *oauth2-endpoint*))
        (parameters 
         `(("response_type" . "code")
           ("client_id" . ,*client-id*)
           ("redirect_uri" . ,*oauth2-callback* )
           ("access_type" . "offline"))))
    (setf (puri:uri-query endpoint)
          (drakma::alist-to-url-encoded-string parameters :latin1))
    (puri:render-uri endpoint nil)))

(defun access-protected-resource (url access-token &rest other-args)
  (assert access-token)
  (apply #'drakma:http-request url 
         :additional-headers
         `(("GData-Version" . "2")
           ("Authorization" . ,(format nil "Bearer ~A" access-token)))
         other-args))

(defun access-protected-resource-with-error (url &rest other-args)
  (multiple-value-bind  (result status prob-hint prob-advice)
      (apply #'access-protected-resource url *access-token* other-args)
    (case status
      (200 result)
      (t (error "Failed to get protected resource ~A: ~A ~A ~A ~A" url status result prob-hint prob-advice)))))

(defun get-access-token (code)
  (let ((endpoint *oauth2-endpoint*)
        (parameters `(("code" . ,code)
                      ("client_id" . ,*client-id*)
                      ("client_secret" . ,*client-secret*)
                      ("scope" . "non-expiring") ; saves us from
                                        ; having to refresh
                                        ; the token
                      ("redirect_uri" . ,*oauth2-callback*)
                      ("grant_type" . "authorization_code"))))
    (multiple-value-bind (body status headers) 
        (drakma:http-request endpoint 
                             :method :post
                             :parameters parameters)
      (let ((json-response
             (json::decode-json-from-string 
              (coerce body 'string))))
        (if (= status 200)
            (let ((access-token (cdr (assoc :ACCESS--TOKEN json-response)))
                  (refresh-token (cdr (assoc :REFRESH--TOKEN json-response))))
              (values access-token refresh-token))
            (error "Failed to get access token ~A ~A" status json-response))))))

(print (get-auth-code-uri))

