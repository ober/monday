;; -*- Gerbil -*-
package: monday
namespace: monday
(export main)

(declare (not optimize-dead-definitions))
(import
  :gerbil/gambit
  :scheme/base
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/db/dbi
  :std/db/sqlite
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/misc/channel
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  )

(def config-file "~/.monday.yaml")
(def display-format "org-mode")
(def monday-company-key #f)
(def monday-personal-key #f)
(def header #f)

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(def program-name "monday")
(def DEBUG (getenv "DEBUG" #f))
(def (dp msg)
  (when DEBUG
    (displayln msg)))

(def interactives
  (hash
   ("boards" (hash (description: "users.") (usage: "users") (count: 0)))
   ("config" (hash (description: "Configure credentials for app.") (usage: "config") (count: 0)))
   ("updates" (hash (description: "updates.") (usage: "updates") (count: 0)))
   ("users" (hash (description: "users.") (usage: "users") (count: 0)))
   ("pulses" (hash (description: "users.") (usage: "users") (count: 0)))
   ))

(def (main . args)
  (if (null? args)
    (usage))
  (let* ((argc (length args))
	 (verb (car args))
	 (args2 (cdr args)))
    (unless (hash-key? interactives verb)
      (usage))
    (let* ((info (hash-get interactives verb))
	   (count (hash-get info count:)))
      (unless count
	(set! count 0))
      (unless (= (length args2) count)
	(usage-verb verb))
      (apply (eval (string->symbol (string-append "monday#" verb))) args2))))

(def (load-config)
  (let ((config (hash)))
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car (yaml-load config-file)))
    (let-hash config
      (when (and .?app-key
		 .?app-iv
		 .?app-password
		 .?api-key
		 .?api-iv
		 .?api-password)
	(let ((keys (get-keys-from-config .api-key
					.api-iv
					.api-password
					.app-key
					.app-iv
					.app-password)))
	  (hash-put! config 'monday-company-key (hash-ref keys "api"))
	  (hash-put! config 'monday-personal-key (hash-ref keys "app")))))
    config))


(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln "Usage: monday <verb>")
  (displayln "Verbs:")
  (for-each
    (lambda (k)
      (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
    (sort! (hash-keys interactives) string<?))
  (exit 2))

(def (show-tables tables)
  (for-each
    (lambda (t)
      (format-me t))
    tables))

(def (format-me t)
  (cond
   ((string=? display-format "org-mode")
    (format-org t))

   ((string=? display-format "raw")
    (format-raw t))
   (else
    (format-raw t))))

(def (format-org t)
  (unless header
    (for-each
      (lambda (h)
	(display (format "|~a" h)))
      (hash-keys t))
    (displayln "|")
    (set! header #t))
  (hash-for-each
   (lambda (k v)
     (display (format "|~a" v)))
   t)
  (displayln "|"))

(def (format-raw t)
  (displayln (hash->list t)))

(def (users)
  (show-tables (monday-call "users.json" "get" [])))

(def (pulses)
  (show-tables (monday-call "pulses.json" "get" [])))

(def (boards)
  (show-tables (monday-call "boards.json" "get" [])))

(def (updates)
  (show-tables (monday-call "updates.json" "get" [])))

(def (monday-get adds data)
  (monday-call adds "get" data))

(def (monday-put adds data)
  (monday-call adds "put" data))

(def (monday-call adds type data)
  (let-hash (load-config)
    (let* ((uri (format "https://api.monday.com/v1/~a?api_key=~a" adds .monday-personal-key))
	   (headers [
		     ["Accept" :: "*/*"]
		     ["Content-type" :: "application/json"]
		     ])
	   (results (cond ((string= type "get")
			   (do-get-generic uri headers))
			  ((string= type "put")
			   (do-put uri headers (json-object->string data)))
			  (else
			   (displayln "unsupported type: " type))))
	   (myjson (from-json results)))
      myjson)))

(def (config)
  (let-hash (load-config)
    (displayln "Please enter your Monday Personal API Key:")
    (def app-key (read-line (current-input-port)))
    (displayln "Please enter your Monday Company Key:")
    (def api-key (read-line (current-input-port)))
    (displayln "Add the following lines to your " config-file)
    (displayln "-----------------------------------------")
    (let ((api-hash (encrypt-string api-key)))
      (displayln "api-key: " (hash-ref api-hash "key"))
      (displayln "api-iv:  " (hash-ref api-hash "iv"))
      (displayln "api-password: " (hash-ref api-hash "password")))
    (let ((app-hash (encrypt-string app-key)))
      (displayln "app-key: " (hash-ref app-hash "key"))
      (displayln "app-iv:  " (hash-ref app-hash "iv"))
      (displayln "app-password: " (hash-ref app-hash "password")))
    (displayln "-----------------------------------------")))

(def (encrypt-string str)
  (let* ((cipher (make-aes-256-ctr-cipher))
	 (iv (random-bytes (cipher-iv-length cipher)))
	 (key (random-bytes (cipher-key-length cipher)))
	 (encrypted-password (encrypt cipher key iv str))
	 (enc-pass-store (u8vector->base64-string encrypted-password))
	 (iv-store (u8vector->base64-string iv))
	 (key-store (u8vector->base64-string key)))
    (hash
	  ("key" key-store)
	  ("iv" iv-store)
	  ("password" enc-pass-store))))

(def (decrypt-password key iv password)
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

(def (get-keys-from-config api-key api-iv api-password app-key app-iv app-password)
  (hash
   ("api" (decrypt-password api-key api-iv api-password))
   ("app" (decrypt-password app-key app-iv app-password))))

(def (do-get-generic uri headers)
  (let* ((reply (http-get uri
			  headers: headers))
	 (status (request-status reply))
	 (text (request-text reply)))
    (print-curl "get" uri "" "")
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (print-curl type uri headers data)
  ;;(displayln headers)
  (let ((heads "Content-type: application/json")
	(do-curl (getenv "DEBUG" #f)))
    (when do-curl
      (cond
       ((string=? type "get")
	(if (string=? "" data)
	  (displayln (format "curl -X GET -H \'~a\' ~a" heads uri))
	  (displayln (format "curl -X GET -H \'~a\' -d \'~a\' ~a" heads data uri))))
       ((string=? type "put")
	(displayln (format "curl -X PUT -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "post")
	(displayln (format "curl -X POST -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "delete")
	(displayln (format "curl -X DELETE -H \'~a\' -d \'~a\' ~a" heads data uri)))
       (else
	(displayln "unknown format " type))))))

(def (success? status)
  (and (>= status 200) (<= status 299)))

(def (do-put uri headers data)
  (dp (print-curl "put" uri headers data))
  (let* ((reply (http-put uri
			  headers: headers
			  data: data))
	 (status (request-status reply))
	 (text (request-text reply)))

    (if (success? status)
      (displayln text)
      (displayln (format "Failure on post. Status:~a Text:~a~%" status text)))))

(def (from-json json)
  (try
   (with-input-from-string json read-json)
   (catch (e)
     (displayln "error parsing json " e))))
