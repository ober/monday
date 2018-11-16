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
;;(def header #f)

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(def program-name "monday")
(def DEBUG (getenv "DEBUG" #f))
(def (dp msg)
  (when DEBUG
    (displayln msg)))

(def interactives
  (hash
   ("board" (hash (description: "Get information board") (usage: "board <id of board>") (count: 1)))
   ("boards" (hash (description: "List All Boards") (usage: "boards") (count: 0)))
   ("bp" (hash (description: "Get Pulses for Board") (usage: "bp <id of board>") (count: 1)))
   ("columns" (hash (description: "List all columns for a board") (usage: "columns <board id>") (count: 1)))
   ("config" (hash (description: "Configure credentials for app.") (usage: "config") (count: 0)))
   ("groups" (hash (description: "List all groups for a board") (usage: "groups <board id>") (count: 1)))
   ("mgroup" (hash (description: "Modify group title") (usage: "mgroup <board id> <gid> <title>") (count: 3)))
   ("ngroup" (hash (description: "Create a new group for board") (usage: "ngroup <board id> <title>") (count: 2)))
   ("npulse" (hash (description: "Create new pulse for Board") (usage: "npulse <id of board> <userid> <group id> <name of pulse>") (count: 4)))
   ("pulses" (hash (description: "List all pulses") (usage: "pulses") (count: 0)))
   ("updates" (hash (description: "List all updates") (usage: "updates") (count: 0)))
   ("users" (hash (description: "List all users.") (usage: "users") (count: 0)))
   ("get-userid" (hash (description: "Find userid for a pattern.") (usage: "get-userid <partial string>") (count: 1)))
   ("get-boardid" (hash (description: "Find boardid for a pattern.") (usage: "get-boardid <partial string>") (count: 1)))
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
  (define header #f)
  (for-each
    (lambda (t)
      (format-me t header))
    tables))

(def (show-table t)
  (define header #f)
  (format-me t header))

(def (format-me t header)
  (cond
   ((string=? display-format "org-mode")
    (format-org t header))
   ((string=? display-format "raw")
    (format-raw t header))
   (else
    (format-raw t header))))

(def (print-elt-table t header)
  (stringify-hash t))

(def (print-elt-list l header)
  (for-each
    (lambda (elt)
      (format-me elt header))
    l))

(def (print-elt e header)
  ;;  (displayln "here: " (type-of e))
  (cond
   ((table? e)
    (print-elt-table e header))
   ((list? e)
    (print-elt-list e header))
   ((string? e)
    e)
   ((symbol? e)
    (symbol->string e))
   ((flonum? e)
    e)
   ((boolean? e)
    e)
   ((fixnum? e)
    e)
   (else
    (format "to:~a" (type-of e)))))

(def (format-org t header)
  (unless header
    (set! header []))
  (hash-for-each
   (lambda (k v)
     (unless (member k header)
       (set! header (flatten (cons header k))))
     (display (format "|~a" (print-elt v header))))
   t)
  (displayln "|"))

(def (format-raw t header)
  (displayln (hash->list t)))

(def (users)
  (displayln "|Name|id|email|title|position|created_at|updated_at|")
  (displayln "|-|")
  (for-each
    (lambda (user)
      (print-user user))
    (monday-call "users.json" "get" [])))

(def (print-user user)
  (let-hash user
    (displayln "|" .name
	       "|" .id
	       "|" .email
	       "|" .title
	       "|" .position
	       "|" .created_at
	       "|" .updated_at)))


(def (print-board b)
  (let-hash b
    (displayln "|" .name
	       "|" .id
	       "|" .description
	       "|" .url
	       "|" .columns
	       "|" .groups
	       "|" .created_at
	       "|" .updated_at)))

(def (get-userid pattern)
  (let ((found #f)
	(users (monday-get "users.json" [])))
    (for-each
      (lambda (u)
	(let-hash u
	  (if (or
		(pregexp-match pattern .name)
		(pregexp-match pattern .email))
	    (set! found .id))))
      users)
    found))

(def (get-boardid pattern)
  (let ((found #f)
	(bs (monday-get "boards.json" [])))
    (for-each
      (lambda (b)
	(let-hash b
	  (when (pregexp-match pattern .name)
	    (set! found .id))))
      bs)
    (displayln found)))

(def (pulses)
  (show-tables (monday-call "pulses.json" "get" [])))

(def (groups bid)
  (show-tables (monday-get (format "boards/~a/groups.json" bid) [])))

(def (columns bid)
  (show-tables (monday-get (format "boards/~a/columns.json" bid) [])))

(def (ngroup bid title)
  (let ((data (hash ("title" title))))
    (show-tables (monday-post (format "boards/~a/groups.json" bid) data))))

(def (mgroup bid gid title)
  (let ((data (hash
	       ("group_id" gid)
	       ("title" title))))
  (show-tables (monday-put (format "boards/~a/groups.json" bid) data))))

(def (npulse board user gid name)
  (let* ((userid (get-userid user))
	 (data (hash
		("user_id" user)
		("group_id" gid)
		("pulse"
		 (hash
		  ("name" name))))))
    (show-tables (monday-post (format "boards/~a/pulses.json" board) data))))

(def (boards)
  (displayln "|Name|Id|Description|Url|Columns|Groups|Created|Updated|")
  (displayln "|-|")
  (for-each
    (lambda (b)
      (print-board b))
    (monday-get "boards.json" [])))

(def (board id)
  (show-table (monday-call (format "boards/~a.json" id) "get" [])))

(def (bp id)
  (show-tables (monday-call (format "boards/~a/pulses.json" id) "get" [])))

(def (updates)
  (show-tables (monday-call "updates.json" "get" [])))

(def (monday-get adds data)
  (monday-call adds "get" data))

(def (monday-put adds data)
  (monday-call adds "put" data))

(def (monday-post adds data)
  (monday-call adds "post" data))

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
			  ((string= type "post")
			   (do-post uri headers (json-object->string data)))
			  (else
			   (displayln "unsupported type: " type))))
	   (myjson (from-json results)))
      (dp (format "uri is ~a" uri))
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

(def (do-post uri headers data)
  (let* ((reply (http-post uri
			   headers: headers
			   data: data))
	 (status (request-status reply))
	 (text (request-text reply)))
    (dp (print-curl "post" uri headers data))
    (if (success? status)
      text
      (displayln (format "Error: Failure on a post. got ~a text: ~a~%" status text)))))


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

(def (stringify-hash h)
  (let ((results []))
    (if (table? h)
      (begin
	(hash-for-each
	 (lambda (k v)
	   (set! results (append results (list (format " ~a->" k) (format "~a   " v)))))
	 h)
	(append-strings results))
      ;;        (pregexp-replace "\n" (append-strings results) "\t"))
      "N/A")))

(def (flatten x)
  (cond ((null? x) '())
	((pair? x) (append (flatten (car x)) (flatten (cdr x))))
	(else (list x))))
