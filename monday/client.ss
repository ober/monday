;; -*- Gerbil -*-
;; Gerbil Library for Monday.com
;; ©ober 2020

(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/db/dbi
  :std/debug/heap
  :std/iter
  :std/error
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/misc/channel
  :std/misc/list
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/net/uri
  :std/pregexp
  :std/srfi/1
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :std/text/zlib
  :ober/oberlib
  :std/xml/ssax)

(export #t)
(def version "0.01")
(declare (not optimize-dead-definitions))
(def program-name "monday")
(def config-file "~/.monday.yaml")

(def display-format "org-mode")
(def monday-company-key #f)
(def monday-personal-key #f)

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(import (rename-in :gerbil/gambit/os (time mytime)))

(def (default-headers)
  [["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]])

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
  (hash->str t))

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

(def (user upat)
  (let* ((uid (get-userid upat))
	 (user (monday-get (format "users/~a.json" uid) [])))
    (when (table? user)
      (pi user))))

(def (uposts upat)
  (let ((uid (get-userid upat)))
    (print-updates
     (monday-get (format "users/~a/posts.json" uid) []))))

(def (ufeed upat)
  (let ((uid (get-userid upat)))
    (print-updates
     (monday-get (format "users/~a/newsfeed.json" uid) []))))

(def (uunread upat)
  (let ((uid (get-userid upat)))
    (print-updates
     (monday-get (format "users/~a/unread_feed.json" uid) []))))

(def (users)
  (let-hash (load-config)
    (let ((url (make-monday-url "users.json" .?monday-personal-key))
          (outs [[ "Name" "Id" "Email" "Title" "Position" "Created" "Updated" ]]))
      (with ([status body] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (list? body)
          (for (user body)
            (when (table? user)
              (let-hash user
                (set! outs (cons [ .?name .?id .?email .?title .?position .?created_at .?updated_at ] outs)))))))
      (style-output outs))))


(def (print-board b)
  (let-hash b
    (displayln "|" .name
	       "|" .id
	       "|" .description
	       "|" .url
	       "|" (print-columns .columns)
	       "|" (print-groups .groups)
	       "|" .created_at
	       "|" .updated_at)))

(def (print-columns columns)
  (let ((out []))
    (when (list? columns)
      (for-each
	(lambda (c)
	  (when (table? c)
	    (let-hash c
	      (when .?labels
		(set! out (flatten (cons out (hash->str .labels))))))))
	columns))
    out))

(def (print-groups groups)
  (let ((out []))
    (when (list? groups)
      (for-each
	(lambda (g)
	  (set! out (flatten (cons out (hash->str g)))))
	groups))
    out))

(def (get-userid pattern)
  (let ((found #f)
	(users (monday-get "users.json" [])))
    (for-each
      (lambda (u)
	(let-hash u
	  (if (or
		(pregexp-match (format "(?i:~a)" pattern) .name)
		(pregexp-match  (format "(?i:~a)" pattern) .email))
	    (set! found .id))))
      users)
    found))

(def (get-board-id pattern)
  (if (or (number? pattern) (string->number pattern))
    pattern
    (begin
      (let ((found #f)
	    (bs (monday-get "boards.json" [])))
	(for-each
	  (lambda (b)
	    (let-hash b
	      (when (pregexp-match pattern .name)
		(set! found .id))))
	  bs)
	found))))

(def (get-pulse-id pat)
  (if (or (number? pat) (string->number pat))
    pat
    (let ((found #f)
	  (bs (monday-get "pulses.json&all_columns=false" [])))
      (for-each
	(lambda (b)
	  (let-hash b
	    (when (pregexp-match pat .name)
	      (set! found .id))))
	bs)
      found)))

;; (def (get-column-id bpat cpat)
;;   (let ((bid (get-board-id bpat)))
;;     (if (or (number? cpat) (string->number cpat))
;;       cpat
;;       (let ((found #f)
;; 	    (columns (monday-get (format "boards/~a/columns.json" bid) [])))
;; 	(when (table? columns)
;; 	  (let-hash columns
;; 	    (when (pregexp-match cpat .title)

(def (get-group-id bpat gpat)
  (let* ((bid (get-board-id bpat))
	 (found #f)
	 (bs (monday-get (format "boards/~a/groups.json" bid) [])))
    (for-each
      (lambda (b)
	(let-hash b
	  (when (pregexp-match gpat .title)
	    (set! found .id))))
      bs)
    found))

(def (pulses)
  (displayln "| Name | Id | Subscribers | Created | Updated |")
  (displayln "|-|")
  (for-each
    (lambda (p)
      (print-pulse p))
    (monday-get "pulses.json" [])))

(def (pulse id)
  (displayln "| Name | Id | Subscribers | Created | Updated |")
  (displayln "|-|")
  (print-pulse (monday-get (format "pulses/~a.json" id) [])))

(def (print-pulses pls)
  (displayln "| Name | Id | Subscribers | Created | Updated |")
  (displayln "|-|")
  (displayln pls)
  (when (list? pls)
    (for-each
      (lambda (u)
	(print-pulse u))
      pls)))

(def (print-pulse pulse)
  (when (table? pulse)
    (let-hash pulse
      (displayln "|" .?name
		 "|" .?id
		 "|" (when (and .?subscribers (table? .subscribers)) (hash-ref .subscribers 'name))
		 "|" .?created_at
		 "|" .?updated_at
		 "|"))))

(def (print-updates updates)
  (displayln "| User | Url | Id | Body | kind | has_assets | assets | created_at | updated_at |")
  (displayln "|-|")
  (when (list? updates)
    (for-each
      (lambda (u)
	(print-update u))
      updates)))

(def (print-update update)
  (when (table? update)
    (let-hash update
      (displayln "|" (when (and .?user (table? .user)) (let-hash .user (format "~a: ~a" .name .email)))
		 "|" .url
		 "|" .id
		 "|" .body
		 "|" .kind
		 "|" .has_assets
		 "|" .assets
		 "|" .created_at
		 "|" .updated_at "|"))))

(def (notes pat)
  (let ((pid (get-pulse-id pat)))
    (displayln "| Type | Id | Title | Project id| Permissions | Created | Updated |")
    (displayln "|-|")
    (for-each
      (lambda (n)
	(print-note n))
      (monday-get (format "pulses/~a/notes.json" pid) []))))

(def (add-note bpat ppat)
  (let ((bid (get-board-id bpat))
	(ppat (get-pulse-id ppat)))
    (displayln "not yet")))

;; (def (note pid nid)
;;   (displayln "| Type | Id | Title | Project id| Permissions | Created | Updated |")
;;   (displayln "|-|")
;;     (monday-get (format "pulses/~a/notes/~a.json" pid nid) [])))


(def (print-note note)
  (when (table? note)
    (let-hash note
      (displayln "|" .type
		 "|" .id
		 "|" .title
		 "|" .project_id
		 "|" .permissions
		 "|" .created_at
		 "|" .updated_at "|"))))

;; type (string, optional): The collaboration box type (rich_text, file_list, faq_list).,
;; id (string): The note's id.,
;; title (string): The note's title.,
;; project_id (string): The note's project_id.,
;; permissions (string, optional): Describes who can edit this note. Can be either 'everyone' or 'owners'.,
;; created_at (DateTime in ISO8601 format): Creation time.,
;; updated_at (DateTime in ISO8601 format): Last update time.

(def (group board grp)
  (let* ((bid (get-board-id board))
	 (gid (get-group-id bid grp)))
    (displayln (format "bid: ~a gid: ~a~%" bid gid))
    (displayln (monday-get (format "boards/~a/groups/~a.json" bid gid) []))))

(def (groups board)
  (let* ((bid (get-board-id board))
	 (groups (monday-get (format "boards/~a/groups.json" bid) [])))
    (displayln "| Title | Id | Pos | color|")
    (displayln "|-|")
    (for-each
      (lambda (g)
	(print-group g))
      groups)))

(def (print-group group)
  (when (table? group)
    (displayln "group: " (hash->list group))
    (let-hash group
      (unless .?deleted
	(displayln "|" .?title
		   "|" .?id
		   "|" .?pos
		   "|" .?color "|")))))

(def (columns bpat)
  (let ((bid (get-board-id bpat)))
    (show-tables (monday-get (format "boards/~a/columns.json" bid) []))))

(def (update-timeline bid pid from to)
  (let* ((bid (get-board-id bid))
	 (cid "timeline")
	 (pid (get-pulse-id pid))
	 (data (hash
		("from" from)
		("to" to)
		(pulse_id pid))))
    (displayln (monday-put (format "boards/~a/columns/~a/timeline.json" bid cid) data))))

(def (ngroup bid title)
  (let ((data (hash ("title" title))))
    (show-tables (monday-post (format "boards/~a/groups.json" bid) data))))

(def (mgroup board group title)
  (let* ((bid (get-board-id board))
	 (gid (get-group-id board group))
	 (data (hash
		("group_id" gid)
		("title" title))))
    (show-tables (monday-put (format "boards/~a/groups.json" bid) data))))

(def (npulse board user group name)
  (let* ((bid (get-board-id board))
	 (userid (get-userid user))
	 (gid (get-group-id board group))
	 (data (hash
		("user_id" userid)
		("group_id" gid)
		("pulse"
		 (hash
		  ("name" name))))))
    (show-tables (monday-post (format "boards/~a/pulses.json" bid) data))))

(def (boards)
  (displayln "|Name|Id|Description|Url|Columns|Groups|Created|Updated|")
  (displayln "|-|")
  (for-each
    (lambda (b)
      (print-board b))
    (monday-get "boards.json" [])))

(def (board board)
  (displayln "|Name|Id|Description|Url|Columns|Groups|Created|Updated|")
  (displayln "|-|")
  (let ((bid (get-board-id board)))
    (print-board (monday-get (format "boards/~a.json" bid) []))))

(def (print-boarditem bi)
  (when (table? bi)
    (let-hash bi
      (let-hash .pulse
	(let ((cv (display-column-values ..column_values)))
	  (displayln "|" .?id
		     "|" .?name
		     "|" (let ((timeline (get-column-by-name ..column_values "timeline")))
			   (when (table? timeline)
			     (let-hash timeline
			       (format "from: ~a to: ~a" .from .to))))
		     "|" (let ((status (get-column-by-name ..column_values "status")))
			   (when (table? status)
			     (let-hash status
			       .index)))
		     "|" .?created_at
		     "|" .?updated_at
		     "|" (hash-ref ..board_meta 'group_id)
		     "|" cv "|"))))))

;;(def (print-bi-pulse bip bm)
;;  (when (table? bip)
;;    (let-hash bip

(def (get-column-by-name cv name)
  (let ((results []))
    (when (list? cv)
      (for-each
	(lambda (c)
	  (let-hash c
	    (when (pregexp-match name .cid)
	      (when (table? .?value)
		(set! results .?value)))))
	cv))
    results))

(def (display-column-values cv)
  (when (list? cv)
    (let ((out []))
      (for-each
	(lambda (c)
	  (let-hash c
	    (set! out (flatten (cons out (format "~a: ~a," (or .?title .?name) (if (table? .?value) (hash->str .?value) .?value)))))))
	cv)
      (string-join out " "))))

(def (bp board)
  (displayln "| Pulse id | Name | Timeline| Status | Created | Updated | Columns |")
  (displayln "|-|")
  (let* ((bid (get-board-id board))
	 (bis (monday-get (format "boards/~a/pulses.json" bid) [])))
    (for-each
      (lambda (bi)
	(print-boarditem bi))
      bis)))

(def (updates)

  (print-updates
   (monday-call "updates.json" "get" [])))

(def (monday-get adds data)
  (monday-call adds "get" data))

(def (monday-put adds data)
  (monday-call adds "put" data))

(def (monday-post adds data)
  (monday-call adds "post" data))

(def (make-monday-url adds .monday-personal-key)
  (format "https://api.monday.com/v1/~a?api_key=~a" adds .monday-personal-key))

(def (monday-call adds type data)
  (let-hash (load-config)
    (let* ((uri (format "https://api.monday.com/v1/~a?api_key=~a" adds .monday-personal-key))
	   (headers [
		     ["Accept" :: "*/*"]
		     ["Content-type" :: "application/json"]
		     ])
	   (results (cond
                     ((string= type "get")
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
