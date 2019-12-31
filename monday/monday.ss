;; -*- Gerbil -*-

(import
  :gerbil/gambit
  :scheme/base
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
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
  :ober/oberlib
  :ober/monday/client)

(export main)

(declare (not optimize-dead-definitions))

(def program-name "monday")

(def interactives
  (hash
   ("board" (hash (description: "Get information board") (usage: "board <id of board>") (count: 1)))
   ("boards" (hash (description: "List All Boards") (usage: "boards") (count: 0)))
   ("bp" (hash (description: "Get Pulses for Board") (usage: "bp <id of board>") (count: 1)))
   ("columns" (hash (description: "List all columns for a board") (usage: "columns <board id>") (count: 1)))
   ("config" (hash (description: "Configure credentials for app.") (usage: "config") (count: 0)))
   ("get-board-id" (hash (description: "Find boardid for a pattern.") (usage: "get-boardid <partial string>") (count: 1)))
   ("get-group-id" (hash (description: "Find userid for a pattern.") (usage: "get-group-id <board partial> <group partial>") (count: 2)))
   ("update-timeline" (hash (description: "Update timeline for pulse.") (usage: "update-timeline <board name/num> <pulse name/num> <from date> <to date>") (count: 4)))
   ("get-pulse-id" (hash (description: "Find pulse-id for a pattern.") (usage: "get-pulse-id <partial string>") (count: 1)))
   ("get-userid" (hash (description: "Find userid for a pattern.") (usage: "get-userid <partial string>") (count: 1)))
   ("groups" (hash (description: "List all groups for a board") (usage: "groups <board id>") (count: 1)))
   ("group" (hash (description: "Get tasks for group id") (usage: "group <board id/pat> <group id/pat>") (count: 2)))
   ("mgroup" (hash (description: "Modify group title") (usage: "mgroup <board id> <gid> <title>") (count: 3)))
   ("ngroup" (hash (description: "Create a new group for board") (usage: "ngroup <board id> <title>") (count: 2)))
   ("notes" (hash (description: "List all notes for a pulse") (usage: "notes <pulse id>") (count: 1)))
   ("npulse" (hash (description: "Create new pulse for Board") (usage: "npulse <id of board> <userid> <group id> <name of pulse>") (count: 4)))
   ("pulse" (hash (description: "List info on pulse") (usage: "pulse <pulse id>") (count: 1)))
   ("pulses" (hash (description: "List all pulses") (usage: "pulses") (count: 0)))
   ("ufeed" (hash (description: "Get feed for user.") (usage: "feed <user pattern>") (count: 1)))
   ("updates" (hash (description: "List all updates") (usage: "updates") (count: 0)))
   ("uposts" (hash (description: "Get posts for user.") (usage: "uposts <user pattern>") (count: 1)))
   ("user" (hash (description: "Get information on user.") (usage: "user <user pattern>") (count: 1)))
   ("users" (hash (description: "List all users.") (usage: "users") (count: 0)))
   ("uunread" (hash (description: "Get unread news items for user.") (usage: "uuread <user pattern>") (count: 1)))
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
      (apply (eval (string->symbol (string-append "ober/monday/client#" verb))) args2))))

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
