;;; notinfo-search.scm -- usage: guile notinfo-search.scm DBPATH [FLAGS] QUERY...
;;;
;;; Flags:
;;;   --debug          extra information on stderr
;;;   --max-width=N    paragraph width in characters (default 80);
;;;                    N of -1/inf/unlimited means the whole paragraph
;;;   --point          print the match's offset within its node instead of
;;;                    the context
;;;   --context[=N]    the match plus N symbols either side (default 1)
;;;   --limit[=N]      number of search results to return
;;;                    N of -1/0/inf/unlimited means return all results
;;;   --view '(M)NODE' print one node's stored text and exit
;;;
;;; One line per hit:
;;;   (tar)Changes: ... matching text ...
;;; with --point:
;;;   (tar)Changes: 388
;;;
;;; Query language: manual:, node: (and its alias title:), chapter: (and its
;;; alias section:), menu:, body:, combinable with AND, OR, parentheses and
;;; quotes.
;;;
;;;   manual:tar AND chapter:6 AND body:globbing
;;;
;;; node: and chapter: match a node's own name and number and every
;;; ancestor's, so chapter:6 means "6 and everything under it" and
;;; node:regexp means "this node, or any ancestor, is about regexps".  Bare
;;; words search a whole manual.
;;;
;;; --point counts characters into the node's stored text, whose position 0
;;; is the first column of the node's header line -- exactly what --view
;;; prints.

(use-modules (ice-9 format)
             (ice-9 match)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-14)
             (xapian xapian))

;;; ---- sidecars ----------------------------------------------------------

(define (slurp-lines port)
  (let loop ((acc '()) (line (read-line port)))
    (if (eof-object? line) (reverse acc) (loop (cons line acc) (read-line port)))))

(define (read-lines file)
  (if (file-exists? file)
      (call-with-input-file file
        (lambda (port)
          (filter (lambda (s) (not (string-null? s)))
                  (map string-trim-both (slurp-lines port)))))
      '()))

(define (stem-name dbpath)
  (let ((names (read-lines (string-append dbpath ".stem"))))
    (if (null? names) "en" (car names))))

(define (stemmer-for name)
  (catch #t (lambda () (make-stem name)) (lambda _ (make-stem "none"))))

;;; ---- finding words in stored text --------------------------------------

(define (first-span text needle start)
  (call-with-values
      (lambda () (string-contains-ci text needle start))
    (lambda args
      (cond ((null? args) #f)
            ((not (car args)) #f)
            ((pair? (cdr args)) (cons (car args) (cadr args)))
            (else (cons (car args) (+ (car args) (string-length needle))))))))

;; The word, then progressively shorter prefixes: the stemmer matched them
;; even though the literal strings differ.
(define (word-candidates w)
  (let ((n (string-length w)))
    (append (list w)
            (if (<= 7 n) (list (substring w 0 6)) '())
            (if (<= 6 n) (list (substring w 0 5)) '()))))

(define (find-match text words start)
  (any (lambda (w)
         (any (lambda (c) (first-span text c start))
              (word-candidates w)))
       words))

;; Boolean operators are not words to look for, and neither is anything from a
;; manual: field: every node in a manual has that manual's name in its header
;; line, so searching for it would report the top of every node rather than the
;; match, and would poison --point the same way.
(define query-operators '("AND" "OR" "NOT" "XOR"))

(define (query-words query)
  (let* ((raw (string-tokenize query char-set:graphic))
         (stripped
          (filter-map
           (lambda (w)
             (let ((colon (string-index w #\:)))
               (cond
                ((member w query-operators) #f)
                ((and colon (string=? (substring w 0 colon) "manual")) #f)
                (colon (substring w (+ colon 1)))
                (else w))))
           raw))
         (words (append-map (lambda (w)
                              (let ((i (string-index w #\-)))
                                (if i (list w (substring w 0 i)) (list w))))
                            stripped)))
    (sort (delete-duplicates
           (filter (lambda (w) (<= 2 (string-length w))) words))
          (lambda (a b) (< (string-length b) (string-length a))))))

;;; ---- context -----------------------------------------------------------

(define no-limit 100000000)

;; Accepts N, inf, infinity, infinite, unlimited.  Zero or negative means no
;; limit: a search for zero matches is never what you want.
(define (parse-limit s)
  (cond ((member s '("inf" "infinity" "infinite" "unlimited")) no-limit)
        ((string->number s)
         => (lambda (n) (let ((n (inexact->exact (truncate n))))
                          (if (<= n 0) no-limit n))))
        (else (format (current-error-port)
                      "warning: bad --limit=~a; showing all matches~%" s)
              no-limit)))

(define (parse-max-width s)
  (cond ((member s '("inf" "infinity" "infinite" "unlimited")) no-limit)
        ((string->number s)
         => (lambda (n) (let ((n (inexact->exact (truncate n))))
                          (if (<= n 0) no-limit n))))
        (else (format (current-error-port)
                      "warning: bad --max-width=~a; using 80~%" s)
              80)))

(define (flatten s)
  (string-trim-both
   (regexp-substitute/global #f (make-regexp "[ \t\n\r]+") s 'pre " " 'post)))

(define (blank-before text pos)
  (let loop ((i (min pos (string-length text))))
    (cond ((<= i 0) 0)
          ((and (< 1 i)
                (char=? (string-ref text (- i 1)) #\newline)
                (char=? (string-ref text (- i 2)) #\newline))
           i)
          (else (loop (- i 1))))))

(define (blank-after text pos)
  (let ((n (string-length text)))
    (let loop ((i pos))
      (cond ((<= (- n 1) i) n)
            ((and (char=? (string-ref text i) #\newline)
                  (char=? (string-ref text (+ i 1)) #\newline))
             (+ i 1))
            (else (loop (+ i 1)))))))

(define (paragraph text pos max-width)
  (let* ((n (string-length text))
         (lo0 (max (blank-before text pos) 0))
         (hi0 (min (blank-after text pos) n)))
    (if (<= no-limit max-width)
        (flatten (substring text lo0 hi0))
        (let* ((half (quotient max-width 2))
               (lo (max lo0 (- pos half)))
               (hi (min hi0 (max (+ pos 1) (+ lo max-width)))))
          (string-append
           (if (< lo0 lo) "..." "")
           (flatten (substring text lo hi))
           (if (< hi hi0) "..." ""))))))

(define symbol-chars
  (char-set-union char-set:letter+digit (string->char-set "_-/()")))

(define (symbol-bounds text pos)
  (let* ((n (string-length text))
         (lo (let loop ((i (min pos n)))
               (if (and (< 0 i)
                        (char-set-contains? symbol-chars (string-ref text (- i 1))))
                   (loop (- i 1))
                   i)))
         (hi (let loop ((i (min pos n)))
               (if (and (< i n)
                        (char-set-contains? symbol-chars (string-ref text i)))
                   (loop (+ i 1))
                   i))))
    (cons lo hi)))

(define (prev-symbol-start text pos)
  (let loop ((i (min pos (string-length text))))
    (cond ((<= i 0) 0)
          ((char-set-contains? symbol-chars (string-ref text (- i 1)))
           (car (symbol-bounds text (- i 1))))
          (else (loop (- i 1))))))

(define (next-symbol-end text pos)
  (let ((n (string-length text)))
    (let loop ((i (max pos 0)))
      (cond ((<= n i) n)
            ((char-set-contains? symbol-chars (string-ref text i))
             (cdr (symbol-bounds text i)))
            (else (loop (+ i 1)))))))

(define (words-around text start end n)
  (let ((lo (let loop ((i start) (k n))
              (if (<= k 0) i (loop (prev-symbol-start text i) (- k 1)))))
        (hi (let loop ((i end) (k n))
              (if (<= k 0) i (loop (next-symbol-end text i) (- k 1))))))
    (flatten (substring text lo hi))))

;;; ---- documents ---------------------------------------------------------

(define (doc-record item)
  (call-with-input-string (document-data (mset-item-document item)) read))

;; All documents of one manual.  The query parser tokenizes punctuation that
;; node names may contain, so filtering the manual's documents by exact
;; string is both simpler and exact.
(define (manual-nodes db manual)
  (let* ((q (parse-query (string-append "manual:" manual)
                         #:stemmer (make-stem "none")
                         #:prefixes '(("manual" . "XM"))))
         (mset (enquire-mset (enquire db q) #:maximum-items 1000000)))
    (mset-fold (lambda (item acc) (cons (doc-record item) acc)) '() mset)))

;; "(tar)Changes" -> ("tar" . "Changes")
(define (parse-node-spec spec)
  (let* ((s (string-trim-both spec))
         (close (and (< 0 (string-length s))
                     (char=? (string-ref s 0) #\()
                     (string-index s #\)))))
    (if close
        (cons (substring s 1 close)
              (string-trim-both (substring s (+ close 1))))
        (cons #f s))))

(define (find-node db spec)
  (let* ((parsed (parse-node-spec spec))
         (manual (car parsed))
         (name (cdr parsed)))
    (and manual
         (find (lambda (rec) (string=? (assoc-ref rec 'node) name))
               (manual-nodes db manual)))))

;;; ---- search ------------------------------------------------------------

(define* (search dbpath querystring #:key (limit no-limit) (debug? #f)
                 (max-width 80) (point? #f) (context #f))
  (let* ((sname (stem-name dbpath))
         (terms (query-words querystring))
         ;; All of these are content prefixes, not filters, so the query's own
         ;; AND/OR structure is honoured: a boolean prefix would be AND-ed
         ;; with the rest regardless of what was written.
         (prefixes '(("manual"  . "XM")
                     ("node"    . "XN")
                     ("title"   . "XN")
                     ("chapter" . "XC")
                     ("section" . "XC")
                     ("menu"    . "XE")
                     ("body"    . "XB"))))
    (when debug?
      (format (current-error-port)
              "stemmer: ~a; query: ~s; terms: ~s; context: ~a~%"
              sname querystring terms context))
    (call-with-database dbpath
      (lambda (db)
        (let* ((query (parse-query querystring
                                   #:stemmer (stemmer-for sname)
                                   #:prefixes prefixes
                                   #:boolean? #t))
               (mset (enquire-mset (enquire db query) #:maximum-items limit))
               (hits
                (reverse
                 (mset-fold
                  (lambda (item acc)
                    (let* ((record (doc-record item))
                           (manual (assoc-ref record 'manual))
                           (node (assoc-ref record 'node))
                           (text (assoc-ref record 'text))
                           (label (format #f "(~a) ~a" manual node)))
                      (cons (list label text) acc)))
                  '()
                  mset)))
               (pad (if (null? hits) 0
                        (apply max (map (lambda (h) (string-length (car h))) hits)))))
          (for-each
           (lambda (hit)
             (match-let (((label text) hit))
               ;; Search the stored text, so the offset is the one --view and
               ;; (forward-char) agree on.
               (let* ((span (find-match text terms 0))
                      (found (or (and span (car span)) 0)))
                 (if point?
                     (format #t "~va: ~a~%" pad label found)
                     (format #t "~va: ~a~%" pad label
                             (if (and context span)
                                 (words-around text (car span) (cdr span) context)
                                 (paragraph text found max-width)))))))
           hits)
          (when debug?
            (format (current-error-port) "~a hits~%" (length hits))))))))

;;; ---- command line ------------------------------------------------------

(define (usage program)
  (format (current-error-port)
          "Usage: ~a DBPATH [--debug] [--max-width=N] [--point] [--context[=N]] [--limit[=N]] QUERY...~%\
       ~a DBPATH --view '(MANUAL)NODE'~%"
          program program))

(define (split-flags args)
  (let loop ((as args) (debug? #f) (max-width 80) (point? #f)
             (context #f) (limit no-limit) (view #f) (terms '()))
    (match as
      (() (values debug? max-width point? context limit view (reverse terms)))
      ((a . rest)
       (cond ((string=? a "--debug")
              (loop rest #t max-width point? context limit view terms))
             ((string=? a "--point")
              (loop rest debug? max-width #t context limit view terms))
             ((string=? a "--context")
              (loop rest debug? max-width point? 1 limit view terms))
             ((string=? a "--view")
              (loop rest debug? max-width point? context limit
                    (if (pair? rest) (car rest) #f)
                    (if (pair? rest) (cdr rest) '())))
             ((string-match "^--view=(.*)$" a)
              => (lambda (m)
                   (loop rest debug? max-width point? context limit
                         (match:substring m 1) terms)))
             ((string-match "^--context=(.*)$" a)
              => (lambda (m)
                   (let ((k (string->number (match:substring m 1))))
                     (loop rest debug? max-width point?
                           (if (and k (< 0 k)) (inexact->exact (truncate k)) 1)
                           limit view terms))))
             ((string-match "^--limit=(.*)$" a)
              => (lambda (m)
                   (loop rest debug? max-width point? context
                         (parse-limit (match:substring m 1)) view terms)))
             ((string-match "^--max-width=(.*)$" a)
              => (lambda (m)
                   (loop rest debug? (parse-max-width (match:substring m 1))
                         point? context limit view terms)))
             (else (loop rest debug? max-width point? context limit view
                         (cons a terms))))))))

(match (command-line)
  ((_ dbpath . rest)
   (call-with-values (lambda () (split-flags rest))
     (lambda (debug? max-width point? context limit view terms)
       (cond
        (view
         (call-with-database dbpath
           (lambda (db)
             (let ((rec (find-node db view)))
               (if rec
                   (display (assoc-ref rec 'text))
                   (begin
                     (format (current-error-port)
                             "no such node: ~a (want '(MANUAL)NODE)~%" view)
                     (exit 1)))))))
        ((null? terms) (usage (car (command-line))))
        (else
         (search dbpath (string-join terms)
                 #:debug? debug? #:max-width max-width
                 #:point? point? #:context context))))))
  ((program . _) (usage program)))
