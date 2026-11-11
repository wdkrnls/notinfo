;;; info-index.scm -- index Info manuals into a Xapian database.
;;;
;;; usage: guile info-index.scm [--rebuild] [--lang=LANG]
;;;                             [--manual=NAME[,NAME...]] DBPATH
;;;        guile info-index.scm --render MANUAL
;;;
;;; info is the interface throughout: the set of manuals is what
;;; `info --output=-' lists, and a manual's text is what
;;; `info --subnodes --output=-' prints.  No file paths are involved, so it
;;; does not matter where the manuals are installed.
;;;
;;; A dir menu line is "* LABEL: (TARGET)." and info's menu lookup matches the
;;; LABEL, case-sensitively; when a name is not in the menu, info falls back to
;;; searching INFOPATH for NAME.info.  A dir entry whose label and file differ
;;; in case therefore resolves for neither spelling first tried -- "Mu4e" is
;;; not a menu item and "Mu4e.info" is not a file -- while "mu4e" finds
;;; mu4e.info.  render-entry tries the spellings we have and then their
;;; downcased forms.
;;;
;;; Nodes are located by their header lines, not by looking for \x1f, because
;;; `info --output' does not emit the separator.
;;;
;;; One Xapian document per node, holding the node's rendered text:
;;;
;;;   manual:              the label, the target, the target's leaf, and the
;;;                        target with its language suffix stripped
;;;   node:, title:        this node's name and every ancestor's, plus each
;;;                        one's display title with its number removed
;;;   chapter:, section:   this node's number and every ancestor's
;;;   menu:                this node's menu text
;;;   body:                this node's text, minus header line and menus
;;;
;;; Because every node carries its ancestors' names and numbers, node: and
;;; chapter: reach down the tree: chapter:6 means "6 and everything under it".

(use-modules (ice-9 format)
             (ice-9 ftw)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-14)
             (srfi srfi-26)
             (xapian xapian)
             (xapian wrap))

(define node-sep "\x1f")                ; Info's node separator

;;; ---- shell helpers -----------------------------------------------------

(define (shell-quote s)
  (string-append "'"
                 (regexp-substitute/global #f (make-regexp "'") s
                                           'pre "'\\''" 'post)
                 "'"))

(define (slurp-lines port)
  (let loop ((acc '()) (line (read-line port)))
    (if (eof-object? line) (reverse acc) (loop (cons line acc) (read-line port)))))

(define (slurp port) (string-join (slurp-lines port) "\n"))

(define (basename path)
  (let ((i (string-rindex path #\/))) (if i (substring path (+ i 1)) path)))

(define (split-on-char s ch)
  (let loop ((start 0) (acc '()))
    (let ((i (string-index s ch start)))
      (if i
          (loop (+ i 1) (cons (substring s start i) acc))
          (reverse (cons (substring s start) acc))))))

;; Run CMD through the shell and return its stdout as a list of lines.
(define (shell-lines cmd)
  (let ((port (open-input-pipe cmd)))
    (let ((lines (slurp-lines port))) (close-pipe port) lines)))

;; First index of NEEDLE in TEXT at or after START, or #f.  string-contains
;; returns two values, so normalise to one.
(define (first-index text needle start)
  (call-with-values
      (lambda () (string-contains text needle start))
    (lambda args (and (pair? args) (car args)))))

;; The manual entries listed in one root's dir.
(define (dir-entries root)
  (map (lambda (line)
         (let ((tab (string-index line #\tab)))
           (cons (substring line 0 tab) (substring line (+ tab 1)))))
       (shell-lines
        (format #f "INFOPATH=~a info --output=- 2>/dev/null | gawk ~a | sort -u"
                (shell-quote root) (shell-quote manual-awk)))))

;;; ---- the roots on the info search path ---------------------------------

(define (env-nonempty name)
  (let ((v (getenv name))) (and v (not (string-null? v)) v)))

;; Directories to look for dir files in, in the order info itself would use:
;; INFOPATH, then the current guix environment, then infopath(1)'s answer,
;; then the usual roots.  Only those that actually hold a dir file count.
(define (info-roots)
  (let* ((home (getenv "HOME"))
         (from-env
          (if (env-nonempty "INFOPATH")
              (filter (lambda (d) (not (string-null? d)))
                      (split-on-char (getenv "INFOPATH") #\:))
              '()))
         (from-command
          (let ((lines (shell-lines "infopath 2>/dev/null")))
            (if (pair? lines)
                (filter (lambda (d) (not (string-null? d)))
                        (split-on-char (car lines) #\:))
                '())))
         (candidates
          (append from-env
                  (if (env-nonempty "GUIX_ENVIRONMENT")
                      (list (string-append (getenv "GUIX_ENVIRONMENT") "/share/info"))
                      '())
                  from-command
                  (list "/usr/share/info")
                  (if home
                      (list (string-append home "/.guix-profile/share/info")
                            (string-append home "/.config/guix/current/share/info"))
                      '()))))
    (delete-duplicates
     (filter (lambda (d)
               (and (not (string-null? d))
                    (file-exists? (string-append d "/dir"))))
             candidates)
     string=?)))

;;; ---- the manuals info knows about --------------------------------------

;; A manual reference in a dir menu is "* LABEL: (TARGET)." -- the closing
;; paren is followed by a period.  A cross-reference to a node within a manual
;; looks like "* Invoking tar: (tar)Invoking tar." and would otherwise be
;; captured with TARGET = "tar", inventing a manual named "Invoking tar" that
;; indexes all of tar's nodes a second time.
(define manual-awk
  "match($0, /^\\* ([^:]+): \\(([^)]+)\\)\\./, m) { print m[1] \"\\t\" m[2] }")

;; Every manual listed in every dir on the search path, one entry per target.
;; info reads only the FIRST dir it finds, so a manual listed in another one --
;; the Guix manual in ~/.config/guix/current/share/info, say -- is invisible to
;; `info --output=-'.  Pinning INFOPATH per root is the only way to see the
;; others.
;;
;; Deduplicated on the target rather than the (label . target) pair: the same
;; file may be listed under different labels in different dirs, and indexing it
;; twice would return every one of its nodes twice.  The first label seen wins,
;; which is the one from the earliest root.
(define (manual-entries)
  (let loop ((entries (append-map dir-entries (info-roots)))
             (seen '())
             (acc '()))
    (cond ((null? entries) (reverse acc))
          ((member (cdr (car entries)) seen)
           (loop (cdr entries) seen acc))
          (else
           (loop (cdr entries)
                 (cons (cdr (car entries)) seen)
                 (cons (car entries) acc))))))

;; "guix.es" -> ("guix" . "es"); "tar" -> ("tar" . #f)
(define (locale-suffix? s)
  (and (>= (string-length s) 2)
       (<= (string-length s) 5)
       (string-match "^[a-z][a-z](_[A-Z][A-Z])?$" s)
       #t))

(define (split-manual name)
  (let ((dot (string-rindex name #\.)))
    (if (and dot (< (+ dot 1) (string-length name))
             (locale-suffix? (substring name (+ dot 1))))
        (cons (substring name 0 dot) (substring name (+ dot 1)))
        (cons name #f))))

;; LANG filters on the target's suffix, where one exists.  `info --output=-'
;; already shows the dir for the current locale, so this is mostly for pinning
;; a database to one language.
(define (wanted-entry? lang entry)
  (let ((suffix (cdr (split-manual (cdr entry)))))
    (cond ((not lang) #t)
          ((string=? lang "all") #t)
          ((string=? lang "en") (not suffix))
          (else (and suffix (string=? suffix lang))))))

;; --manual= may name a manual by its label, its target, the target's leaf, or
;; the target with its language suffix stripped.
(define (entry-requested? entry manuals)
  (or (not manuals)
      (let ((label (car entry))
            (target (cdr entry)))
        (any (lambda (m)
               (or (string=? m label)
                   (string=? m target)
                   (string=? m (basename target))
                   (string=? m (car (split-manual target)))))
             manuals))))

;;; ---- rendering ---------------------------------------------------------

(define (render-manual name)
  (let ((port (open-input-pipe
               (format #f "info --subnodes --output=- ~a 2>/dev/null"
                       (shell-quote name)))))
    (let ((text (slurp port))) (close-pipe port) text)))

;; info's menu lookup is case-sensitive, and so is its fallback of searching
;; INFOPATH for NAME.info.  A dir entry whose label and file disagree in case
;; resolves for neither spelling we would try first, so try what we have and
;; then the downcased forms.
(define (render-entry entry)
  (let loop ((names (delete-duplicates
                     (list (car entry) (cdr entry)
                           (string-downcase (car entry))
                           (string-downcase (cdr entry)))
                     string=?)))
    (if (null? names)
        ""
        (let ((text (render-manual (car names))))
          (if (not (string-null? (string-trim-both text)))
              text
              (loop (cdr names)))))))

;; A string that changes whenever TEXT does.  cksum rather than a Scheme hash:
;; it is deterministic across processes, which Guile's `hash' is not, and a
;; signature that varied between runs would make every manual look changed.
(define (text-signature text)
  (let* ((tmp (format #f "/tmp/notinfo-text-~a" (getpid)))
         (port (begin
                 (call-with-output-file tmp
                   (lambda (out) (display text out)))
                 (open-input-pipe
                  (format #f "cksum ~a 2>/dev/null" (shell-quote tmp))))))
    (let ((line (read-line port)))
      (close-pipe port)
      (false-if-exception (delete-file tmp))
      (and (not (eof-object? line)) (string-trim-both line)))))

;;; ---- parsing a manual's text -------------------------------------------

(define info-fields '("File" "Node" "Next" "Prev" "Up" "Dir" "Menu" "Subfile"))

(define (field-name-at? header k)
  (any (lambda (name)
         (let ((pat (string-append name ":")))
           (and (<= (+ k (string-length pat)) (string-length header))
                (string=? (substring header k (+ k (string-length pat))) pat))))
       info-fields))

;; A field name begins the line or follows a comma, with info padding by a
;; variable number of spaces: "File: tar.info,  Node: Top,  Up: (dir)".
(define (field-start? header i)
  (cond ((= i 0) #t)
        (else
         (let loop ((j (- i 1)))
           (cond ((< j 0) #f)
                 ((char=? (string-ref header j) #\space) (loop (- j 1)))
                 ((char=? (string-ref header j) #\,) #t)
                 (else #f))))))

;; A header line as an alist of (field-name . value).  A value runs to the next
;; comma that is followed by a known field name, not merely to the next comma,
;; because node names may themselves contain ", ".
(define (parse-header header)
  (let ((n (string-length header)))
    (let loop ((i 0) (acc '()))
      (cond
       ((>= i n) (reverse acc))
       ((not (field-start? header i)) (loop (+ i 1) acc))
       (else
        (let* ((colon (string-index header #\: i))
               (name (and colon (substring header i colon))))
          (if (not (and name (member name info-fields)))
              (loop (+ i 1) acc)
              (let ((vend (let scan ((j (+ colon 1)))
                            (cond ((>= j n) j)
                                  ((char=? (string-ref header j) #\newline) j)
                                  ((and (char=? (string-ref header j) #\,)
                                        (let ((k (let skip ((k (+ j 1)))
                                                   (if (and (< k n)
                                                            (char=? (string-ref header k)
                                                                    #\space))
                                                       (skip (+ k 1))
                                                       k))))
                                          (field-name-at? header k)))
                                   j)
                                  (else (scan (+ j 1)))))))
                (loop vend
                      (cons (cons name (string-trim-both
                                        (substring header (+ colon 1) vend)))
                            acc))))))))))

;; The node name in an "Up:" value, or #f if it points outside this manual.
;; Values are plain names, or "(manual)Node"; Top's Up: is "(dir)".  A manual
;; may be referred to by either its label or its target.
(define (parent-name manual target up)
  (cond ((or (not up) (string-null? (string-trim-both up))) #f)
        ((string-prefix? "(" up)
         (let ((close (string-index up #\))))
           (and close
                (let ((file (substring up 1 close))
                      (rest (string-trim-both (substring up (+ close 1)))))
                  (and (or (string=? file manual) (string=? file target))
                       (not (string-null? rest))
                       rest)))))
        (else (string-trim-both up))))

;; "6 Choosing Files" -> ("6" . "Choosing Files"); "Appendix A Changes" ->
;; ("A" . "Changes"); "Changes" -> (#f . "Changes").
(define (split-heading h)
  (let ((m (string-match "^([0-9]+(\\.[0-9]+)*)[ \t]+(.*)$" h)))
    (cond
     (m (cons (match:substring m 1) (match:substring m 3)))
     (else
      (let ((m2 (string-match "^Appendix[ \t]+([A-Z])[ \t]+(.*)$" h)))
        (if m2
            (cons (match:substring m2 1) (match:substring m2 2))
            (cons #f h)))))))

;; Split TEXT into (body . menus): a menu block starts at "* Menu:" and runs
;; through the run of blank and "* "-prefixed lines that follows.  A wrapped
;; menu description is not "* "-prefixed, so it ends the block -- indexing a
;; stray line is better than eating body text.
(define (split-menus text)
  (let loop ((lines (split-on-char text #\newline))
             (body '()) (menus '()) (in-menu? #f))
    (if (null? lines)
        (cons (string-join (reverse body) "\n")
              (string-join (reverse menus) "\n"))
        (let* ((line (car lines))
               (trimmed (string-trim-both line)))
          (cond
           ((string=? trimmed "* Menu:")
            (loop (cdr lines) body (cons line menus) #t))
           (in-menu?
            (if (or (string-null? trimmed) (string-prefix? "* " trimmed))
                (loop (cdr lines) body (cons line menus) #t)
                (loop (cdr lines) (cons line body) menus #f)))
           (else (loop (cdr lines) (cons line body) menus #f)))))))

;; The display heading at the top of TEXT, with the rest of TEXT after it.
;; Returns three values: NUMBER, TITLE and the text following the heading.  The
;; heading is the first non-blank line when the line after it is a rule of
;; asterisks.
(define (take-heading text)
  (let loop ((ls (split-on-char text #\newline)))
    (cond
     ((null? ls) (values #f #f text))
     ((string-null? (string-trim-both (car ls))) (loop (cdr ls)))
     ((and (pair? (cdr ls))
           (let ((under (string-trim-both (cadr ls))))
             (and (<= 3 (string-length under))
                  (string-every (lambda (c) (char=? c #\*)) under))))
      (let ((h (split-heading (string-trim-both (car ls)))))
        (values (car h) (cdr h) (string-join (cddr ls) "\n"))))
     (else (values #f #f text)))))

;; The next node header at or after FROM, or #f.  A node header is a line
;; beginning with "File:" that also contains "Node:".  Nodes are located this
;; way rather than by looking for \x1f, because `info --output' does not emit
;; the separator.
;;
;; FROM >= the text length means there is nothing left to find; without this,
;; the final iteration of info-nodes would search from one past the end.
(define (next-header text from)
  (let ((n (string-length text)))
    (if (<= n from)
        #f
        (let loop ((i from))
          (let ((p (first-index text "File:" i)))
            (cond
             ((not p) #f)
             ((and (or (= p 0)
                       (char=? (string-ref text (- p 1)) #\newline)
                       (char=? (string-ref text (- p 1)) #\x1f))
                   (let* ((eol (or (string-index text #\newline p) n))
                          (line (substring text p eol)))
                     (first-index line "Node:" 0)))
              p)
             (else (loop (+ p 1)))))))))

;; Trailing whitespace and any node separator, so a node's stored text ends
;; with its own content.
(define tail-chars (char-set #\space #\tab #\newline #\return #\x1f))

(define (trim-tail s)
  (let loop ((n (string-length s)))
    (if (and (> n 0) (char-set-contains? tail-chars (string-ref s (- n 1))))
        (loop (- n 1))
        (substring s 0 n))))

;; Nodes of TEXT in reading order.  START is the node's own header line, so
;; position 0 of the stored text is that line's first column.
;;
;; LIMIT becomes N once the last node has been read, so the loop has to stop
;; on a start at or past the end as well as on #f.
(define (info-nodes text manual target)
  (let ((n (string-length text)))
    (let loop ((start (next-header text 0)) (acc '()))
      (if (or (not start) (>= start n))
          (reverse acc)
          (let* ((limit (or (next-header text (+ start 1)) n))
                 (raw (trim-tail (substring text start limit)))
                 (end (+ start (string-length raw)))
                 (he (or (string-index text #\newline start) end))
                 (fields (parse-header (substring text start (min he end))))
                 (name (assoc-ref fields "Node"))
                 (rest (if (< he end) (substring text (+ he 1) end) ""))
                 (head (call-with-values (lambda () (take-heading rest)) list))
                 (parts (split-menus (list-ref head 2))))
            (loop limit
                  (if (not name)
                      acc
                      (cons `((name . ,name)
                              (parent . ,(parent-name manual target
                                                      (assoc-ref fields "Up")))
                              (start . ,start)
                              (end . ,end)
                              (text . ,raw)
                              (number . ,(list-ref head 0))
                              (title . ,(list-ref head 1))
                              (body . ,(car parts))
                              (menus . ,(cdr parts)))
                            acc))))))))

;;; ---- hierarchy ---------------------------------------------------------

;; The node NAME and each of its ancestors, nearest first.
(define (lineage nodes name)
  (let ((index (map (lambda (nd) (cons (assoc-ref nd 'name) nd)) nodes)))
    (let loop ((n name) (acc '()) (seen '()) (limit 64))
      (if (or (<= limit 0) (not n) (member n seen))
          (reverse acc)
          (let ((nd (assoc-ref index n)))
            (if nd
                (loop (assoc-ref nd 'parent) (cons nd acc) (cons n seen) (- limit 1))
                (reverse acc)))))))

;;; ---- language and stemming ---------------------------------------------

(define (lang-base lang)
  (let loop ((i 0))
    (cond ((>= i (string-length lang)) lang)
          ((memv (string-ref lang i) '(#\_ #\.)) (substring lang 0 i))
          (else (loop (+ i 1))))))

(define (stem-name lang)
  (cond ((or (not lang) (string=? lang "en") (string=? lang "all")) "en")
        (else (lang-base lang))))

(define (stemmer-for lang)
  (catch #t
    (lambda () (make-stem (stem-name lang)))
    (lambda _ (make-stem "none"))))

;;; ---- timing ------------------------------------------------------------

(define (make-timer) (vector '()))

(define (timed timer label thunk)
  (let* ((t0 (get-internal-real-time))
         (value (thunk))
         (dt (/ (- (get-internal-real-time) t0)
                internal-time-units-per-second)))
    (vector-set! timer 0 (cons (cons label dt) (vector-ref timer 0)))
    value))

(define (report-slow what timer)
  (let* ((phases (reverse (vector-ref timer 0)))
         (total (apply + (map cdr phases))))
    (when (< 2.0 total)
      (format (current-error-port) "\nslow ~,1fs: ~a~%" total what)
      (for-each (lambda (p)
                  (format (current-error-port) "    ~10a ~,1fs~%" (car p) (cdr p)))
                phases)
      (force-output (current-error-port)))))

;;; ---- indexing ----------------------------------------------------------

(define max-term 240)

;; The unique term for one node.  Xapian caps terms at 245 bytes; the fallback
;; keeps a truncated name plus a hash, so the term stays unique and stable for
;; a given (manual, node), which is what makes purging work.
(define (node-idterm manual name)
  (let ((full (string-append "QN" manual node-sep name)))
    (if (<= (string-length full) max-term)
        full
        (string-append "QN" manual node-sep
                       (substring name 0 32) "-"
                       (number->string (string-hash name))))))

;; Index every node of the manual named by ENTRY, whose rendered text is TEXT.
;; Returns the list of node names found, or #f.
(define (index-manual db stem entry text)
  (let* ((manual (car entry))
         (target (cdr entry))
         (timer (make-timer))
         (nodes (timed timer "nodes" (lambda () (info-nodes text manual target)))))
    (if (null? nodes)
        (begin
          (format (current-error-port)
                  "no nodes found in ~a (~a bytes)~%" manual (string-length text))
          #f)
        (begin
          (timed timer "index"
                 (lambda ()
                   (for-each
                    (lambda (node)
                      (let* ((name (assoc-ref node 'name))
                             (idterm (node-idterm manual name))
                             (record `((manual . ,manual) (node . ,name)
                                       (text . ,(assoc-ref node 'text))))
                             (doc (make-document
                                   #:data (call-with-output-string
                                           (cut write record <>))
                                   #:terms (list (cons idterm 0))))
                             (gen (make-term-generator #:stem stem
                                                       #:document doc))
                             (kin (lineage nodes name))
                             (title (assoc-ref node 'title))
                             (body (assoc-ref node 'body))
                             (menus (assoc-ref node 'menus)))
                        ;; The label, the target, the target's leaf and its
                        ;; language-stripped form, so manual:mu4e,
                        ;; manual:Mu4e, manual:maxima and manual:abs_integrate
                        ;; all work.
                        (for-each (lambda (m) (index-text! gen m #:prefix "XM"))
                                  (delete-duplicates
                                   (list manual target
                                         (basename target)
                                         (car (split-manual target)))
                                   string=?))
                        ;; This node and every ancestor, by name and by number,
                        ;; so node: and chapter: reach descendants.
                        (for-each (lambda (nd)
                                    (index-text! gen (assoc-ref nd 'name)
                                                 #:prefix "XN")
                                    (let ((t (assoc-ref nd 'title))
                                          (k (assoc-ref nd 'number)))
                                      (when t (index-text! gen t #:prefix "XN"))
                                      (when k (index-text! gen k #:prefix "XC"))))
                                  kin)
                        (index-text! gen body #:prefix "XB")
                        (index-text! gen menus #:prefix "XE")
                        ;; Bare words search a whole manual: name, title, body
                        ;; and menus.
                        (index-text! gen name)
                        (when title (index-text! gen title))
                        (increase-termpos! gen)
                        (index-text! gen body)
                        (increase-termpos! gen)
                        (index-text! gen menus)
                        (replace-document! db idterm doc)))
                    nodes)))
          (report-slow manual timer)
          (map (lambda (nd) (assoc-ref nd 'name)) nodes)))))

;;; ---- manifest ----------------------------------------------------------

;; Bump whenever the terms a document carries change, or when what a document
;; holds changes.  The manifest is a memo of what is in the database, so a
;; change to how documents are built has to invalidate it.
;;   1: one document per node of one file, paths as identity
;;   2: one document per node, manual names as identity
;;   3: labels, not targets, are the manual name
;;   4: nodes located by header lines, not by \x1f
(define index-version 4)

(define (manifest-file dbpath) (string-append dbpath ".manifest"))

;; The entries alist, or '() if the manifest is missing or was written by a
;; different version of this program.  Entries are (manual signature nodes).
(define (read-manifest dbpath)
  (let ((f (manifest-file dbpath)))
    (if (file-exists? f)
        (let ((data (catch #t
                      (lambda () (call-with-input-file f read))
                      (lambda _ #f))))
          (cond ((not (and (pair? data) (pair? (cdr data)))) '())
                ((equal? (car data) index-version) (cadr data))
                (else
                 (format (current-error-port)
                         "index format changed; reindexing everything~%")
                 '())))
        '())))

(define (write-manifest dbpath entries)
  (let ((tmp (string-append (manifest-file dbpath) ".new")))
    (call-with-output-file tmp
      (lambda (port) (write (list index-version entries) port) (newline port)))
    (rename-file tmp (manifest-file dbpath))))

;; guile-xapian 0.6 exports no delete-document!; in git it is a one-line alias
;; for this SWIG binding, which takes the same unique term.
(define delete-document! WritableDatabase-delete-document)

(define (purge-node! db manual name)
  (catch #t
    (lambda () (delete-document! db (node-idterm manual name)))
    (lambda (key . args)
      (format (current-error-port) "could not purge ~a ~a: ~a ~s~%"
              manual name key args))))

(define (rm-rf path)
  (cond ((not (file-exists? path)) #t)
        ((file-is-directory? path)
         (for-each (lambda (f)
                     (rm-rf (string-append path "/" f)))
                   (scandir path (lambda (f) (not (member f '("." ".."))))))
         (rmdir path))
        (else (delete-file path))))

;;; ---- committing --------------------------------------------------------

;; guile-xapian 0.6 exports no commit; (xapian wrap) has the SWIG binding, the
;; same way it has WritableDatabase-delete-document.  Without it the whole run
;; is a single transaction: every document stays pending in memory until the
;; database is closed, and a crash anywhere loses the lot.
(define commit-database!
  (catch #t
    (lambda () (module-ref (resolve-module '(xapian wrap)) 'WritableDatabase-commit))
    (lambda _ #f)))

;;; ---- driver ------------------------------------------------------------

(define (index-all dbpath lang manuals)
  (let* ((lang (if lang (string-trim-both lang) "en"))
         (tty? (isatty? (current-error-port)))
         (all (manual-entries))
         (selected (filter (lambda (e)
                             (and (wanted-entry? lang e)
                                  (entry-requested? e manuals)))
                           all))
         (stem (stemmer-for lang))
         (total (length selected))
         (old (read-manifest dbpath))
         (counters (vector 0 0))
         (entries '())
         (n 0))
    (when tty?
      (format (current-error-port) "Manuals: ~a~%"
              (if manuals (string-join manuals ", ")
                  (format #f "~a of ~a" total (length all)))))
    (call-with-writable-database dbpath
      (lambda (db)
        (for-each
         (lambda (entry)
           (set! n (+ 1 n))
           (let* ((manual (car entry))
                  (text (render-entry entry))
                  (sig (and (not (string-null? (string-trim-both text)))
                            (text-signature text)))
                  (prev (assoc manual old))
                  (unchanged? (and sig prev (equal? (cadr prev) sig))))
             (cond
              ((not sig)
               (when tty? (format (current-error-port) "~%"))
               (format (current-error-port)
                       "info produced nothing for ~a (label) / ~a (target)~%"
                       manual (cdr entry))
               ;; Keep the old entry, so a manual info cannot render now does
               ;; not purge documents that were fine before.
               (when prev (set! entries (cons prev entries))))
              (unchanged?
               (vector-set! counters 1 (+ 1 (vector-ref counters 1)))
               (set! entries (cons prev entries)))
              (else
               (vector-set! counters 0 (+ 1 (vector-ref counters 0)))
               (let ((found (catch #t
                              (lambda () (index-manual db stem entry text))
                              (lambda (key . args)
                                (format (current-error-port)
                                        "skip ~a: ~a ~s~%" manual key args)
                                #f))))
                 ;; One document per node means replacing a manual does not
                 ;; replace all its documents: a node that has gone leaves one
                 ;; behind, so delete those explicitly.
                 (when (and found prev)
                   (for-each (lambda (name)
                               (unless (member name found)
                                 (purge-node! db manual name)))
                             (list-ref prev 2)))
                 (cond (found (set! entries (cons (list manual sig found) entries)))
                       (prev (set! entries (cons prev entries)))))))
             ;; Commit and record progress after each manual, so the pending
             ;; changes do not grow without bound and a crash costs one manual
             ;; rather than the whole run.  A partial manifest is safe: manuals
             ;; missing from it are simply re-indexed next time.
             (when commit-database! (commit-database! db))
             (write-manifest dbpath entries)
             (when tty?
               (format (current-error-port)
                       "\rIndexing: ~a/~a (~a new, ~a unchanged)"
                       n total (vector-ref counters 0) (vector-ref counters 1))
               (force-output (current-error-port)))))
         selected)
        ;; Purge every node of every manual no longer in the set -- including
        ;; manuals excluded by --manual= or --lang=, which is what makes a
        ;; restricted run a complete description of the database.
        (let ((stale (filter (lambda (e) (not (assoc (car e) entries))) old)))
          (unless (null? stale)
            (when tty? (format (current-error-port) "~%"))
            (format (current-error-port)
                    "purging ~a stale manual(s)~%" (length stale))
            (force-output (current-error-port))
            (for-each (lambda (e)
                        (for-each (lambda (name) (purge-node! db (car e) name))
                                  (list-ref e 2)))
                      stale)))))
    (when tty? (format (current-error-port) "~%"))
    (write-manifest dbpath entries)
    (call-with-output-file (string-append dbpath ".stem")
      (lambda (port) (display (stem-name lang) port) (newline port)))))

(define (rebuild dbpath lang manuals)
  (rm-rf dbpath)
  (rm-rf (string-append dbpath ".manifest"))
  (rm-rf (string-append dbpath ".stem"))
  (index-all dbpath lang manuals))

;;; ---- command line ------------------------------------------------------

(define (usage program)
  (format (current-error-port)
          "Usage: ~a [--rebuild] [--lang=LANG] [--manual=NAME[,NAME...]] DBPATH~%\
       ~a --render MANUAL~%"
          program program))

;; Flags may appear anywhere; the one positional argument is the database.
(define (parse-args args)
  (let loop ((as args) (rebuild? #f) (render? #f)
             (lang #f) (manuals '()) (pos '()))
    (match as
      (() (values rebuild? render? lang
                  (if (null? manuals) #f (reverse manuals))
                  (reverse pos)))
      ((a . rest)
       (cond
        ((string=? a "--rebuild") (loop rest #t render? lang manuals pos))
        ((string=? a "--render") (loop rest rebuild? #t lang manuals pos))
        ((string-match "^--lang=(.*)$" a)
         => (lambda (m) (loop rest rebuild? render? (match:substring m 1)
                              manuals pos)))
        ((string-match "^--manual=(.*)$" a)
         => (lambda (m)
              (let ((ms (filter (lambda (s) (not (string-null? s)))
                                (split-on-char (match:substring m 1) #\,))))
                (loop rest rebuild? render? lang
                      (append (reverse ms) manuals) pos))))
        (else (loop rest rebuild? render? lang manuals (cons a pos))))))))

(match (command-line)
  ((_ . args)
   (call-with-values (lambda () (parse-args args))
     (lambda (rebuild? render? lang manuals positional)
       (cond
        (render?
         (if (pair? positional)
             (let ((text (render-manual (car positional))))
               (if (string-null? (string-trim-both text))
                   (begin
                     (format (current-error-port)
                             "info produced nothing for ~a~%" (car positional))
                     (exit 1))
                   (display text)))
             (usage (car (command-line)))))
        ((null? positional) (usage (car (command-line))))
        (else
         (let ((dbpath (car positional)))
           (if rebuild?
               (rebuild dbpath lang manuals)
               (index-all dbpath lang manuals))))))))
  ((program . _) (usage program)))
