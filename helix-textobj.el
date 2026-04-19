;;; helix-textobj.el --- Text objects for Helix Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  jixiuf

;; Author: jixiuf
;; Keywords: convenience
;; URL: https://github.com/mgmarlow/helix-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


(require 'cl-lib)
(require 'thingatpt)

;; Declare helix-textobj-map which is defined in helix-core.el
(defvar helix-textobj-map nil "Keymap for textobj mode.")
(defvar helix--current-selection nil
  "Beginning point of current visual selection.")

;; ============================================================================

;; Internal Variables and Configuration
;; ============================================================================

(defvar helix-restriction-stack nil
  "List of previous restrictions for helix-with-restriction macro.")

(defcustom helix-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in CJK text handling. See the documentation of
`word-separating-categories'."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'helix)

(defcustom helix-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A))
  "List of pair (cons) of categories to determine word boundary
used in CJK text handling. See the documentation of
`word-combining-categories'."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'helix)

;; ============================================================================
;; Macro and Helper Functions (copied from evil-common.el)
;; ============================================================================

(defmacro helix-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with VAR bound to 1 or -1,
depending on the sign of COUNT. Set RESULT, if specified, to the
number of unsuccessful iterations, which is 0 if the loop completes
successfully. This is also the return value.

Each iteration must move point; if point does not change, the loop
immediately quits.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug ((symbolp form &optional symbolp) body)))
  (let* ((var (or (pop spec) (make-symbol "unitvar")))
         (count (or (pop spec) 0))
         (result (or (pop spec) var))
         (i (make-symbol "loopvar")))
    `(let* ((,i ,count)
            (,var (if (< ,i 0) -1 1)))
       (while (and (/= ,i 0)
                   (/= (point) (progn ,@body (point))))
         (setq ,i (if (< ,i 0) (1+ ,i) (1- ,i))))
       (setq ,result ,i))))

(defmacro helix-with-restriction (beg end &rest body)
  "Execute BODY with the buffer narrowed to BEG and END.
BEG or END may be nil to specify a one-sided restriction."
  (declare (indent 2) (debug t))
  `(save-restriction
     (let ((helix-restriction-stack
            (cons (cons (point-min) (point-max)) helix-restriction-stack)))
       (narrow-to-region (or ,beg (point-min)) (or ,end (point-max)))
       ,@body)))

(defun helix-forward-chars (chars &optional count)
  "Move point to the end or beginning of a sequence of CHARS.
CHARS is a character set as inside [...] in a regular expression."
  (let ((notchars (if (= (aref chars 0) ?^)
                      (substring chars 1)
                    (concat "^" chars))))
    (helix-motion-loop (dir (or count 1))
      (cond
       ((< dir 0)
        (skip-chars-backward notchars)
        (skip-chars-backward chars))
       (t
        (skip-chars-forward notchars)
        (skip-chars-forward chars))))))

(defun helix-forward-nearest (count &rest forwards)
  "Move point forward to the first of several motions.
FORWARDS is a list of forward motion functions (i.e. each moves
point forward to the next end of a text object (if passed a +1)
or backward to the preceeding beginning of a text object (if
passed a -1)). This function calls each of these functions once
and moves point to the nearest of the resulting positions. If
COUNT is positive point is moved forward COUNT times, if negative
point is moved backward -COUNT times."
  (helix-motion-loop (dir (or count 1))
    (let ((pnt (point))
          (nxt (if (< dir 0) (point-min) (point-max))))
      (dolist (fwd forwards)
        (goto-char pnt)
        (ignore-errors
          (helix-with-restriction
              (when (< dir 0)
                (save-excursion
                  (goto-char nxt)
                  (line-beginning-position 0)))
              (when (> dir 0)
                (save-excursion
                  (goto-char nxt)
                  (line-end-position 2)))
            (and (zerop (funcall fwd dir))
                 (/= (point) pnt)
                 (if (< dir 0) (> (point) nxt) (< (point) nxt))
                 (setq nxt (point))))))
      (goto-char nxt))))

(defun helix--forward-empty-line (&optional count)
  "Move forward COUNT empty lines."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (while (and (> count 0) (not (eobp)))
      (when (and (bolp) (eolp))
        (setq count (1- count)))
      (forward-line 1)))
   (t
    (while (and (< count 0) (not (bobp))
                (zerop (forward-line -1)))
      (when (and (bolp) (eolp))
        (setq count (1+ count))))))
  count)

(defun helix--forward-word (&optional count)
  "Move forward COUNT words.
Moves point COUNT words forward or (- COUNT) words backward if
COUNT is negative.  Point is placed after the end of the word (if
forward) or at the first character of the word (if backward).  A
word is a sequence of word characters matching
\[[:word:]] (recognized by `forward-word'), a sequence of
non-whitespace non-word characters '[^[:word:]\\n\\r\\t\\f ]', or
an empty line matching ^$."
  (helix-forward-nearest
   count
   #'(lambda (&optional cnt)
       (let ((word-separating-categories helix-cjk-word-separating-categories)
             (word-combining-categories helix-cjk-word-combining-categories)
             (pnt (point)))
         (forward-word cnt)
         (if (= pnt (point)) cnt 0)))
   #'(lambda (&optional cnt)
       (helix-forward-chars "^[:word:]\n\r\t\f " cnt))
    #'helix--forward-empty-line))
(put 'helix-word 'forward-op #'helix--forward-word)

(defun helix--forward-WORD (&optional count)
  "Move forward COUNT \"WORDS\".
Moves point COUNT WORDS forward or (- COUNT) WORDS backward if
COUNT is negative. Point is placed after the end of the WORD (if
forward) or at the first character of the WORD (if backward). A
WORD is a sequence of non-whitespace characters
'[^\\n\\r\\t\\f ]', or an empty line matching ^$."
  (helix-forward-nearest count
                        #'(lambda (&optional cnt)
                            (helix-forward-chars "^\n\r\t\f " cnt))
                         #'helix--forward-empty-line))
(put 'helix-WORD 'forward-op #'helix--forward-WORD)

(defun helix-forward-not-thing (thing &optional count)
  "Move point to the end or beginning of the complement of THING."
  (helix-motion-loop (dir (or count 1))
    (let (bnd)
      (cond
       ((> dir 0)
        (while (and (setq bnd (bounds-of-thing-at-point thing))
                    (< (point) (cdr bnd)))
          (goto-char (cdr bnd)))
        ;; no thing at (point)
        (if (zerop (forward-thing thing))
            ;; now at the end of the next thing
            (let ((bnd (bounds-of-thing-at-point thing)))
              (if (or (< (car bnd) (point))    ; end of a thing
                      (= (car bnd) (cdr bnd))) ; zero width thing
                  (goto-char (car bnd))
                ;; beginning of yet another thing, go back
                (forward-thing thing -1)))
          (goto-char (point-max))))
       (t
        (while (and (not (bobp))
                    (setq bnd (progn (backward-char) (bounds-of-thing-at-point thing)))
                    (< (point) (cdr bnd)))
          (goto-char (car bnd)))
        ;; either bob or no thing at point
        (goto-char
         (if (and (not (bobp))
                  (zerop (forward-thing thing -1))
                  (setq bnd (bounds-of-thing-at-point thing)))
             (cdr bnd)
           (point-min))))))))

(defun helix-bounds-of-not-thing-at-point (thing &optional which)
  "Return the bounds of a complement of THING at point.
If there is a THING at point nil is returned.  Otherwise if WHICH
is nil or 0 a cons cell (BEG . END) is returned. If WHICH is
negative the beginning is returned. If WHICH is positive the END
is returned."
  (let ((pnt (point)))
    (let ((beg (save-excursion
                 (and (zerop (forward-thing thing -1))
                      (forward-thing thing))
                 (if (> (point) pnt) (point-min) (point))))
          (end (save-excursion
                 (and (zerop (forward-thing thing))
                      (forward-thing thing -1))
                 (if (< (point) pnt) (point-max) (point)))))
      (when (and (<= beg (point) end) (< beg end))
        (cond
         ((or (not which) (zerop which)) (cons beg end))
         ((< which 0) beg)
         ((> which 0) end))))))

(defun helix-select-inner-object (thing beg end &optional count)
  "Return an inner text object range of COUNT objects.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by `thing-at-point'.  BEG, END specify the current
selection."
  (let* ((count (or count 1))
         (bnd (or (let ((b (bounds-of-thing-at-point thing)))
                    (and b (< (point) (cdr b)) b))
                  (helix-bounds-of-not-thing-at-point thing)
                  (cons (point-min) (point-max)))))
    ;; check if current object is selected
    (when (or (not beg) (not end)
              (> beg (car bnd))
              (< end (cdr bnd)))
      (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
      (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
      (setq count (if (> count 0) (1- count) (1+ count))))
    (goto-char (if (< count 0) beg end))
    (helix-forward-nearest count
                          #'(lambda (cnt) (forward-thing thing cnt))
                          #'(lambda (cnt) (helix-forward-not-thing thing cnt)))
    (cons (if (>= count 0) beg (point))
          (if (< count 0) end (point)))))

(defun helix-select-a-object (thing beg end &optional count)
  "Return an outer text object range of COUNT objects.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by thing-at-point.  BEG, END specify the current
selection."
  (let* ((dir (if (> (or count 1) 0) +1 -1))
         (count (abs (or count 1)))
         (objbnd (let ((b (bounds-of-thing-at-point thing)))
                   (and b (< (point) (cdr b)) b)))
         (bnd (or objbnd
                  (helix-bounds-of-not-thing-at-point thing)
                  (cons (point-min) (point-max))))
         addcurrent other)
    ;; check if current object is not selected
    (when (or (not beg) (not end)
              (> beg (car bnd))
              (< end (cdr bnd)))
      ;; if not, enlarge selection
      (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
      (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
      (if objbnd (setq addcurrent t)))
    ;; make other and (point) reflect the selection
    (cond
     ((> dir 0) (goto-char end) (setq other beg))
     (t (goto-char beg) (setq other end)))
    (cond
     ;; do nothing more than only current is selected
     ((not (and (= beg (car bnd)) (= end (cdr bnd)))))
     ;; current match is thing, add whitespace
     (objbnd
      (let ((wsend (helix-with-restriction
                       ;; restrict to current line if we do non-line selection
                       (line-beginning-position)
                       (line-end-position)
                     (helix-bounds-of-not-thing-at-point thing dir))))
        (cond
         (wsend
          ;; add whitespace at end
          (goto-char wsend)
          (setq addcurrent t))
         (t
          ;; no whitespace at end, try beginning
          (save-excursion
            (goto-char other)
            (setq wsend
                  (helix-with-restriction
                      ;; restrict to current line if we do non-line selection
                      (if (member thing '(helix-word helix-WORD))
                          (save-excursion (back-to-indentation) (point))
                        (line-beginning-position))
                      (line-end-position)
                    (helix-bounds-of-not-thing-at-point thing (- dir))))
            (when wsend (setq other wsend addcurrent t)))))))
     ;; current match is whitespace, add thing
     (t
      (forward-thing thing dir)
      (setq addcurrent t)))
    ;; possibly count current object as selection
    (if addcurrent (setq count (1- count)))
    ;; move
    (dotimes (_ count)
      (let ((wsend (helix-bounds-of-not-thing-at-point thing dir)))
        (if (and wsend (/= wsend (point)))
            ;; start with whitespace
            (forward-thing thing dir)
          ;; start with thing
          (forward-thing thing dir)
          (setq wsend (helix-bounds-of-not-thing-at-point thing dir))
          (when wsend (goto-char wsend)))))
    ;; return range
    (cons (if (> dir 0) other (point))
          (if (< dir 0) other (point)))))

(defun helix-select-inner-restricted-object (thing beg end &optional count)
  "Return an inner text object range of COUNT objects.
Selection is restricted to the current line, unless it is empty.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by `thing-at-point'.  BEG, END specify the current
selection."
  (save-restriction
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (unless (= start end)
        (narrow-to-region start end)))
    (helix-select-inner-object thing beg end count)))

(defun helix-select-a-restricted-object (thing beg end &optional count)
  "Return an outer text object range of COUNT objects.
Selection is restricted to the current line, unless it is empty.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by `thing-at-point'.  BEG, END specify the current
selection."
  (save-restriction
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (when (/= start end)
        (narrow-to-region start end)))
    (helix-select-a-object thing beg end count)))

;; ============================================================================
;; Text Object Interactive Commands
;; ============================================================================

;; ============================================================================
;; Text Object Interactive Commands (now defined via helix-define-mark-object)
;; ============================================================================

(defun helix--forward-symbol (&optional count)
  "Move forward COUNT symbols.
Moves point COUNT symbols forward or (- COUNT) symbols backward
if COUNT is negative. Point is placed after the end of the
symbol (if forward) or at the first character of the symbol (if
backward). A symbol is either determined by `forward-symbol', or
is a sequence of characters not in the word, symbol or whitespace
syntax classes."
  (helix-forward-nearest
   count
   #'(lambda (&optional cnt)
       (helix-forward-syntax "^w_->" cnt))
   #'(lambda (&optional cnt)
       (let ((pnt (point)))
         (forward-symbol cnt)
         (if (= pnt (point)) cnt 0)))
    #'helix--forward-empty-line))
(put 'helix-symbol 'forward-op #'helix--forward-symbol)

(defun helix-forward-syntax (syntax &optional count)
  "Move point to the end or beginning of a sequence of characters in SYNTAX.
Stop on reaching a character not in SYNTAX."
  (let ((notsyntax (if (= (aref syntax 0) ?^)
                       (substring syntax 1)
                     (concat "^" syntax))))
    (helix-motion-loop (dir (or count 1))
      (cond
       ((< dir 0)
        (skip-syntax-backward notsyntax)
        (skip-syntax-backward syntax))
       (t
        (skip-syntax-forward notsyntax)
        (skip-syntax-forward syntax))))))

;; ============================================================================
;; Sentence Text Objects
;; ============================================================================

(defun helix--forward-sentence (&optional count)
  "Move forward COUNT sentences.
Moves point COUNT sentences forward or (- COUNT) sentences
backward if COUNT is negative.  This function is the same as
`forward-sentence' but returns the number of sentences that could
NOT be moved over."
  (helix-motion-loop (dir (or count 1))
    (ignore-errors (forward-sentence dir))))
(put 'helix-sentence 'forward-op #'helix--forward-sentence)

;; ============================================================================
;; Paragraph Text Objects
;; ============================================================================

(defun helix--forward-paragraph (&optional count)
  "Move forward COUNT paragraphs.
Moves point COUNT paragraphs forward or (- COUNT) paragraphs backward
if COUNT is negative.  A paragraph is defined by
`start-of-paragraph-text' and `forward-paragraph' functions."
  (helix-motion-loop (dir (or count 1))
    (cond
     ((> dir 0) (forward-paragraph))
      ((not (bobp)) (start-of-paragraph-text) (beginning-of-line)))))
(put 'helix-paragraph 'forward-op #'helix--forward-paragraph)

;; ============================================================================
;; Parenthesis/Bracket Text Objects
;; ============================================================================
(defvar helix-type-properties nil
  "Specifications made by `helix-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(defun helix-type-p (sym)
  "Whether SYM is the name of a type."
  (assq sym helix-type-properties))

(defun helix-normalize-position (pos)
  "Return POS if it does not exceed the buffer boundaries.
If POS is less than `point-min', return `point-min'.
Is POS is more than `point-max', return `point-max'.
If POS is a marker, return its position."
  (cond
   ((not (number-or-marker-p pos))
    pos)
   ((< pos (point-min))
    (point-min))
   ((> pos (point-max))
    (point-max))
   ((markerp pos)
    (marker-position pos))
   (t
    pos)))

(defmacro helix-sort (&rest vars)
  "Sort the symbol values of VARS.
Place the smallest value in the first argument and the largest in the
last, sorting in between."
  (if (= (length vars) 2)
      `(when (> ,@vars) (cl-rotatef ,@vars))
    (let ((sorted (make-symbol "sortvar")))
      `(let ((,sorted (sort (list ,@vars) #'<)))
         (setq ,@(apply #'nconc
                        (mapcar (lambda (var) (list var `(pop ,sorted)))
                                vars)))))))

(defun helix-range (beg end &optional type &rest properties)
  "Return a list (BEG END [TYPE] PROPERTIES...).
BEG and END are buffer positions (numbers or markers),
TYPE is a type as per `helix-type-p', and PROPERTIES is
a property list."
  (let ((beg (helix-normalize-position beg))
        (end (helix-normalize-position end)))
    (when (and (numberp beg) (numberp end))
      (helix-sort beg end)
      (nconc (list beg end)
             (when (helix-type-p type) (list type))
             properties))))

(defun helix-up-block (beg end &optional count)
  "Move point to the end or beginning of text enclosed by BEG and END.
BEG and END should be regular expressions matching the opening
and closing delimiters, respectively. If COUNT is greater than
zero point is moved forward otherwise it is moved
backwards. Whenever an opening delimiter is found the COUNT is
increased by one, if a closing delimiter is found the COUNT is
decreased by one. The motion stops when COUNT reaches zero. The
match-data reflects the last successful match (that caused COUNT
to reach zero). The behaviour of this functions is similar to
`up-list'."
  (let* ((count (or count 1))
         (forwardp (> count 0))
         (dir (if forwardp +1 -1)))
    (catch 'done
      (while (not (zerop count))
        (let* ((pnt (point))
               (cl (save-excursion
                     (and (re-search-forward (if forwardp end beg) nil t dir)
                          (or (/= pnt (point))
                              (progn
                                ;; zero size match, repeat search from
                                ;; the next position
                                (forward-char dir)
                                (re-search-forward (if forwardp end beg) nil t dir)))
                          (point))))
               (match (match-data t))
               (op (save-excursion
                     (and (not (equal beg end))
                          (re-search-forward (if forwardp beg end) cl t dir)
                          (or (/= pnt (point))
                              (progn
                                ;; zero size match, repeat search from
                                ;; the next position
                                (forward-char dir)
                                (re-search-forward (if forwardp beg end) cl t dir)))
                          (point)))))
          (cond
           ((not cl)
            (goto-char (if forwardp (point-max) (point-min)))
            (set-match-data nil)
            (throw 'done count))
           (t
            (if op
                (progn
                  (setq count (if forwardp (1+ count) (1- count)))
                  (goto-char op))
              (setq count (if forwardp (1- count) (1+ count)))
              (if (zerop count) (set-match-data match))
              (goto-char cl))))))
      0)))

(defun helix--get-block-range (op cl selection-type)
  "Return the exclusive range of a visual selection.
OP and CL are pairs of buffer positions for the opening and
closing delimiter of a range. SELECTION-TYPE is the desired type
of selection.  It is a symbol that determines which parts of the
block are selected.  If it is `inclusive' or t the returned range
is \(cons (car OP) (cdr CL)). If it is `exclusive' or nil the
returned range is (cons (cdr OP) (car CL)).  If it is
`exclusive-line' the returned range will skip whitespace at the
end of the line of OP and at the beginning of the line of CL."
  (cond
   ((memq selection-type '(inclusive t)) (cons (car op) (cdr cl)))
   ((memq selection-type '(exclusive nil)) (cons (cdr op) (car cl)))
   ((eq selection-type 'exclusive-line)
    (let ((beg (cdr op))
          (end (car cl)))
      (save-excursion
        (goto-char beg)
        (when (and (eolp) (not (eobp)))
          (setq beg (line-beginning-position 2)))
        (goto-char end)
        (skip-chars-backward " \t")
        (when (bolp)
          (setq end (point))
          (goto-char beg)
          (when (and (not (bolp)) (< beg end))
            (setq end (1- end)))))
      (cons beg end)))
   (t (user-error "Unknown selection-type `%s'" selection-type))))

(defun helix-select-block (thing beg end type count
                                &optional
                                selection-type
                                countcurrent
                                fixedscan)
  "Return a range (BEG END) of COUNT delimited text objects.
BEG END TYPE are the currently selected (visual) range.  The
delimited object must be given by THING-up function (see
`helix-up-block').

SELECTION-TYPE is symbol that determines which parts of the block
are selected.  If it is `inclusive' or t OPEN and CLOSE are
included in the range. If it is `exclusive' or nil the delimiters
are not contained. If it is `exclusive-line' the delimiters are
not included as well as adjacent whitespace until the beginning
of the next line or the end of the previous line. If the
resulting selection consists of complete lines only and visual
state is not active, the returned selection is linewise.

If COUNTCURRENT is non-nil an objected is counted if the current
selection matches that object exactly.

Usually scanning for the surrounding block starts at (1+ beg)
and (1- end). If this might fail due to the behavior of THING
then FIXEDSCAN can be set to t. In this case the scan starts at
BEG and END. One example where this might fail is if BEG and END
are the delimiters of a string or comment."
  (save-excursion
    (save-match-data
      (let* ((orig-beg beg)
             (orig-end end)
             (beg (or beg (point)))
             (end (or end (point)))
             (count (abs (or count 1)))
             op cl op-end cl-end)
        ;; We always assume at least one selected character.
        (if (= beg end) (setq end (1+ end)))
        ;; We scan twice: starting at (1+ beg) forward and at (1- end)
        ;; backward. The resulting selection is the smaller one.
        (goto-char (if fixedscan beg (1+ beg)))
        (when (and (zerop (funcall thing +1)) (match-beginning 0))
          (setq cl (cons (match-beginning 0) (match-end 0)))
          (goto-char (car cl))
          (when (and (zerop (funcall thing -1)) (match-beginning 0))
            (setq op (cons (match-beginning 0) (match-end 0)))))
        ;; start scanning from end
        (goto-char (if fixedscan end (1- end)))
        (when (and (zerop (funcall thing -1)) (match-beginning 0))
          (setq op-end (cons (match-beginning 0) (match-end 0)))
          (goto-char (cdr op-end))
          (when (and (zerop (funcall thing +1)) (match-beginning 0))
            (setq cl-end (cons (match-beginning 0) (match-end 0)))))
        ;; Bug #607: use the tightest selection that contains the
        ;; original selection. If non selection contains the original,
        ;; use the larger one.
        (cond
         ((and (not op) (not cl-end))
          (error "No surrounding delimiters found"))
         ((or (not op) ; first not found
              (and cl-end ; second found
                   (>= (car op-end) (car op)) ; second smaller
                   (<= (cdr cl-end) (cdr cl))
                   (<= (car op-end) beg)      ; second contains orig
                   (>= (cdr cl-end) end)))
          (setq op op-end cl cl-end)))
        (setq op-end op cl-end cl) ; store copy
        ;; if the current selection contains the surrounding
        ;; delimiters, they do not count as new selection
        (let ((cnt (if (and orig-beg orig-end (not countcurrent))
                       (let ((sel (helix--get-block-range op cl selection-type)))
                         (if (and (<= orig-beg (car sel))
                                  (>= orig-end (cdr sel)))
                             count
                           (1- count)))
                     (1- count))))
          ;; starting from the innermost surrounding delimiters
          ;; increase selection
          (when (> cnt 0)
            (setq op (progn
                       (goto-char (car op-end))
                       (funcall thing (- cnt))
                       (if (match-beginning 0)
                           (cons (match-beginning 0) (match-end 0))
                         op))
                  cl (progn
                       (goto-char (cdr cl-end))
                       (funcall thing cnt)
                       (if (match-beginning 0)
                           (cons (match-beginning 0) (match-end 0))
                         cl)))))
        (let ((sel (helix--get-block-range op cl selection-type)))
          (setq op (car sel)
                cl (cdr sel)))
        (cond
         ((and (equal op orig-beg) (equal cl orig-end)
               (or (not countcurrent) (/= count 1)))
          (error "No surrounding delimiters found"))
         ((save-excursion
            (and (not helix--current-selection)
                 (eq type 'inclusive)
                 (progn (goto-char op) (bolp))
                 (progn (goto-char cl) (bolp))))
          (helix-range op cl 'line :expanded t))
         (t (helix-range op cl type :expanded t)))))))

(defun helix-up-paren (open close &optional count)
  "Move point to the end or beginning of balanced parentheses.
OPEN and CLOSE should be characters identifying the opening and
closing parenthesis, respectively. If COUNT is greater than zero
point is moved forward otherwise it is moved backwards. Whenever
an opening delimiter is found the COUNT is increased by one, if a
closing delimiter is found the COUNT is decreased by one. The
motion stops when COUNT reaches zero. The match-data reflects the
last successful match (that caused COUNT to reach zero)."
  ;; Always use the default `forward-sexp-function'. This is important
  ;; for modes that use a custom one like `python-mode'.
  ;; (addresses #364)
  (let (forward-sexp-function up-list-function)
    (with-syntax-table (copy-syntax-table (syntax-table))
      (modify-syntax-entry open (format "(%c" close))
      (modify-syntax-entry close (format ")%c" open))
      (let ((rest (helix-motion-loop (dir count)
                    (let ((pnt (point)))
                      (condition-case nil
                          (cond
                           ((> dir 0)
                            (while (progn
                                     (up-list dir t)
                                     (/= (char-before) close))))
                           (t
                            (while (progn
                                     (up-list dir t)
                                     (/= (char-after) open)))))
                        (error (goto-char pnt)))))))
        (cond
         ((= rest count) (set-match-data nil))
         ((> count 0) (set-match-data (list (1- (point)) (point))))
         (t (set-match-data (list (point) (1+ (point))))))
        rest))))

(defun helix-select-paren (open close beg end type count &optional inclusive)
  "Return a range (BEG END) of COUNT delimited text objects.
OPEN and CLOSE specify the opening and closing delimiter,
respectively. BEG END TYPE are the currently selected (visual)
range.  If INCLUSIVE is non-nil, OPEN and CLOSE are included in
the range; otherwise they are excluded.

If you aren't inside a pair of the opening and closing delimiters,
it jumps you inside the next one. If there isn't one, it errors.

The types of OPEN and CLOSE specify which kind of THING is used
for parsing with `helix-select-block'. If OPEN and CLOSE are
characters `helix-up-paren' is used. Otherwise OPEN and CLOSE
must be regular expressions and `helix-up-block' is used.

If the selection is exclusive, whitespace at the end or at the
beginning of the selection until the end-of-line or beginning-of-line
is ignored."
  (condition-case nil
      (progn
        ;; we need special linewise exclusive selection
        (unless inclusive (setq inclusive 'exclusive-line))
        (cond
         ((and (characterp open) (characterp close))
          (let ((thing #'(lambda (&optional cnt)
                           (helix-up-paren open close cnt)))
                (bnd (or (bounds-of-thing-at-point 'helix-string)
                         (bounds-of-thing-at-point 'helix-comment)
                         ;; If point is at the opening quote of a string,
                         ;; this must be handled as if point is within the
                         ;; string, i.e. the selection must be extended
                         ;; around the string. Otherwise
                         ;; `helix-select-block' might do the wrong thing
                         ;; because it accidentally moves point inside the
                         ;; string (for inclusive selection) when looking
                         ;; for the current surrounding block. (re #364)
                         (and (= (point) (or beg (point)))
                              (save-excursion
                                (goto-char (1+ (or beg (point))))
                                (or (bounds-of-thing-at-point 'helix-string)
                                    (bounds-of-thing-at-point 'helix-comment)))))))
            (if (not bnd)
                (helix-select-block thing beg end type count inclusive)
              (or (helix-with-restriction (car bnd) (cdr bnd)
                    (ignore-errors
                      (helix-select-block thing beg end type count inclusive)))
                  (save-excursion
                    (setq beg (or beg (point))
                          end (or end (point)))
                    (goto-char (car bnd))
                    (let ((extbeg (min beg (car bnd)))
                          (extend (max end (cdr bnd))))
                      (helix-select-block thing
                                         extbeg extend
                                         type
                                         count
                                         inclusive
                                         (or (< extbeg beg) (> extend end))
                                         t)))))))
         (t
          (helix-select-block #'(lambda (&optional cnt)
                                 (helix-up-block open close cnt))
                             beg end type count inclusive))))
    (error ; we aren't in the parens, so find next instance
     (save-match-data
       (goto-char (or (if (and count (> 0 count)) end beg)
                      (point)))
       (let ((re (if (characterp open) (regexp-quote (string open)) open)))
         (if (and (not (looking-at-p re))
                  (re-search-forward re nil t count))
             (progn
               (goto-char (match-beginning 0))
               (let* ((mbeg (match-beginning 0))
                      (res (helix-select-paren open close mbeg mbeg
                                              type nil inclusive)))
                 (if (< (car res) mbeg)
                     ;; this will error if the beginning of the found parens is before the target paren
                     ;; this prevents things such as on the line `prova ( verder "((testo)")`,
                     ;; the inputs `g2ci(` from putting your cursor inside the deleted `()` after `prova`
                     ;; without this, it would go to the second paren (the unbalanced first paren inside the quotes)
                     ;; and then do a change there, changing inside the whole paren after `prova`
                     (error "No surrounding delimiters found")
                   res)))
           (error "No surrounding delimiters found")))))))

(defun helix--bounds-of-string-at-point (&optional state)
  "Return the bounds of a string at point.
If STATE is given it used a parsing state at point."
  (save-excursion
    (let ((state (or state (syntax-ppss))))
      (when (nth 3 state)
        (cons (nth 8 state)
              (when (parse-partial-sexp
                     (point) (point-max) nil nil state 'syntax-table)
                (point)))))))
(put 'helix-string 'bounds-of-thing-at-point #'helix--bounds-of-string-at-point)

(defun helix--bounds-of-comment-at-point ()
  "Return the bounds of a string at point."
  (save-excursion
    (let ((state (syntax-ppss)))
      (when (nth 4 state)
        (cons (nth 8 state)
              (when (parse-partial-sexp
                     (point) (point-max) nil nil state 'syntax-table)
                (point)))))))
(put 'helix-comment 'bounds-of-thing-at-point #'helix--bounds-of-comment-at-point)


(defun helix-forward-quote (quote &optional count)
  "Move point to the end or beginning of a string.
QUOTE is the character delimiting the string. If COUNT is greater
than zero point is moved forward otherwise it is moved
backwards."
  (let (reset-parser)
    (with-syntax-table (copy-syntax-table (syntax-table))
      (unless (= (char-syntax quote) ?\")
        (modify-syntax-entry quote "\"")
        (setq reset-parser t))
      ;; global parser state is out of state, use local one
      (let* ((pnt (point))
             (state (save-excursion
                      (beginning-of-defun)
                      (parse-partial-sexp (point) pnt nil nil (syntax-ppss))))
             (bnd (helix--bounds-of-string-at-point state)))
        (when (and bnd (< (point) (cdr bnd)))
          ;; currently within a string
          (if (> count 0)
              (progn
                (goto-char (cdr bnd))
                (setq count (1- count)))
            (goto-char (car bnd))
            (setq count (1+ count))))
        ;; forward motions work with local parser state
        (cond
         ((> count 0)
          ;; no need to reset global parser state because we only use
          ;; the local one
          (setq reset-parser nil)
          (catch 'done
            (while (and (> count 0) (not (eobp)))
              (setq state (parse-partial-sexp
                           (point) (point-max) nil nil state 'syntax-table))
              (cond
               ((nth 3 state)
                (setq bnd (bounds-of-thing-at-point 'helix-string))
                (goto-char (cdr bnd))
                (setq count (1- count)))
               ((eobp) (goto-char pnt) (throw 'done nil))))))
         ((< count 0)
          ;; need to update global cache because of backward motion
          (setq reset-parser (and reset-parser (point)))
          (save-excursion
            (beginning-of-defun)
            (syntax-ppss-flush-cache (point)))
          (catch 'done
            (while (and (< count 0) (not (bobp)))
              (setq pnt (point))
              (while (and (not (bobp))
                          (or (eobp) (/= (char-after) quote)))
                (backward-char))
              (cond
               ((setq bnd (bounds-of-thing-at-point 'helix-string))
                (goto-char (car bnd))
                (setq count (1+ count)))
               ((bobp) (goto-char pnt) (throw 'done nil))
               (t (backward-char))))))
         (t (setq reset-parser nil)))))
    (when reset-parser
      ;; reset global cache
      (save-excursion
        (goto-char reset-parser)
        (beginning-of-defun)
        (syntax-ppss-flush-cache (point))))
    count))

(defvar helix-forward-quote-char ?\"
  "The character to be used by `helix--forward-quote-default'.")

(defun helix--forward-quote (&optional count)
  "Move forward COUNT strings.
The quotation character is specified by the global variable
`helix-forward-quote-char'. This character is passed to
`helix-forward-quote'."
  (helix-forward-quote helix-forward-quote-char count))
(put 'helix-quote 'forward-op #'helix--forward-quote)

(defun helix-select-quote-thing (thing beg end _type count &optional inclusive)
  "Selection THING as if it described a quoted object.
THING is typically either `helix-quote' or `helix-chars'. This
function is called from `helix-select-quote'."
  (save-excursion
    (let* ((count (or count 1))
           (dir (if (> count 0) 1 -1))
           (bnd (let ((b (bounds-of-thing-at-point thing)))
                  (and b (< (point) (cdr b)) b)))
           addcurrent
           wsboth)
      (if inclusive (setq inclusive t)
        (when (= (abs count) 2)
          (setq count dir)
          (setq inclusive 'quote-only))
        ;; never extend with exclusive selection
        (setq beg nil end nil))
      ;; check if the previously selected range does not contain a
      ;; string
      (unless (and beg end
                   (save-excursion
                     (goto-char (if (> dir 0) beg end))
                     (forward-thing thing dir)
                     (and (<= beg (point)) (< (point) end))))
        ;; if so forget the range
        (setq beg nil end nil))
      ;; check if there is a current object, if not fetch one
      (when (not bnd)
        (unless (and (zerop (forward-thing thing dir))
                     (setq bnd (bounds-of-thing-at-point thing)))
          (error "No quoted string found"))
        (if (> dir 0)
            (setq end (point))
          (setq beg (point)))
        (setq addcurrent t))
      ;; check if current object is not selected
      (when (or (not beg) (not end) (> beg (car bnd)) (< end (cdr bnd)))
        ;; if not, enlarge selection
        (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
        (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
        (setq addcurrent t wsboth t))
      ;; maybe count current element
      (when addcurrent
        (setq count (if (> dir 0) (1- count) (1+ count))))
      ;; enlarge selection
      (goto-char (if (> dir 0) end beg))
      (when (and (not addcurrent)
                 (= count (forward-thing thing count)))
        (error "No quoted string found"))
      (if (> dir 0) (setq end (point)) (setq beg (point)))
      ;; add whitespace
      (cond
       ((not inclusive) (setq beg (1+ beg) end (1- end)))
       ((not (eq inclusive 'quote-only))
        ;; try to add whitespace in forward direction
        (goto-char (if (> dir 0) end beg))
        (if (setq bnd (bounds-of-thing-at-point 'helix-space))
            (if (> dir 0) (setq end (cdr bnd)) (setq beg (car bnd)))
          ;; if not found try backward direction
          (goto-char (if (> dir 0) beg end))
          (if (and wsboth (setq bnd (bounds-of-thing-at-point 'helix-space)))
              (if (> dir 0) (setq beg (car bnd)) (setq end (cdr bnd)))))))
      (helix-range beg end
                  ;; HACK: fixes #583
                  ;; When not in visual state, an empty range is
                  ;; possible. However, this cannot be achieved with
                  ;; inclusive ranges, hence we use exclusive ranges
                  ;; in this case. In visual state the range must be
                  ;; inclusive because otherwise the selection would
                  ;; be wrong.
                  (if helix--current-selection 'inclusive 'exclusive)
                  :expanded t))))

(defun helix-select-quote (quote beg end type count &optional inclusive)
  "Return a range (BEG END) of COUNT quoted text objects.
QUOTE specifies the quotation delimiter. BEG END TYPE are the
currently selected (visual) range.

If INCLUSIVE is nil the previous selection is ignore. If there is
quoted string at point this object will be selected, otherwise
the following (if (> COUNT 0)) or preceeding object (if (< COUNT
0)) is selected. If (/= (abs COUNT) 2) the delimiting quotes are not
contained in the range, otherwise they are contained in the range.

If INCLUSIVE is non-nil the selection depends on the previous
selection. If the currently selection contains at least one
character that is contained in a quoted string then the selection
is extended, otherwise it is thrown away. If there is a
non-selected object at point then this object is added to the
selection. Otherwise the selection is extended to the
following (if (> COUNT 0)) or preceeding object (if (< COUNT
0)). Any whitespace following (or preceeding if (< COUNT 0)) the
new selection is added to the selection. If no such whitespace
exists and the selection contains only one quoted string then the
preceeding (or following) whitespace is added to the range. "
  (let ((helix-forward-quote-char quote))
    (or (let ((bnd (or (bounds-of-thing-at-point 'helix-comment)
                       (bounds-of-thing-at-point 'helix-string))))
          (when (and bnd (< (point) (cdr bnd))
                     (/= (char-after (car bnd)) quote)
                     (/= (char-before (cdr bnd)) quote))
            (helix-with-restriction (car bnd) (cdr bnd)
              (ignore-errors (helix-select-quote-thing
                              'helix-quote-simple
                              beg end type
                              count
                              inclusive)))))
        (let ((helix-forward-quote-char quote))
          (helix-select-quote-thing 'helix-quote
                                   beg end type
                                   count
                                   inclusive)))))
(defun helix-range-p (object)
  "Whether OBJECT is a range."
  (and (listp object)
       (numberp (nth 0 object))
       (numberp (nth 1 object))))

(defun helix-range-end (range)
  "Return end of RANGE."
  (when (helix-range-p range)
    (let ((beg (helix-normalize-position (nth 0 range)))
          (end (helix-normalize-position (nth 1 range))))
      (max beg end))))

(defun helix-range-beginning (range)
  "Return beginning of RANGE."
  (when (helix-range-p range)
    (let ((beg (helix-normalize-position (nth 0 range)))
          (end (helix-normalize-position (nth 1 range))))
      (min beg end))))


(defun helix-select-xml-tag (beg end type &optional count inclusive)
  "Return a range (BEG END) of COUNT matching XML tags.
If INCLUSIVE is non-nil, the tags themselves are included
from the range."
  (cond
   ((and (not inclusive) (= (abs (or count 1)) 1))
    (let ((rng (helix-select-block #'helix-up-xml-tag beg end type count nil t)))
      (if (or (and beg (= beg (helix-range-beginning rng))
                   end (= end (helix-range-end rng))))
          (helix-select-block #'helix-up-xml-tag beg end type count t)
        rng)))
   (t
    (helix-select-block #'helix-up-xml-tag beg end type count inclusive))))

(defun helix-up-xml-tag (&optional count)
  "Move point to the end or beginning of balanced xml tags.
If COUNT is greater than zero point is moved forward otherwise it is moved
backwards.  Whenever an opening delimiter is found the COUNT is increased by
one, if a closing delimiter is found the COUNT is decreased by one.  The motion
stops when COUNT reaches zero.  The match data reflects the last successful
match (that caused COUNT to reach zero)."
  (let* ((dir (if (> (or count 1) 0) +1 -1))
         (count (abs (or count 1)))
         (op (if (> dir 0) 1 2))
         (cl (if (> dir 0) 2 1))
         (orig (point))
         pnt tags match)
    (catch 'done
      (while (> count 0)
        ;; find the previous opening tag
        (while
            (and (setq match
                       (re-search-forward
                        "<\\([^/ >\n]+\\)\\(?:=>?\\|[^\"/>]\\|\"[^\"]*\"\\)*?>\\|</\\([^>]+?\\)>"
                        nil t dir))
                 (cond
                  ((match-beginning op)
                   (push (match-string op) tags))
                  ((null tags) nil) ; free closing tag
                  ((and (< dir 0)
                        (string= (car tags) (match-string cl)))
                   ;; in backward direction we only accept matching
                   ;; tags. If the current tag is a free opener
                   ;; without matching closing tag, the subsequent
                   ;; test will make us ignore this tag
                   (pop tags))
                  ((and (> dir 0))
                   ;; non matching openers are considered free openers
                   (while (and tags
                               (not (string= (car tags)
                                             (match-string cl))))
                     (pop tags))
                   (pop tags)))))
        (unless (setq match (and match (match-data t)))
          (setq match nil)
          (throw 'done count))
        ;; found closing tag, look for corresponding opening tag
        (cond
         ((> dir 0)
          (setq pnt (match-end 0))
          (goto-char (match-beginning 0)))
         (t
          (setq pnt (match-beginning 0))
          (goto-char (match-end 0))))
        (let* ((tag (match-string cl))
               (refwd (concat "<\\(/\\)?"
                              (regexp-quote tag)
                              "\\(?:>\\|[ \n]\\(?:[^\"/>]\\|\"[^\"]*\"\\)*?>\\)"))
               (cnt 1))
          (while (and (> cnt 0) (re-search-backward refwd nil t dir))
            (setq cnt (+ cnt (if (match-beginning 1) dir (- dir)))))
          (if (zerop cnt) (setq count (1- count) tags nil))
          (goto-char pnt)))
      (if (> count 0)
          (set-match-data nil)
        (set-match-data match)
        (goto-char (if (> dir 0) (match-end 0) (match-beginning 0)))))
    ;; if not found, set to point-max/point-min
    (unless (zerop count)
      (set-match-data nil)
      (goto-char (if (> dir 0) (point-max) (point-min)))
      (if (/= (point) orig) (setq count (1- count))))
    (* dir count)))

(defmacro helix-define-mark-pair (name open close doc inner-p)
  "Define mark inner/a functions for a pair of brackets.
INNER-P non-nil means inner, nil means a."
  (let ((func-name (intern (format "helix-mark-%s-%s" (if inner-p "inner" "a") name)))
        (func-doc (format "Select %s %s." (if inner-p "inner" "a") doc))
        (inclusive (if inner-p nil t)))
    `(defun ,func-name (&optional count)
       ,func-doc
       (interactive "p")
       (let* ((range (helix-select-paren ,open ,close
                                        (when (region-active-p) (region-beginning))
                                        (when (region-active-p) (region-end))
                                        nil count ,inclusive)))
         (when range
           (push-mark (car range) nil t)
           (goto-char (cadr range)))))))

(defmacro helix-define-mark-quote (name quote-char doc inner-p)
  "Define mark inner/a functions for a quote character.
INNER-P non-nil means inner, nil means a."
  (let ((func-name (intern (format "helix-mark-%s-%s" (if inner-p "inner" "a") name)))
        (func-doc (format "Select %s %s." (if inner-p "inner" "a") doc))
        (inclusive (if inner-p nil t)))
    `(defun ,func-name (&optional count)
       ,func-doc
       (interactive "p")
       (let* ((range (helix-select-quote ,quote-char
                                        (when (region-active-p) (region-beginning))
                                        (when (region-active-p) (region-end))
                                        nil count ',inclusive)))
         (when range
           (push-mark (car range) nil t)
           (goto-char (cadr range)))))))

(defmacro helix-define-mark-object (name thing doc &optional restricted-p)
  "Define mark inner/a functions for a text object.
THING should be a quoted symbol like \='helix-word.
RESTRICTED-P non-nil means use restricted version (for word/WORD)."
  (let ((inner-name (intern (format "helix-mark-inner-%s" name)))
        (outer-name (intern (format "helix-mark-a-%s" name)))
        (inner-doc (format "Select inner %s." doc))
        (outer-doc (format "Select a %s." doc))
        (inner-func (if restricted-p
                        'helix-select-inner-restricted-object
                      'helix-select-inner-object))
        (outer-func (if restricted-p
                        'helix-select-a-restricted-object
                      'helix-select-a-object)))
    `(progn
       (defun ,inner-name (&optional count)
         ,inner-doc
         (interactive "p")
         (let* ((range (,inner-func ,thing
                                    (when (region-active-p) (region-beginning))
                                    (when (region-active-p) (region-end))
                                    count)))
           (when range
             (push-mark (car range) nil t)
             (goto-char (cdr range)))))
       (defun ,outer-name (&optional count)
         ,outer-doc
         (interactive "p")
         (let* ((range (,outer-func ,thing
                                    (when (region-active-p) (region-beginning))
                                    (when (region-active-p) (region-end))
                                    count)))
           (when range
             (push-mark (car range) nil t)
             (goto-char (cdr range))))))))

(helix-define-mark-object "word" 'helix-word "word" t)
(helix-define-mark-object "WORD" 'helix-WORD "WORD" t)
(helix-define-mark-object "symbol" 'helix-symbol "symbol")
(helix-define-mark-object "sentence" 'helix-sentence "sentence")
(helix-define-mark-object "paragraph" 'helix-paragraph "paragraph")

(helix-define-mark-pair "paren" ?\( ?\) "parenthesis" t)
(helix-define-mark-pair "paren" ?\( ?\) "parenthesis" nil)
(helix-define-mark-pair "bracket" ?\[ ?\] "bracket" t)
(helix-define-mark-pair "bracket" ?\[ ?\] "bracket" nil)
(helix-define-mark-pair "brace" ?\{ ?\} "brace" t)
(helix-define-mark-pair "brace" ?\{ ?\} "brace" nil)
(helix-define-mark-pair "angle" ?\< ?\> "angle" t)
(helix-define-mark-pair "angle" ?\< ?\> "angle" nil)


(helix-define-mark-quote "single-quote" ?' "single-quoted string" t)
(helix-define-mark-quote "single-quote" ?' "single-quoted string" nil)
(helix-define-mark-quote "double-quote" ?\" "double-quoted string" t)
(helix-define-mark-quote "double-quote" ?\" "double-quoted string" nil)
(helix-define-mark-quote "back-quote" ?` "back-quoted string" t)
(helix-define-mark-quote "back-quote" ?` "back-quoted string" nil)

;; ============================================================================
;; tag Text Objects
;; ============================================================================

(defun helix-mark-inner-tag (&optional count)
  "Select inner tag."
  (interactive "p")
  (let* ((range (helix-select-xml-tag
                 (when (region-active-p) (region-beginning))
                 (when (region-active-p) (region-end))
                 nil count nil)))
    (when range
      (push-mark (car range) nil t)
      (goto-char (cadr range)))))

(defun helix-mark-a-tag (&optional count)
  "Select a tag."
  (interactive "p")
  (let* ((range (helix-select-xml-tag
                 (when (region-active-p) (region-beginning))
                 (when (region-active-p) (region-end))
                 nil count t)))
    (when range
      (push-mark (car range) nil t)
      (goto-char (cadr range)))))


;; ============================================================================
;; Keymaps
;; ============================================================================

(defvar-keymap helix-textobj-inner-map
  "w"  #'helix-mark-inner-word
  "W"  #'helix-mark-inner-WORD
  "o"  #'helix-mark-inner-symbol
  "s"  #'helix-mark-inner-sentence
  "p"  #'helix-mark-inner-paragraph
  "("  #'helix-mark-inner-paren
  ")"  #'helix-mark-inner-paren
  "b"  #'helix-mark-inner-paren
  "["  #'helix-mark-inner-bracket
  "]"  #'helix-mark-inner-bracket
  "B"  #'helix-mark-inner-brace
  "{"  #'helix-mark-inner-brace
  "}"  #'helix-mark-inner-brace
  "<"  #'helix-mark-inner-angle
  ">"  #'helix-mark-inner-angle
  "t"  #'helix-mark-inner-tag
  "\`" #'helix-mark-inner-back-quote
  "'"  #'helix-mark-inner-single-quote
  "\"" #'helix-mark-inner-double-quote)

(defvar-keymap helix-textobj-outer-map
  "w"  #'helix-mark-a-word
  "W"  #'helix-mark-a-WORD
  "o"  #'helix-mark-a-symbol
  "s"  #'helix-mark-a-sentence
  "p"  #'helix-mark-a-paragraph
  "("  #'helix-mark-a-paren
  ")"  #'helix-mark-a-paren
  "b"  #'helix-mark-a-paren
  "["  #'helix-mark-a-bracket
  "]"  #'helix-mark-a-bracket
  "B"  #'helix-mark-a-brace
  "{"  #'helix-mark-a-brace
  "}"  #'helix-mark-a-brace
  "<"  #'helix-mark-a-angle
  ">"  #'helix-mark-a-angle
  "t"  #'helix-mark-a-tag
  "\`" #'helix-mark-a-back-quote
  "'"  #'helix-mark-a-single-quote
  "\"" #'helix-mark-a-double-quote)

;; helix-textobj-map is defined in helix-core.el don't replace
;; this with defvar-keymap
(with-eval-after-load 'helix-core
  (set-keymap-parent helix-textobj-map helix-textobj-inner-map))
(define-key helix-textobj-map "i" helix-textobj-inner-map)
(define-key helix-textobj-map "a" helix-textobj-outer-map)

(provide 'helix-textobj)
;;; helix-textobj.el ends here
