;;; helix.el --- Tests for Helix minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Graham Marlow

;; Author: Graham Marlow
;;         Corentin Roy
;; Keywords: tests
;; Version: 0
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

;;; Commentary:

;; Helix tests.

;;; Code:

(require 'ert)
(require 'helix)

;;; Forward long word tests

(ert-deftest helix-test-forward-long-word-start-basic-movement ()
  "Test basic forward movement between words."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 6)) ; before "world"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-start)
    (should (= (point) 12)) ; before "test"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-start)
    (should (= (point) (- (point-max) 1))) ; before end of line
    (should (= (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-forward-long-word-start-hyphenated-words ()
  "Test forward movement with hyphenated words (long words)."
  (with-temp-buffer
    (insert "this test-string-example works")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 5)) ; before "test-string-example"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-start)
    (should (= (point) 25)) ; before "works"
    (should (= (- (region-end) (region-beginning)) 19))))

(ert-deftest helix-test-forward-long-word-start-on-whitespace ()
  "Test that forward movement skips over whitespace."
  (with-temp-buffer
    (insert "word next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word-start)
    (should (= (point) (- (point-max) 1))) ; before end of line
    (should (= (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-forward-long-word-start-on-whitespaces ()
  "Test that forward movement skips over whitespace."
  (with-temp-buffer
    (insert "word   \t  next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word-start)
    (should (= (point) 10)) ; start of "next"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-start-multiple-lines ()
  "Test forward movement across multiple lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird")
    (goto-char 1) ; start of buffer
    (helix-forward-long-word-start)
    (should (= (point) 6)) ; before "line"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-start)
    (should (= (point) 10)) ; before end of first line
    (should (= (- (region-end) (region-beginning)) 3))
    (helix-forward-long-word-start)
    (should (= (point) 18)) ; before "line" of second line
    (should (= (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-forward-long-word-start-empty-lines ()
  "Test forward movement with empty lines."
  (with-temp-buffer
    (insert "first\n\n\nsecond")
    (goto-char 5) ; before end of first line
    (helix-forward-long-word-start)
    (should (= (point) (- (point-max) 1))) ; before end of second line
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-start-at-end-of-buffer ()
  "Test that forward movement at end of buffer doesn't move."
  (with-temp-buffer
    (insert "test word")
    (goto-char (point-max))
    (let ((initial-point (point)))
      (helix-forward-long-word-start)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-forward-long-word-start-mixed-separators ()
  "Test forward movement with mixed word separators."
  (with-temp-buffer
    (insert "word1_part2-part3.part4 next")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 24)) ; before "next"
    (should (= (- (region-end) (region-beginning)) 23))))

(ert-deftest helix-test-forward-long-word-start-punctuation ()
  "Test forward movement with punctuation."
  (with-temp-buffer
    (insert "Hello, world! How are you?")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 7)) ; before "world!"
    (should (= (- (region-end) (region-beginning)) 6))
    (helix-forward-long-word-start)
    (should (= (point) 14)) ; before "How"
    (should (= (- (region-end) (region-beginning)) 6))))

;;; Forward long word end tests

(ert-deftest helix-test-forward-long-word-end-basic-movement ()
  "Test basic forward movement to word ends."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 5)) ; end of "hello"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-end)
    (should (= (point) 11)) ; end of "world"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-end)
    (should (= (point) 16)) ; end of "test"
    (should (= (- (region-end) (region-beginning)) 4))))

(ert-deftest helix-test-forward-long-word-end-hyphenated-words ()
  "Test forward movement to ends of hyphenated words (long words)."
  (with-temp-buffer
    (insert "this test-string-example works")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 4)) ; end of "this"
    (should (= (- (region-end) (region-beginning)) 3))
    (helix-forward-long-word-end)
    (should (= (point) 24)) ; end of "test-string-example"
    (should (= (- (region-end) (region-beginning)) 19))
    (helix-forward-long-word-end)
    (should (= (point) 30)) ; end of "works"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-end-on-whitespace ()
  "Test that forward movement to word ends skips over whitespace."
  (with-temp-buffer
    (insert "word next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word-end)
    (should (= (point) 9)) ; end of "next"
    (should (= (- (region-end) (region-beginning)) 4))))

(ert-deftest helix-test-forward-long-word-end-on-whitespaces ()
  "Test that forward movement to word ends skips over multiple whitespaces."
  (with-temp-buffer
    (insert "word   \t  next")
    (goto-char 5) ; on the first whitespace
    (helix-forward-long-word-end)
    (should (= (point) 14)) ; end of "next"
    (should (= (- (region-end) (region-beginning)) 9))))

(ert-deftest helix-test-forward-long-word-end-multiple-lines ()
  "Test forward movement to word ends across multiple lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird")
    (goto-char 1) ; start of buffer
    (helix-forward-long-word-end)
    (should (= (point) 5)) ; end of "first"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-end)
    (should (= (point) 10)) ; end of "line"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-end)
    (should (= (point) 17)) ; end of "second"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-end-empty-lines ()
  "Test forward movement to word ends with empty lines."
  (with-temp-buffer
    (insert "first\n\n\nsecond")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 5)) ; end of "first"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-forward-long-word-end)
    (should (= (point) 14)) ; end of "second"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-forward-long-word-end-at-end-of-buffer ()
  "Test that forward movement to word end at end of buffer doesn't move."
  (with-temp-buffer
    (insert "test word")
    (goto-char (point-max))
    (let ((initial-point (point)))
      (helix-forward-long-word-end)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-forward-long-word-end-mixed-separators ()
  "Test forward movement to word ends with mixed word separators."
  (with-temp-buffer
    (insert "word1_part2-part3.part4 next")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 23)) ; end of "word1_part2-part3.part4"
    (should (= (- (region-end) (region-beginning)) 22))
    (helix-forward-long-word-end)
    (should (= (point) 28)) ; end of "next"
    (should (= (- (region-end) (region-beginning)) 4))))

(ert-deftest helix-test-forward-long-word-end-punctuation ()
  "Test forward movement to word ends with punctuation."
  (with-temp-buffer
    (insert "Hello, world! How are you?")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 6)) ; end of "Hello,"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-forward-long-word-end)
    (should (= (point) 13)) ; end of "world!"
    (should (= (- (region-end) (region-beginning)) 6))
    (helix-forward-long-word-end)
    (should (= (point) 17)) ; end of "How"
    (should (= (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-forward-long-word-end-empty-buffer ()
  "Test forward movement to word end in empty buffer."
  (with-temp-buffer
    (let ((initial-point (point)))
      (helix-forward-long-word-end)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

;;; Backward long word tests

(ert-deftest helix-test-backward-long-word-basic-movement ()
  "Test basic backward movement between words."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 13)) ; start of "test"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-backward-long-word)
    (should (= (point) 7)) ; start of "world"
    (should (= (- (region-end) (region-beginning)) 6))
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "hello"
    (should (= (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-backward-long-word-hyphenated-words ()
  "Test backward movement with hyphenated words (long words)."
  (with-temp-buffer
    (insert "this test-string-example works")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 26)) ; start of "works"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-backward-long-word)
    (should (= (point) 6)) ; start of "test-string-example"
    (should (= (- (region-end) (region-beginning)) 20))))

(ert-deftest helix-test-backward-long-word-on-whitespace ()
  "Test that backward movement skips over whitespace."
  (with-temp-buffer
    (insert "word next")
    (goto-char 5) ; on whitespace
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "word"
    (should (= (- (region-end) (region-beginning)) 4))))

(ert-deftest helix-test-backward-long-word-on-whitespaces ()
  "Test that backward movement skips over whitespace."
  (with-temp-buffer
    (insert "word   \t  next")
    (goto-char 10) ; on the last whitespace
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "word"
    (should (= (- (region-end) (region-beginning)) 9))))

(ert-deftest helix-test-backward-long-word-multiple-lines ()
  "Test backward movement across multiple lines."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 24)) ; start of "third"
    (should (= (- (region-end) (region-beginning)) 5))
    (helix-backward-long-word)
    (should (= (point) 19)) ; start of "line"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-backward-long-word)
    (should (= (point) 12)))) ; start of "second"

(ert-deftest helix-test-backward-long-word-empty-lines ()
  "Test backward movement with empty lines."
  (with-temp-buffer
    (insert "first\n\n\nsecond")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 9)) ; start of "second"
    (should (= (- (region-end) (region-beginning)) 6))
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "first"
    (should (= (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-backward-long-word-at-beginning-of-buffer ()
  "Test that backward movement at beginning of buffer doesn't move."
  (with-temp-buffer
    (insert "test word")
    (goto-char 1)
    (let ((initial-point (point)))
      (helix-backward-long-word)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-backward-long-word-mixed-separators ()
  "Test backward movement with mixed word separators."
  (with-temp-buffer
    (insert "word1_part2-part3.part4 next")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 25)) ; start of "next"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-backward-long-word)
    (should (= (point) 1)) ; start of "word1_part2-part3.part4"
    (should (= (- (region-end) (region-beginning)) 24))))

(ert-deftest helix-test-backward-long-word-punctuation ()
  "Test backward movement with punctuation."
  (with-temp-buffer
    (insert "Hello, world! How are you?")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 23)) ; start of "you?"
    (should (= (- (region-end) (region-beginning)) 4))
    (helix-backward-long-word)
    (should (= (point) 19)) ; start of "are"
    (should (= (- (region-end) (region-beginning)) 4))))

;;; Edge case tests

(ert-deftest helix-test-forward-long-word-empty-buffer ()
  "Test forward movement in empty buffer."
  (with-temp-buffer
    (let ((initial-point (point)))
      (helix-forward-long-word-start)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-backward-long-word-empty-buffer ()
  "Test backward movement in empty buffer."
  (with-temp-buffer
    (let ((initial-point (point)))
      (helix-backward-long-word)
      (should (= (point) initial-point))
      (should (not (use-region-p))))))

(ert-deftest helix-test-forward-long-word-start-only-whitespace ()
  "Test forward movement in buffer with only whitespace."
  (with-temp-buffer
    (insert "   \t\n  ")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 4)) ; before end of first line
    (should (= (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-forward-long-word-end-only-whitespace ()
  "Test forward movement to word end in buffer with only whitespace."
  (with-temp-buffer
    (insert "   \t\n  ")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 4)) ; end of first line whitespace
    (should (= (- (region-end) (region-beginning)) 3))
    (helix-forward-long-word-end)
    (should (= (point) 7)) ; end of buffer
    (should (= (- (region-end) (region-beginning)) 1))))

(ert-deftest helix-test-backward-long-word-only-whitespace ()
  "Test backward movement in buffer with only whitespace."
  (with-temp-buffer
    (insert "   \t\n  ")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 6)) ; start of second line
    (should (= (- (region-end) (region-beginning) 2)))))

(ert-deftest helix-test-forward-long-word-start-single-character ()
  "Test forward movement with single character words."
  (with-temp-buffer
    (insert "a b c d")
    (goto-char 1)
    (helix-forward-long-word-start)
    (should (= (point) 2)) ; before "b"
    (should (= (- (region-end) (region-beginning)) 1))
    (helix-forward-long-word-start)
    (should (= (point) 4)) ; before "c"
    (should (= (- (region-end) (region-beginning)) 1))))

(ert-deftest helix-test-forward-long-word-end-single-character ()
  "Test forward movement to word ends with single character words."
  (with-temp-buffer
    (insert "a b c d")
    (goto-char 1)
    (helix-forward-long-word-end)
    (should (= (point) 3)) ; end of "b"
    (should (= (- (region-end) (region-beginning)) 1))
    (helix-forward-long-word-end)
    (should (= (point) 5)) ; end of "c"
    (should (= (- (region-end) (region-beginning)) 1))
    (helix-forward-long-word-end)
    (should (= (point) 7)) ; end of "d"
    (should (= (- (region-end) (region-beginning)) 1))))

(ert-deftest helix-test-backward-long-word-single-character ()
  "Test backward movement with single character words."
  (with-temp-buffer
    (insert "a b c d")
    (goto-char (point-max))
    (helix-backward-long-word)
    (should (= (point) 7)) ; start of "d"
    (should (= (- (region-end) (region-beginning)) 1))
    (helix-backward-long-word)
    (should (= (point) 5)) ; start of "c"
    (should (= (- (region-end) (region-beginning)) 2))))

;; Find char

(ert-deftest helix-test-find-next-char ()
  "Test finding next character and selecting from current position to it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-next-char ?d)
    (should (eql (point) 13))
    (should (eql (- (region-end) (region-beginning)) 12))))

(ert-deftest helix-test-find-next-char-two-line ()
  "Test finding next character across multiple lines and selecting to it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char 1)
    (helix-find-next-char ?d)
    (should (eql (point) 13))
    (should (eql (- (region-end) (region-beginning)) 12))))

(ert-deftest helix-test-find-till-char ()
  "Test finding till character and selecting from current position to before it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-till-char ?d)
    (should (eql (point) 12))
    (should (eql (- (region-end) (region-beginning)) 11))))

(ert-deftest helix-test-find-till-char-two-line ()
  "Test finding till character across multiple lines and selecting to before it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char 1)
    (helix-find-till-char ?d)
    (should (eql (point) 12))
    (should (eql (- (region-end) (region-beginning)) 11))))

(ert-deftest helix-test-find-till-char-repeat ()
  "Test repeating find till character operation to next occurrence."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-till-char ?d)
    (helix-find-repeat)
    (should (eql (point) 18))
    (should (eql (- (region-end) (region-beginning)) 5))))

(ert-deftest helix-test-find-next-char-repeat ()
  "Test repeating find next character operation to next occurrence."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-next-char ?d)
    (helix-find-repeat)
    (should (eql (point) 19))
    (should (eql (- (region-end) (region-beginning)) 6))))

(ert-deftest helix-test-find-prev-char ()
  "Test finding previous character and selecting to it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char (point-max))
    (helix-find-prev-char ?s)
    (should (eql (point) 7))
    (should (eql (- (region-end) (region-beginning)) 12))))

(ert-deftest helix-test-find-prev-char-multiple-lines ()
  "Test finding previous character across multiple lines and selecting to it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char (point-max))
    (helix-find-prev-char ?s)
    (should (eql (point) 7))
    (should (eql (- (region-end) (region-beginning)) 12))))

(ert-deftest helix-test-find-prev-till-char ()
  "Test finding previous character and selecting till (after) it."
  (with-temp-buffer
    (insert "first second third")
    (goto-char (point-max))
    (helix-find-prev-till-char ?s)
    (should (eql (point) 8))
    (should (eql (- (region-end) (region-beginning)) 11))))

(ert-deftest helix-test-find-prev-till-char-multiple-lines ()
  "Test finding previous character across multiple lines and selecting till (after) it."
  (with-temp-buffer
    (insert "first\nsecond\nthird")
    (goto-char (point-max))
    (helix-find-prev-till-char ?s)
    (should (eql (point) 8))
    (should (eql (- (region-end) (region-beginning)) 11))))

(ert-deftest helix-test-find-prev-char-repeat ()
  "Test repeating previous character find operation and extending selection."
  (with-temp-buffer
    (insert "first second third")
    (goto-char (point-max))
    (helix-find-prev-char ?s)
    (helix-find-repeat)
    (should (eql (point) 4))
    (should (eql (- (region-end) (region-beginning)) 3))))

(ert-deftest helix-test-find-prev-till-char-repeat ()
  "Test repeating previous till character find operation and extending selection."
  (with-temp-buffer
    (insert "first second third")
    (goto-char (point-max))
    (helix-find-prev-till-char ?s)
    (helix-find-repeat)
    (should (eql (point) 5))
    (should (eql (- (region-end) (region-beginning)) 2))))

(ert-deftest helix-test-empty-find-repeat ()
  "Test find repeat when nothing to repeat."
  (with-temp-buffer
    (insert "first second third")
    (goto-char 1)
    (helix-find-repeat)
    (should (eql (point) 1))))

;;; helix-define-key tests

(ert-deftest helix-test-define-key-standard ()
  "Test standard helix-define-key without optional keymap."
  (let ((original-binding (lookup-key helix-space-map "t")))
    (unwind-protect
        (progn
          (helix-define-key 'space "t" #'ignore)
          (should (eq (lookup-key helix-space-map "t") #'ignore)))
      (define-key helix-space-map "t" original-binding))))

(ert-deftest helix-test-define-key-with-mode ()
  "Test helix-define-key with MODE stores binding in helix--mode-keybindings."
  (let ((helix--mode-keybindings nil))
    (helix-define-key 'normal "j" #'next-line 'dired-mode)
    (let ((entry (assoc (cons 'dired-mode 'normal) helix--mode-keybindings)))
      (should entry)
      (should (eq (lookup-key (cdr entry) "j") #'next-line)))))

(ert-deftest helix-test-define-key-with-mode-multiple-bindings ()
  "Test that multiple bindings for the same mode and state accumulate."
  (let ((helix--mode-keybindings nil))
    (helix-define-key 'normal "j" #'next-line 'dired-mode)
    (helix-define-key 'normal "k" #'previous-line 'dired-mode)
    (let ((entry (assoc (cons 'dired-mode 'normal) helix--mode-keybindings)))
      (should (eq (lookup-key (cdr entry) "j") #'next-line))
      (should (eq (lookup-key (cdr entry) "k") #'previous-line)))))

(ert-deftest helix-test-define-key-with-mode-different-states ()
  "Test that different states get different sparse keymaps."
  (let ((helix--mode-keybindings nil))
    (helix-define-key 'normal "j" #'next-line 'dired-mode)
    (helix-define-key 'insert "j" #'self-insert-command 'dired-mode)
    (let ((normal-entry (assoc (cons 'dired-mode 'normal) helix--mode-keybindings))
          (insert-entry (assoc (cons 'dired-mode 'insert) helix--mode-keybindings)))
      (should normal-entry)
      (should insert-entry)
      (should-not (eq (cdr normal-entry) (cdr insert-entry)))
      (should (eq (lookup-key (cdr normal-entry) "j") #'next-line))
      (should (eq (lookup-key (cdr insert-entry) "j") #'self-insert-command)))))

(ert-deftest helix-test-define-key-invalid-state ()
  "Test that invalid state signals an error."
  (should-error (helix-define-key 'invalid-state "t" #'ignore)))

(ert-deftest helix-test-define-key-invalid-state-with-mode ()
  "Test that invalid state signals error even with explicit mode."
  (should-error (helix-define-key 'invalid-state "t" #'ignore 'dired-mode)))

;;; helix--refresh-overriding-maps tests

(ert-deftest helix-test-refresh-overriding-maps-with-major-mode-bindings ()
  "Test that refresh builds correct minor-mode-overriding-map-alist."
  (let ((helix--mode-keybindings nil))
    (with-temp-buffer
      ;; Simulate a major mode
      (setq major-mode 'helix-test-mode)
      (setq-local helix--current-state 'normal)
      ;; Register a binding for this mode
      (helix-define-key 'normal "j" #'next-line 'helix-test-mode)
      (helix--refresh-overriding-maps)
      ;; Should have an entry in minor-mode-overriding-map-alist
      (let ((entry (assq 'helix-normal-mode minor-mode-overriding-map-alist)))
        (should entry)
        (should (eq (lookup-key (cdr entry) "j") #'next-line))))))

(ert-deftest helix-test-refresh-overriding-maps-clears-when-no-bindings ()
  "Test that refresh clears overriding alist when no bindings apply."
  (let ((helix--mode-keybindings nil))
    (with-temp-buffer
      (setq-local helix--current-state 'normal)
      ;; Pre-populate with a stale entry
      (setq minor-mode-overriding-map-alist
            (list (cons 'helix-normal-mode (make-sparse-keymap))))
      (helix--refresh-overriding-maps)
      ;; Should have cleared the entry
      (should-not (assq 'helix-normal-mode minor-mode-overriding-map-alist)))))

(ert-deftest helix-test-refresh-overriding-maps-no-cross-mode-leak ()
  "Test that bindings for one major mode don't leak into another."
  (let ((helix--mode-keybindings nil))
    (with-temp-buffer
      ;; Register binding for dired-mode
      (helix-define-key 'normal "j" #'next-line 'dired-mode)
      ;; But current buffer is a different major mode
      (setq major-mode 'fundamental-mode)
      (setq-local helix--current-state 'normal)
      (helix--refresh-overriding-maps)
      ;; Should have no overriding entry
      (should-not (assq 'helix-normal-mode minor-mode-overriding-map-alist)))))

(ert-deftest helix-test-refresh-overriding-maps-fallback-to-base ()
  "Test that non-overridden keys fall back to base helix keymap."
  (let ((helix--mode-keybindings nil))
    (with-temp-buffer
      (setq major-mode 'helix-test-mode)
      (setq-local helix--current-state 'normal)
      ;; Override only "j"
      (helix-define-key 'normal "j" #'next-line 'helix-test-mode)
      (helix--refresh-overriding-maps)
      (let ((entry (assq 'helix-normal-mode minor-mode-overriding-map-alist)))
        (should entry)
        ;; Overridden key works
        (should (eq (lookup-key (cdr entry) "j") #'next-line))
        ;; Non-overridden key falls back to helix binding
        (should (eq (lookup-key (cdr entry) "k") #'helix-previous-line))))))

;;; Word text object tests

(ert-deftest helix-test-textobj-word-basic ()
  "Test basic word text object selection."
  (with-temp-buffer
    (insert ";; This buffer is for notes.")
    (goto-char 4) ; at "T" of "This"
    (call-interactively #'helix-mark-inner-word)
    (should (eql (region-beginning) 4))
    (should (eql (region-end) 8)))
  (with-temp-buffer
    (insert ";; This buffer is for notes.")
    (goto-char 4)
    (call-interactively #'helix-mark-a-word)
    (should (eql (region-beginning) 4))
    (should (eql (region-end) 9))))

(ert-deftest helix-test-textobj-word-select-first ()
  "Test selecting first word in buffer."
  (with-temp-buffer
    (insert "(a)")
    (goto-char 2) ; inside the parens, on "a"
    (call-interactively #'helix-mark-inner-word)
    (should (eql (region-beginning) 2))
    (should (eql (region-end) 3))))

(ert-deftest helix-test-textobj-word-whitespace-line-bound ()
  "Test selecting word when surrounded by whitespace."
  (with-temp-buffer
    (insert "foo\n  bar")
    (goto-char 7)
    (call-interactively #'helix-mark-inner-word)
    (should (= (region-beginning) 7))))

(ert-deftest helix-test-textobj-WORD-basic ()
  "Test basic WORD text object selection."
  (with-temp-buffer
    (insert ";; This buffer is for notes.")
    (goto-char 4)
    (call-interactively #'helix-mark-inner-WORD)
    (should (= (region-beginning) 4))
    (should (= (region-end) 8))))

(ert-deftest helix-test-textobj-word-cjk ()
  "Test word text object with CJK characters."
  (with-temp-buffer
    (insert "abc漢字")
    (goto-char 1)
    (call-interactively #'helix-mark-inner-word)
    (should (= (region-beginning) 1))
    (should (= (region-end) 4))))

;;; Symbol text object tests

(ert-deftest helix-test-textobj-symbol-basic ()
  "Test basic symbol text object selection."
  (with-temp-buffer
    (insert ";; This buffer is for notes.")
    (goto-char 4) ; at "T" of "This"
    (call-interactively #'helix-mark-inner-symbol)
    (should (= (region-beginning) 4))
    (should (= (region-end) 8))))

;;; Sentence text object tests

(ert-deftest helix-test-textobj-sentence-basic ()
  "Test basic sentence text object selection."
  (with-temp-buffer
    (insert "This is sentence one. This is sentence two.")
    (goto-char 1)
    (call-interactively #'helix-mark-inner-sentence)
    (should (= (region-beginning) 1))
    (should (= (region-end) 44))))

(ert-deftest helix-test-textobj-sentence-select ()
  "Test selecting sentence from middle."
  (with-temp-buffer
    (insert "This is sentence one. This is sentence two.")
    (goto-char 10)
    (call-interactively #'helix-mark-inner-sentence)
    (should (= (region-beginning) 1))
    (should (= (region-end) 44))))

;;; Paragraph text object tests

(ert-deftest helix-test-textobj-paragraph-basic ()
  "Test basic paragraph text object selection."
  (with-temp-buffer
    (insert ";; This buffer is for notes,
;; and for Lisp evaluation.

;; Another paragraph here.")
    (goto-char 1)
    (call-interactively #'helix-mark-inner-paragraph)
    (should (= (region-beginning) 1))
    (should (= (region-end) 58))))

(ert-deftest helix-test-textobj-paragraph-select ()
  "Test selecting paragraph at different positions."
  (with-temp-buffer
    (insert "First paragraph.

Second paragraph.")
    (goto-char 1)
    (call-interactively #'helix-mark-inner-paragraph)
    (should (= (region-beginning) 1))
    (should (= (region-end) 18))))

;;; Outer (a) text object tests

(ert-deftest helix-test-textobj-a-word ()
  "Test a-word text object selection."
  (with-temp-buffer
    (insert ";; This buffer is for notes.")
    (goto-char 4)
    (call-interactively #'helix-mark-a-word)
    (should (= (region-beginning) 4))
    (should (= (region-end) 9))))

(ert-deftest helix-test-textobj-a-symbol ()
  "Test a-symbol text object selection."
  (with-temp-buffer
    (insert ";; This buffer is for notes.")
    (goto-char 4)
    (call-interactively #'helix-mark-a-symbol)
    (should (= (region-beginning) 4))
    (should (= (region-end) 9))))

(ert-deftest helix-test-textobj-a-sentence ()
  "Test a-sentence text object selection."
  (with-temp-buffer
    (insert "This is sentence one. This is sentence two.")
    (goto-char 1)
    (call-interactively #'helix-mark-a-sentence)
    (should (= (region-beginning) 1))
    (should (= (region-end) 44))))

(ert-deftest helix-test-textobj-a-paragraph ()
  "Test a-paragraph text object selection."
  (with-temp-buffer
    (insert ";; This buffer is for notes,
;; and for Lisp evaluation.

;; Another paragraph here.")
    (goto-char 1)
    (call-interactively #'helix-mark-a-paragraph)
    (should (= (region-beginning) 1))
    (should (= (region-end) 58))))

;;; Paren text object tests

 (ert-deftest helix-test-textobj-paren-inner ()
  "Test inner paren text object."
  (with-temp-buffer
    (insert "(hello)")
    (goto-char 2)
    (call-interactively #'helix-mark-inner-paren)
    (should (= (region-beginning) 2))
    (should (= (region-end) 7))))

 (ert-deftest helix-test-textobj-paren-outer ()
  "Test outer paren text object."
  (with-temp-buffer
    (insert "(hello)")
    (goto-char 2)
    (call-interactively #'helix-mark-a-paren)
    (should (= (region-beginning) 1))
    (should (= (region-end) 8))))

;;; Bracket text object tests

 (ert-deftest helix-test-textobj-bracket-inner ()
  "Test inner bracket text object."
  (with-temp-buffer
    (insert "[hello]")
    (goto-char 2)
    (call-interactively #'helix-mark-inner-bracket)
    (should (= (region-beginning) 2))
    (should (= (region-end) 7))))

 (ert-deftest helix-test-textobj-bracket-outer ()
  "Test outer bracket text object."
  (with-temp-buffer
    (insert "[hello]")
    (goto-char 2)
    (call-interactively #'helix-mark-a-bracket)
    (should (= (region-beginning) 1))
    (should (= (region-end) 8))))

;;; Brace text object tests

 (ert-deftest helix-test-textobj-brace-inner ()
  "Test inner brace text object."
  (with-temp-buffer
    (insert "{hello}")
    (goto-char 2)
    (call-interactively #'helix-mark-inner-brace)
    (should (= (region-beginning) 2))
    (should (= (region-end) 7))))

 (ert-deftest helix-test-textobj-brace-outer ()
  "Test outer brace text object."
  (with-temp-buffer
    (insert "{hello}")
    (goto-char 2)
    (call-interactively #'helix-mark-a-brace)
    (should (= (region-beginning) 1))
    (should (= (region-end) 8))))

;;; Angle bracket text object tests

 (ert-deftest helix-test-textobj-angle-inner ()
  "Test inner angle bracket text object."
  (with-temp-buffer
    (insert "<hello>")
    (goto-char 2)
    (call-interactively #'helix-mark-inner-angle)
    (should (= (region-beginning) 2))
    (should (= (region-end) 7))))

 (ert-deftest helix-test-textobj-angle-outer ()
  "Test outer angle bracket text object."
  (with-temp-buffer
    (insert "<hello>")
    (goto-char 2)
    (call-interactively #'helix-mark-a-angle)
    (should (= (region-beginning) 1))
    (should (= (region-end) 8))))

;;; Quote text object tests

(ert-deftest helix-test-textobj-single-quote-inner ()
  "Test inner single-quote text object."
  (with-temp-buffer
    (insert "'hello'")
    (goto-char 2)
    (call-interactively #'helix-mark-inner-single-quote)
    (should (= (region-beginning) 2))
    (should (= (region-end) 7))))

(ert-deftest helix-test-textobj-single-quote-outer ()
  "Test outer single-quote text object."
  (with-temp-buffer
    (insert "'hello'")
    (goto-char 2)
    (call-interactively #'helix-mark-a-single-quote)
    (should (= (region-beginning) 1))
    (should (= (region-end) 8))))

(ert-deftest helix-test-textobj-double-quote-inner ()
  "Test inner double-quote text object."
  (with-temp-buffer
    (insert "\"hello\"")
    (goto-char 2)
    (call-interactively #'helix-mark-inner-double-quote)
    (should (= (region-beginning) 2))
    (should (= (region-end) 7))))

(ert-deftest helix-test-textobj-double-quote-outer ()
  "Test outer double-quote text object."
  (with-temp-buffer
    (insert "\"hello\"")
    (goto-char 2)
    (call-interactively #'helix-mark-a-double-quote)
    (should (= (region-beginning) 1))
    (should (= (region-end) 8))))

(ert-deftest helix-test-textobj-back-quote-inner ()
  "Test inner back-quote text object."
  (with-temp-buffer
    (insert "`hello`")
    (goto-char 2)
    (call-interactively #'helix-mark-inner-back-quote)
    (should (= (region-beginning) 2))
    (should (= (region-end) 7))))

(ert-deftest helix-test-textobj-back-quote-outer ()
  "Test outer back-quote text object."
  (with-temp-buffer
    (insert "`hello`")
    (goto-char 2)
    (call-interactively #'helix-mark-a-back-quote)
    (should (= (region-beginning) 1))
    (should (= (region-end) 8))))

;;; Tag text object tests

(ert-deftest helix-test-textobj-tag-inner ()
  "Test inner tag text object."
  (with-temp-buffer
    (insert "<foo>bar</foo>")
    (goto-char 2)
    (call-interactively #'helix-mark-inner-tag)
    (should (= (region-beginning) 6))
    (should (= (region-end) 9))))

(ert-deftest helix-test-textobj-tag-outer ()
  "Test outer tag text object."
  (with-temp-buffer
    (insert "<foo>bar</foo>")
    (goto-char 2)
    (call-interactively #'helix-mark-a-tag)
    (should (= (region-beginning) 1))
    (should (= (region-end) 15))))

;;; Line-wise helper tests

(defmacro helix-test-with-buffer (content &rest body)
  "Execute BODY in a temp buffer with CONTENT and transient-mark-mode on.
Buffer starts with point at position 1."
  (declare (indent 1))
  `(with-temp-buffer
     (transient-mark-mode 1)
     (insert ,content)
     (goto-char 1)
     ,@body))

(ert-deftest helix-test-linewise-text-adds-newline ()
  "Test that `helix--linewise-text' ensures trailing newline."
  (let ((text (helix--linewise-text "hello")))
    (should (string= text "hello\n"))
    (should (eq (car (get-text-property 0 'yank-handler text))
                'helix--yank-handler-line-wise))))

(ert-deftest helix-test-linewise-text-preserves-existing-newline ()
  "Test that `helix--linewise-text' doesn't double newline."
  (let ((text (helix--linewise-text "hello\n")))
    (should (string= text "hello\n"))
    (should (eq (car (get-text-property 0 'yank-handler text))
                'helix--yank-handler-line-wise))))

(ert-deftest helix-test-linewise-kill-p-positive ()
  "Test `helix--linewise-kill-p' detects line-wise text."
  (let ((text (helix--linewise-text "hello\n")))
    (should (helix--linewise-kill-p text))))

(ert-deftest helix-test-linewise-kill-p-negative ()
  "Test `helix--linewise-kill-p' returns nil for plain text."
  (should-not (helix--linewise-kill-p "hello")))

(ert-deftest helix-test-linewise-kill-p-nil ()
  "Test `helix--linewise-kill-p' returns nil when no kill ring."
  (let ((kill-ring nil))
    (should-not (helix--linewise-kill-p))))

;;; helix--selection-type tests

(ert-deftest helix-test-selection-type-nil-without-region ()
  "Test `helix--selection-type' returns nil when no region."
  (helix-test-with-buffer "hello"
    (should-not (helix--selection-type))))

(ert-deftest helix-test-selection-type-line-validated ()
  "Test `helix--selection-type' validates line selection bounds."
  (helix-test-with-buffer "first line\nsecond line\nthird line"
    (push-mark (point) t t)
    (end-of-line)
    (setq helix--selection-type 'line)
    (should (eq (helix--selection-type) 'line))))

(ert-deftest helix-test-selection-type-line-invalidated ()
  "Test `helix--selection-type' rejects invalid line selection."
  (helix-test-with-buffer "first line\nsecond line"
    ;; region doesn't start at bol
    (goto-char 3)
    (push-mark (point) t t)
    (end-of-line)
    (setq helix--selection-type 'line)
    (should-not (helix--selection-type))))

;;; helix-select-line sets selection type

(ert-deftest helix-test-select-line-sets-type ()
  "Test `helix-select-line' sets `helix--selection-type' to line."
  (helix-test-with-buffer "first line\nsecond line\nthird line"
    (setq helix--selection-type nil)
    (helix-select-line)
    (should (eq helix--selection-type 'line))
    (should (region-active-p))))

;;; helix--clear-data resets selection type

(ert-deftest helix-test-clear-data-resets-type ()
  "Test `helix--clear-data' resets `helix--selection-type'."
  (helix-test-with-buffer "hello"
    (setq helix--selection-type 'line)
    (helix--clear-data)
    (should-not helix--selection-type)))

;;; helix--line-bounds-of-region tests

(ert-deftest helix-test-line-bounds-single-line ()
  "Test line bounds expansion for a single line selection."
  (helix-test-with-buffer "first line\nsecond line\nthird line"
    ;; Select "first line" (bol to eol)
    (push-mark (point) t t)
    (end-of-line)
    (let ((bounds (helix--line-bounds-of-region)))
      (should bounds)
      ;; beg=1, end includes newline=12
      (should (= (car bounds) 1))
      (should (= (cdr bounds) 12)))))

(ert-deftest helix-test-line-bounds-multi-line ()
  "Test line bounds expansion for a multi-line selection."
  (helix-test-with-buffer "first line\nsecond line\nthird line"
    ;; Select from middle of first to middle of second
    (goto-char 5)
    (push-mark (point) t t)
    (goto-char 18)
    (let ((bounds (helix--line-bounds-of-region)))
      (should bounds)
      ;; Should expand to cover both full lines
      (should (= (car bounds) 1))
      (should (= (cdr bounds) 24)))))

(ert-deftest helix-test-line-bounds-last-line-no-newline ()
  "Test line bounds at end of buffer without trailing newline."
  (helix-test-with-buffer "first line\nlast line"
    (goto-char 12)
    (push-mark (point) t t)
    (goto-char (point-max))
    (let ((bounds (helix--line-bounds-of-region)))
      (should bounds)
      (should (= (car bounds) 12))
      ;; end should be point-max since no trailing newline
      (should (= (cdr bounds) (point-max))))))

;;; helix-kill-ring-save (y) line-wise tests

(ert-deftest helix-test-kill-ring-save-linewise ()
  "Test `helix-kill-ring-save' tags text as line-wise."
  (helix-test-with-buffer "first line\nsecond line\nthird line"
    (let ((kill-ring nil))
      (helix-select-line)
      (helix-kill-ring-save)
      (should (helix--linewise-kill-p (car kill-ring)))
      ;; Should include trailing newline
      (should (string= (car kill-ring) "first line\n"))
      ;; Buffer content unchanged
      (should (string= (buffer-string) "first line\nsecond line\nthird line")))))

(ert-deftest helix-test-kill-ring-save-charwise ()
  "Test `helix-kill-ring-save' does not tag charwise text."
  (helix-test-with-buffer "hello world"
    (let ((kill-ring nil))
      (push-mark (point) t t)
      (goto-char 6)
      (setq helix--selection-type nil)
      (helix-kill-ring-save)
      (should-not (helix--linewise-kill-p (car kill-ring)))
      (should (string= (car kill-ring) "hello")))))

;;; helix-kill-thing-at-point (d) line-wise tests

(ert-deftest helix-test-kill-thing-linewise ()
  "Test `helix-kill-thing-at-point' kills whole line and tags line-wise."
  (helix-test-with-buffer "first line\nsecond line\nthird line"
    (let ((kill-ring nil))
      (helix-select-line)
      (helix-kill-thing-at-point)
      ;; Kill ring should have the line with trailing newline
      (should (helix--linewise-kill-p (car kill-ring)))
      (should (string= (car kill-ring) "first line\n"))
      ;; Buffer should have remaining lines
      (should (string= (buffer-string) "second line\nthird line")))))

(ert-deftest helix-test-kill-thing-linewise-last-line ()
  "Test killing the last line (no trailing newline in buffer)."
  (helix-test-with-buffer "first line\nlast line"
    (let ((kill-ring nil))
      (goto-char 12)
      (helix-select-line)
      (helix-kill-thing-at-point)
      (should (helix--linewise-kill-p (car kill-ring)))
      (should (string= (buffer-string) "first line\n")))))

(ert-deftest helix-test-kill-thing-linewise-multi-line ()
  "Test killing multiple lines selected with helix-select-line."
  (helix-test-with-buffer "line one\nline two\nline three"
    (let ((kill-ring nil))
      (helix-select-line)
      (helix-select-line) ;; extend to second line
      (helix-kill-thing-at-point)
      (should (helix--linewise-kill-p (car kill-ring)))
      (should (string= (car kill-ring) "line one\nline two\n"))
      (should (string= (buffer-string) "line three")))))

(ert-deftest helix-test-kill-thing-charwise ()
  "Test `helix-kill-thing-at-point' without line-wise selection."
  (helix-test-with-buffer "hello world"
    (let ((kill-ring nil))
      (push-mark (point) t t)
      (goto-char 6)
      (setq helix--selection-type nil)
      (helix-kill-thing-at-point)
      (should-not (helix--linewise-kill-p (car kill-ring)))
      (should (string= (buffer-string) " world")))))

(ert-deftest helix-test-kill-thing-no-region ()
  "Test `helix-kill-thing-at-point' deletes char when no region."
  (helix-test-with-buffer "hello"
    (helix-kill-thing-at-point)
    (should (string= (buffer-string) "ello"))))

;;; helix-yank (p) line-wise tests

(ert-deftest helix-test-yank-linewise-below ()
  "Test `helix-yank' pastes line-wise content below current line."
  (helix-test-with-buffer "first line\nsecond line"
    ;; Put a line-wise kill in the kill ring
    (kill-new (helix--linewise-text "new line\n"))
    ;; Cursor on first line
    (goto-char 5)
    (let ((this-command 'helix-yank))
      (helix-yank))
    ;; "new line" should appear between first and second
    (should (string= (buffer-string) "first line\nnew line\nsecond line"))))

(ert-deftest helix-test-yank-linewise-at-last-line ()
  "Test `helix-yank' pastes line-wise content below last line."
  (helix-test-with-buffer "only line"
    (kill-new (helix--linewise-text "new line\n"))
    (goto-char 5)
    (let ((this-command 'helix-yank))
      (helix-yank))
    (should (string= (buffer-string) "only line\nnew line"))))

(ert-deftest helix-test-yank-charwise ()
  "Test `helix-yank' pastes charwise content at point."
  (helix-test-with-buffer "hello world"
    (kill-new "XYZ")
    (goto-char 6)
    (helix-yank)
    (should (string= (buffer-string) "helloXYZ world"))))

;;; helix-yank-before (P) line-wise tests

(ert-deftest helix-test-yank-before-linewise ()
  "Test `helix-yank-before' pastes line-wise content above current line."
  (helix-test-with-buffer "first line\nsecond line"
    (kill-new (helix--linewise-text "new line\n"))
    ;; Cursor on second line
    (goto-char 15)
    (let ((this-command 'helix-yank-before))
      (helix-yank-before))
    ;; "new line" should appear between first and second
    (should (string= (buffer-string) "first line\nnew line\nsecond line"))))

(ert-deftest helix-test-yank-before-linewise-first-line ()
  "Test `helix-yank-before' pastes above first line."
  (helix-test-with-buffer "only line"
    (kill-new (helix--linewise-text "new line\n"))
    (let ((this-command 'helix-yank-before))
      (helix-yank-before))
    (should (string= (buffer-string) "new line\nonly line"))))

(ert-deftest helix-test-yank-before-charwise ()
  "Test `helix-yank-before' pastes charwise content at point."
  (helix-test-with-buffer "hello world"
    (kill-new "XYZ")
    (goto-char 6)
    (helix-yank-before)
    (should (string= (buffer-string) "helloXYZ world"))))

;;; helix-replace-yanked (R) line-wise tests

(ert-deftest helix-test-replace-yanked-linewise-selection-linewise-kill ()
  "Test replacing line-wise selection with line-wise kill."
  (helix-test-with-buffer "first line\nsecond line\nthird line"
    (kill-new (helix--linewise-text "REPLACED\n"))
    ;; Select second line
    (goto-char 12)
    (helix-select-line)
    (helix-replace-yanked)
    (should (string= (buffer-string) "first line\nREPLACED\nthird line"))))

(ert-deftest helix-test-replace-yanked-linewise-selection-charwise-kill ()
  "Test replacing line-wise selection with charwise kill."
  (helix-test-with-buffer "first line\nsecond line\nthird line"
    (kill-new "INLINE")
    (goto-char 12)
    (helix-select-line)
    (helix-replace-yanked)
    ;; Charwise kill replaces the full line
    (should (string= (buffer-string) "first line\nINLINE\nthird line"))))

(ert-deftest helix-test-replace-yanked-charwise-selection-linewise-kill ()
  "Test replacing charwise selection with line-wise kill (strips newline)."
  (helix-test-with-buffer "hello world"
    (kill-new (helix--linewise-text "REPLACED\n"))
    (push-mark (point) t t)
    (goto-char 6)
    (setq helix--selection-type nil)
    (helix-replace-yanked)
    ;; Line-wise kill should be stripped of trailing newline for inline replace
    (should (string= (buffer-string) "REPLACED world"))))

(ert-deftest helix-test-replace-yanked-no-region ()
  "Test replacing char at point with kill ring content."
  (helix-test-with-buffer "hello"
    (kill-new "X")
    (setq helix--selection-type nil)
    (setq helix--current-selection nil)
    (helix-replace-yanked)
    (should (string= (buffer-string) "Xello"))))

(ert-deftest helix-test-replace-yanked-empty-kill-ring ()
  "Test replace with empty kill ring shows message."
  (helix-test-with-buffer "hello"
    (let ((kill-ring nil))
      (helix-replace-yanked)
      ;; Buffer unchanged
      (should (string= (buffer-string) "hello")))))

;;; Integration: select-line -> kill -> yank round-trip

(ert-deftest helix-test-linewise-round-trip ()
  "Test full round-trip: select line, kill, then yank elsewhere."
  (helix-test-with-buffer "line A\nline B\nline C"
    (let ((kill-ring nil))
      ;; Select and kill line B
      (goto-char 8)
      (helix-select-line)
      (helix-kill-thing-at-point)
      (should (string= (buffer-string) "line A\nline C"))
      ;; Now yank (paste below) on line A
      (goto-char 1)
      (let ((this-command 'helix-yank))
        (helix-yank))
      (should (string= (buffer-string) "line A\nline B\nline C")))))

(ert-deftest helix-test-linewise-copy-yank-round-trip ()
  "Test round-trip: select line, copy, then yank-before."
  (helix-test-with-buffer "line A\nline B\nline C"
    (let ((kill-ring nil))
      ;; Select and copy line A
      (helix-select-line)
      (helix-kill-ring-save)
      ;; Yank before line C
      (goto-char 15) ;; on line C
      (let ((this-command 'helix-yank-before))
        (helix-yank-before))
      (should (string= (buffer-string) "line A\nline B\nline A\nline C")))))

(ert-deftest helix-test-charwise-not-affected ()
  "Test that charwise operations are unaffected by line-wise changes."
  (helix-test-with-buffer "hello world"
    (let ((kill-ring nil))
      (push-mark (point) t t)
      (goto-char 6)
      (setq helix--selection-type nil)
      (helix-kill-thing-at-point)
      (should (string= (car kill-ring) "hello"))
      (should-not (helix--linewise-kill-p (car kill-ring)))
      (goto-char 1)
      (helix-yank)
      (should (string= (buffer-string) "hello world")))))

;;; helix-begin-selection clears line type

(ert-deftest helix-test-begin-selection-clears-line-type ()
  "Test that `helix-begin-selection' clears line selection type."
  (helix-test-with-buffer "hello"
    (setq helix--selection-type 'line)
    (helix-begin-selection)
    (should-not helix--selection-type)))

(provide 'helix-test)
;;; helix-test.el ends here
