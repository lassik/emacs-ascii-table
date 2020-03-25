;;; ascii.el --- Display ASCII table in a help buffer -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-ascii
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Package-Version: 0.1.0
;; Keywords: help tools
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Browse ascii pages downloaded from the internet in Emacs.
;;
;;; Code:

(defvar ascii-base 10
  "Number base used for character codes in the ASCII table.

Valid values are 2 (binary), 8 (octal), 10 (decimal), and
16 (hex). Another word for 'base' is 'radix'.")

(defvar ascii-caret-p nil
  "Use ^A notation for control characters in the ASCII table?

If non-nil, control characters use caret notation (^A .. ^?).
Otherwise their names NUL .. DEL are shown.")

(defun ascii--binary (codepoint)
  "Internal helper to format CODEPOINT in binary."
  (setq codepoint (logand codepoint #x7F))
  (string (+ ?0 (logand 1 (lsh codepoint -6)))
          (+ ?0 (logand 1 (lsh codepoint -5)))
          (+ ?0 (logand 1 (lsh codepoint -4)))
          (+ ?0 (logand 1 (lsh codepoint -3)))
          (+ ?0 (logand 1 (lsh codepoint -2)))
          (+ ?0 (logand 1 (lsh codepoint -1)))
          (+ ?0 (logand 1 (lsh codepoint -0)))))

(defun ascii--control-caret (codepoint)
  "Internal helper to format CODEPOINT in caret notation."
  (cond ((< codepoint #x20) (string ?^ (+ ?@ codepoint)))
        ((= codepoint #x7F) "^?")
        (t nil)))

(defun ascii--control-name (codepoint)
  "Internal helper to get the control character name of CODEPOINT."
  (cond ((< codepoint #x20)
         (elt ["NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL"
               "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI"
               "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB"
               "CAN" "EM" "SUB" "ESC" "FS" "GS" "RS" "US"]
              codepoint))
        ((= codepoint #x7F) "DEL")
        (t nil)))

(defun ascii--table (codepoints/row)
  "Internal helper to compute the cells in the ASCII table.

CODEPOINTS/ROW is how many ASCII characters to show in each row.

Each character takes two cells: one for the character number, and
one for the name or other representation."
  (let* ((codepoints 128)
         (rows (ceiling codepoints codepoints/row))
         (cols (* 2 codepoints/row))
         (table (make-vector (* 2 rows cols) "")))
    (cl-do
        ((codepoint 0 (1+ codepoint)))
        ((= codepoint codepoints) table)
      (let* ((code (cl-ecase ascii-base
                     (2  (ascii--binary codepoint))
                     (8  (format "%03o" codepoint))
                     (10 (format "%d" codepoint))
                     (16 (format "%02X" codepoint))))
             (name (or (if ascii-caret-p (ascii--control-caret codepoint)
                         (ascii--control-name codepoint))
                       (string codepoint)))
             (row  (mod codepoint rows))
             (col  (truncate codepoint rows))
             (cell (* 2 (+ (* codepoints/row row) col))))
        (aset table (+ 0 cell) code)
        (aset table (+ 1 cell) name)))))

(defun ascii--column-widths (table cols)
  "Internal helper to compute column widths needed for TABLE.

Assume the table is formatted using COLS columns."
  (let* ((cells (length table))
         (widths (make-vector cols 0)))
    (cl-do
        ((cell 0 (1+ cell)))
        ((= cell cells) widths)
      (let ((col (mod cell cols)))
        (let ((width (length (aref table cell))))
          (aset widths col (max (aref widths col) width)))))))

(defun ascii--width-limit ()
  "Internal helper to get narrowest window width for ASCII table."
  (let ((min-width nil))
    (let ((ascii-buffer (get-buffer "*ASCII*")))
      (when ascii-buffer
        (walk-windows
         (lambda (w)
           (when (eq ascii-buffer (window-buffer w))
             (let ((width (window-width w)))
               (setq min-width (or (and min-width (min min-width width))
                                   width)))))
         nil t)))
    (or min-width (window-width))))

(defun ascii--revert (&optional _arg _noconfirm)
  "Redisplay the *ASCII* buffer, i.e. refresh the ASCII table."
  (let ((inhibit-read-only t))
    (cl-assert (equal major-mode 'ascii-mode))
    (cl-assert (null (buffer-file-name)))
    (erase-buffer)
    (insert "ASCII Table"
            " ("
            (cl-ecase ascii-base
              (2 "binary")
              (8 "octal")
              (10 "decimal")
              (16 "hex"))
            ")\n\n")
    (cl-dolist (codepoints/row '(8 7 6 5 4 3 2 1))
      (let* ((table (ascii--table codepoints/row))
             (cols (* 2 codepoints/row))
             (rows (truncate (truncate (length table) 2) cols))
             (widths (ascii--column-widths table cols))
             (width (+ (cl-reduce #'+ widths)
                       (* 2 (length widths))))
             (width-limit (ascii--width-limit)))
        (when (< width width-limit)
          (cl-dotimes (row rows)
            (cl-dotimes (col cols)
              (let* ((cell (+ col (* row cols)))
                     (contents (aref table cell))
                     (col-width (aref widths col))
                     (pad-amount (max 0 (- col-width (length contents))))
                     (pad (make-string pad-amount ? ))
                     (right-justify-p (= 0 (mod col 2))))
                (unless (= col 0) (insert "  "))
                (if right-justify-p (insert pad contents)
                  (insert contents pad))))
            (insert "\n"))
          (cl-return))))
    (goto-char (point-min))))

(defun ascii--revert-if-active ()
  "Redisplay the *ASCII* buffer if it exists."
  (let ((buffer (get-buffer "*ASCII*")))
    (when buffer (with-current-buffer buffer (ascii--revert)))))

(defun ascii--set-base (base)
  "Internal helper to change the ASCII number base to BASE."
  (setq ascii-base (cl-case base ((2 8 16) base) (t 10)))
  (ascii--revert-if-active))

(defun ascii-toggle-caret ()
  "Switch ASCII table between ^A notation and control character names."
  (interactive)
  (setq ascii-caret-p (not ascii-caret-p))
  (ascii--revert-if-active))

(defun ascii-base-binary ()
  "Switch ASCII table to binary (base 2)."
  (interactive)
  (ascii--set-base 2))

(defun ascii-base-octal ()
  "Switch ASCII table to octal (base 8)."
  (interactive)
  (ascii--set-base 8))

(defun ascii-base-decimal ()
  "Switch ASCII table to decimal (base 10)."
  (interactive)
  (ascii--set-base 10))

(defun ascii-base-hex ()
  "Switch ASCII table to hexadecimal (base 16)."
  (interactive)
  (ascii--set-base 16))

(defvar ascii-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "b") 'ascii-base-binary)
    (define-key map (kbd "c") 'ascii-toggle-caret)
    (define-key map (kbd "d") 'ascii-base-decimal)
    (define-key map (kbd "o") 'ascii-base-octal)
    (define-key map (kbd "x") 'ascii-base-hex)
    map)
  "Keymap for `ascii-mode'.")

(define-derived-mode ascii-mode special-mode "ASCII"
  "Major mode that shows an interactive ASCII table.

\\{ascii-mode-map}"
  (setq-local revert-buffer-function 'ascii--revert)
  (ascii--revert))

;;;###autoload
(defun ascii ()
  "Show an interactive ASCII table in the other window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*ASCII*"))
  (ascii-mode))

(provide 'ascii)

;;; ascii.el ends here
