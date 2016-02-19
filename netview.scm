;    This program will collect printer resources from the output of Microsoft
;    Windows "net view" command.  Tested with Windows XP + Windows 2003.
;    Copyright (C) 2007 Jeff Zaroyko

;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.

;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.

;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require (planet "scripting.ss" ("ryanc" "scripting.plt" 1 1)))
(require (planet "port.ss" ("schematics" "port.plt" 1 0)))

(define (net-view hostname)
  (let ((net-view-proc (loud:process (string-append "net view " hostname))))
    (port->string-list(car net-view-proc))))

(define (cons-car-self-n lat n)
  (if (zero? n) (cdr lat)
    (cons-car-self-n (cons (car lat) lat) (sub1 n))))

(require (lib "1.ss" "srfi"))
(require (lib "13.ss" "srfi"))

; net view produces 4 columns,
; "Share name" - the resource name, if Print, this is recognised by lpq
; "Type" - Print or Disk
; "Used as" - if this is mapped locally?
; "Comment" - a description

; To successfully parse the output of net view,
; we need to find:
; * the string which contains the column headings
; * the start end end positions of each column based on the headings
; * the remaining list of strings which are Printer resources
;
; and return a list of lists provided tokenized access.

(define (heading-coordinates heading-row)
  (map string-contains 
       (cons-car-self-n (list heading-row) 4) 
       '("Share name" "Type" "Used as" "Comment")))

(define (claw-data positions strings lat)
  (if (null? strings) lat
      (claw-data positions (cdr strings) 
                 (cons 
                  (list (substring (car strings) (first positions) (second positions))
                        (substring (car strings) (second positions) (third positions))
                        (substring (car strings) (third positions) (fourth positions))
                        (substring (car strings) (fourth positions))) lat))))
(define (parse-helper heading resources)
  (if (null? resources) #f (claw-data (heading-coordinates (car heading)) resources '())))

(define (format-resources nv-output)
  (parse-helper (remove (lambda (x) (not (string-contains x "Share name"))) nv-output) ; headers
                (remove (lambda (x) (not (string-contains x "Print"))) nv-output)))    ; resources


; sample input/output
; > (format-resources (net-view "pinochet"));
;  net view pinochet
; (("Xerox451    " "Print  " "         " "Xerox 4517 PostScript Level 2  \r")
;  ("hp          " "Print  " "         " "HP DeskJet 670C                \r")
;  ("bw          " "Print  " "         " "Generic PostScript Printer     \r"))
