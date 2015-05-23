; Copyright (c) 2015, Joshua Scoggins
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
; 
; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
;          
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;  
; The views and conclusions contained in the software and documentation are those
; of the authors and should not be interpreted as representing official policies,
; either expressed or implied, of the FreeBSD Project.(

; life.clp - implements conway's game of life to be played on the unicornhat
(defgeneric n+1 
            "Gets the next cell with wraparound ")
(defgeneric n-1
            "Gets the previous cell with wraparound")

(defmethod n+1
  ((?n INTEGER (<= 0 (+ ?n 1) 7)))
  (+ ?n 1))
(defmethod n+1
  ((?n INTEGER (> (+ ?n 1) 7)))
  0)
(defmethod n+1
  ((?n INTEGER (< (+ ?n 1) 0)))
  7)
(defmethod n-1
  ((?n INTEGER (<= 0 (- ?n 1) 7)))
  (- ?n 1))
(defmethod n-1
  ((?n INTEGER (> (- ?n 1) 7)))
  0)
(defmethod n-1
  ((?n INTEGER (< (- ?n 1) 0)))
  7)
(defclass cell
  (is-a USER)
  (slot status
        (type SYMBOL)
        (allowed-symbols dead
                         alive))
  (slot x
        (type INTEGER)
        (range 0 7)
        (default ?NONE))
  (slot y
        (type INTEGER)
        (range 0 7)
        (default ?NONE)))

(deftemplate pattern-cell
             (slot x
                   (type INTEGER)
                   (range 0 7)
                   (default ?NONE))
             (slot y
                   (type INTEGER)
                   (range 0 7)
                   (default ?NONE)))
(deftemplate stage
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))
(defrule next-stage
         (declare (salience -10000))
         ?f <- (stage (rest ?next $?rest))
         =>
         (modify ?f (current ?next)
                 (rest ?rest)))
(definstances board
              (of cell (x 0) (y 0))
              (of cell (x 0) (y 1))
              (of cell (x 0) (y 2))
              (of cell (x 0) (y 3))
              (of cell (x 0) (y 4))
              (of cell (x 0) (y 5))
              (of cell (x 0) (y 6))
              (of cell (x 0) (y 7))
              (of cell (x 1) (y 0))
              (of cell (x 1) (y 1))
              (of cell (x 1) (y 2))
              (of cell (x 1) (y 3))
              (of cell (x 1) (y 4))
              (of cell (x 1) (y 5))
              (of cell (x 1) (y 6))
              (of cell (x 1) (y 7))
              (of cell (x 2) (y 0))
              (of cell (x 2) (y 1))
              (of cell (x 2) (y 2))
              (of cell (x 2) (y 3))
              (of cell (x 2) (y 4))
              (of cell (x 2) (y 5))
              (of cell (x 2) (y 6))
              (of cell (x 2) (y 7))
              (of cell (x 3) (y 0))
              (of cell (x 3) (y 1))
              (of cell (x 3) (y 2))
              (of cell (x 3) (y 3))
              (of cell (x 3) (y 4))
              (of cell (x 3) (y 5))
              (of cell (x 3) (y 6))
              (of cell (x 3) (y 7))
              (of cell (x 4) (y 0))
              (of cell (x 4) (y 1))
              (of cell (x 4) (y 2))
              (of cell (x 4) (y 3))
              (of cell (x 4) (y 4))
              (of cell (x 4) (y 5))
              (of cell (x 4) (y 6))
              (of cell (x 4) (y 7))
              (of cell (x 5) (y 0))
              (of cell (x 5) (y 1))
              (of cell (x 5) (y 2))
              (of cell (x 5) (y 3))
              (of cell (x 5) (y 4))
              (of cell (x 5) (y 5))
              (of cell (x 5) (y 6))
              (of cell (x 5) (y 7))
              (of cell (x 6) (y 0))
              (of cell (x 6) (y 1))
              (of cell (x 6) (y 2))
              (of cell (x 6) (y 3))
              (of cell (x 6) (y 4))
              (of cell (x 6) (y 5))
              (of cell (x 6) (y 6))
              (of cell (x 6) (y 7))
              (of cell (x 7) (y 0))
              (of cell (x 7) (y 1))
              (of cell (x 7) (y 2))
              (of cell (x 7) (y 3))
              (of cell (x 7) (y 4))
              (of cell (x 7) (y 5))
              (of cell (x 7) (y 6))
              (of cell (x 7) (y 7))
              )

(defmessage-handler
