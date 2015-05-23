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
(deffacts initialize-startup
          (initialize initialize-startup))
(defglobal MAIN
           ?*stages* = (create$ update-pixel
                                draw
                                wait
                                perhaps-terminate
                                generate-update
                                rules
                                restart))
(defgeneric n+1 
            "Gets the next cell with wraparound ")
(defgeneric n-1
            "Gets the previous cell with wraparound")
(defgeneric count$
            "applies a function to a multifield and counts how many times TRUE is returned")


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
(defmethod count$
  ((?fn SYMBOL)
   (?elements MULTIFIELD))
  (bind ?count 0)
  (progn$ (?e ?elements)
          (if (funcall ?fn ?e) then
            (bind ?count (+ ?count 1))))
  (return ?count))
(defmethod count$
  ((?fn SYMBOL)
   $?elements)
  (count$ ?fn ?elements))

(defclass cell
  (is-a USER)
  (slot state 
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

(deftemplate neighbor-results
             (slot target
                   (default ?NONE))
             (slot num-dead
                   (type INTEGER)
                   (default ?NONE))
             (slot num-alive
                   (type INTEGER)
                   (default ?NONE)))
(deffunction is-dead
             (?symbol)
             (eq ?symbol dead))
(deffunction is-alive
             (?symbol)
             (eq ?symbol alive))
(defrule get-neighbors
         (stage (current generate-update))
         (object (is-a cell)
                 (x ?x)
                 (y ?y)
                 (name ?cell0))
         (object (is-a cell)
                 (x ?x)
                 (y =(n+1 ?y))
                 (state ?state1))
         (object (is-a cell)
                 (x ?x)
                 (y =(n-1 ?y))
                 (state ?state2))
         (object (is-a cell)
                 (x =(n+1 ?x))
                 (y ?y)
                 (state ?state3))
         (object (is-a cell)
                 (x =(n+1 ?x))
                 (y =(n+1 ?y))
                 (state ?state4))
         (object (is-a cell)
                 (x =(n+1 ?x))
                 (y =(n-1 ?y))
                 (state ?state5))
         (object (is-a cell)
                 (x =(n-1 ?x))
                 (y ?y)
                 (state ?state6))
         (object (is-a cell)
                 (x =(n-1 ?x))
                 (y =(n+1 ?y))
                 (state ?state7))
         (object (is-a cell)
                 (x =(n-1 ?x))
                 (y =(n-1 ?y))
                 (state ?state8))
         =>
         (bind ?states (create$ ?state1 
                                ?state2 
                                ?state3 
                                ?state4 
                                ?state5 
                                ?state6 
                                ?state7 
                                ?state8))
         (assert (neighbor-results (target ?cell0)
                                   (num-dead (count$ is-dead 
                                                     ?states))
                                   (num-alive (count$ is-alive 
                                                      ?states)))))

(defrule update-pixels:dead
         (stage (current update-pixel))
         (object (is-a cell)
                 (x ?x)
                 (y ?y)
                 (state dead))
         =>
         (unicornhat:set-pixel-color (unicornhat:get-pixel-position ?x ?y)
                                     0 0 0))
(defrule update-pixels:alive
         (stage (current update-pixel))
         (object (is-a cell)
                 (x ?x)
                 (y ?y)
                 (state alive))
         =>
         (unicornhat:set-pixel-color (unicornhat:get-pixel-position ?x ?y)
                                     128 0 128))

(defrule draw
         (stage (current draw))
         =>
         (unicornhat:show))

(defrule update-pixel:underpopulation
         (declare (salience 1))
         (stage (current rules))
         ?f <- (neighbor-results (num-alive ?na&:(< ?na 2))
                                 (target ?cell))
         (object (is-a cell)
                 (name ?cell)
                 (state alive))
         =>
         (retract ?f)
         (modify-instance ?cell 
                          (state dead)))
(defrule update-pixel:keep-alive
         (declare (salience 1))
         (stage (current rules))
         ?f <- (neighbor-results (num-alive ?number&:(or (= ?number 2)
                                                         (= ?number 3)))
                                 (target ?cell))
         (object (is-a cell)
                 (name ?cell)
                 (state alive))
         =>
         (retract ?f))

(defrule update-pixel:overpopulation
         (declare (salience 1))
         (stage (current rules))
         ?f <- (neighbor-results (num-alive ?number&:(> ?number 3))
                                 (target ?cell))
         (object (is-a cell)
                 (name ?cell)
                 (state alive))
         =>
         (retract ?f)
         (modify-instance ?cell (state dead)))

(defrule update-pixel:resurrection
         (declare (salience 1))
         (stage (current rules))
         ?f <- (neighbor-results (num-alive ?number&:(= ?number 3))
                                 (target ?cell))
         (object (is-a cell)
                 (name ?cell)
                 (state dead))
         =>
         (retract ?f)
         (modify-instance ?cell (state alive)))

(defrule update-pixel:do-nothing
         (stage (current rules))
         ?f <- (neighbor-results)
         =>
         (retract ?f))

(defrule restart-process
         ?f <- (stage (current restart)
                      (rest $?rest))
         =>
         (bind ?new-stage (expand$ (first$ ?*stages*)))
         (bind ?contents (create$ (rest$ ?*stages*)
                                  ?rest))
         (modify ?f (current ?new-stage)
                 (rest ?contents)))

(defrule terminate
         ?f <- (stage (current perhaps-terminate))
         (not (exists (object (is-a cell) 
                              (state alive))))
         =>
         (retract ?f))
(defrule startup
         (declare (salience 10000))
         ?f <- (initialize ?deffacts)
         =>
         (retract ?f)
         (undeffacts ?deffacts)
         (unicornhat:set-brightness 50)
         (loop-for-count (?a 0 63) do
                         (unicornhat:set-pixel-color ?a 0 0 0))
         (bind ?target (expand$ (first$ ?*stages*)))
         (bind ?rest (rest$ ?*stages*))
         (assert (stage (current ?target)
                        (rest ?rest))))
