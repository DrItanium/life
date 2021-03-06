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
                                restart)
           ; r g b
           ?*dead-pixel* = (create$ 0 0 0)
           ?*live-pixel* = (create$ 128 128 128)
           ?*display-min* = 0
           ?*display-max* = 7
           ?*board-min* = ?*display-min*
           ?*board-max* = ?*display-max*)

(defgeneric n+1 
            "Gets the next cell with wraparound ")
(defgeneric n-1
            "Gets the previous cell with wraparound")
(defgeneric count$
            "applies a function to a multifield and counts how many times TRUE is returned")


(defmethod n+1
  ((?n INTEGER (<= ?*board-min* (+ ?n 1) ?*board-max*)))
  (+ ?n 1))
(defmethod n+1
  ((?n INTEGER (> (+ ?n 1) ?*board-max*)))
  ?*board-min*)
(defmethod n+1
  ((?n INTEGER (< (+ ?n 1) ?*board-min*)))
  ?*board-max*)
(defmethod n-1
  ((?n INTEGER (<= ?*board-min* (- ?n 1) ?*board-max*)))
  (- ?n 1))
(defmethod n-1
  ((?n INTEGER (> (- ?n 1) ?*board-max*)))
  ?*board-min*)
(defmethod n-1
  ((?n INTEGER (< (- ?n 1) ?*board-min*)))
  ?*board-max*)
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
        (default ?NONE))
  (multislot neighbors
             (type INSTANCE)
             (allowed-classes cell))
  (message-handler is-dead primary)
  (message-handler is-alive primary))

(deffunction is-dead
             (?symbol)
             (send ?symbol is-dead))
(deffunction is-alive
             (?symbol)
             (send ?symbol is-alive))
(defmessage-handler cell is-dead primary
                    ()
                    (eq ?self:state dead))
(defmessage-handler cell is-alive primary
                    ()
                    (eq ?self:state alive))


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
             (slot num-alive
                   (type INTEGER)
                   (default ?NONE)))

(defrule get-neighbors
         (stage (current generate-update))
         (object (is-a cell)
                 (x ?x)
                 (y ?y)
                 (neighbors $?neighbors)
                 (name ?cell))
         =>
         (assert (neighbor-results (target ?cell)
                                   (num-alive (count$ is-alive 
                                                      ?neighbors)))))
(deffunction in-display-range
             (?index)
             (<= ?*display-min* ?index ?*display-max*))
(defrule update-pixels:dead
         (stage (current update-pixel))
         (object (is-a cell)
                 (x ?x&:(in-display-range ?x))
                 (y ?y&:(in-display-range ?y))
                 (state dead))
         =>
         (unicornhat:set-pixel-color (unicornhat:get-pixel-position ?x ?y)
                                     $?*dead-pixel*))
(defrule update-pixels:alive
         (stage (current update-pixel))
         (object (is-a cell)
                 (x ?x&:(in-display-range ?x))
                 (y ?y&:(in-display-range ?y))
                 (state alive))
         =>
         (unicornhat:set-pixel-color (unicornhat:get-pixel-position ?x ?y)
                                     $?*live-pixel*))

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
         (bind ?contents (rest$ ?*stages*) ?rest)
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
         (assert (stage (current update-constants)
                        (rest build-board
                              pre-compute
                              ?*stages*))))
(defrule build-board
         (stage (current build-board))
         =>
         ; has to be done this way since sequence operators aren't valid in assertions
         (loop-for-count (?x ?*board-min* ?*board-max*) do
                         (loop-for-count (?y ?*board-min* ?*board-max*) do
                                         ; generate the values that make up the neighbors
                                         (assert (compute-neighbors ?x ?y
                                                                    (n-1 ?x) (n-1 ?y)
                                                                    (n+1 ?x) (n+1 ?y)))
                                         (make-instance of cell 
                                                        (x ?x) 
                                                        (y ?y) 
                                                        (state dead)))))
(defrule apply-pixel
         (stage (current pre-compute))
         ?f <- (cell ?x ?y ?state)
         ?cell <- (object (is-a cell) 
                          (x ?x) 
                          (y ?y))
         =>
         (retract ?f)
         (modify-instance ?cell (state ?state)))

(defrule compute-neighbors
         "Collects the set of neighbors for a given cell and stores it in that cell"
         (stage (current pre-compute))
         ?c <- (object (is-a cell)
                       (x ?x)
                       (y ?y))
         ?q <- (compute-neighbors ?x ?y 
                                  ?xm ?ym
                                  ?xp ?yp)
         (object (is-a cell)
                 (x ?x)
                 (y ?yp)
                 (name ?cell1))
         (object (is-a cell)
                 (x ?x)
                 (y ?ym)
                 (name ?cell2))
         (object (is-a cell)
                 (x ?xp)
                 (y ?y)
                 (name ?cell3))
         (object (is-a cell)
                 (x ?xp)
                 (y ?yp)
                 (name ?cell4))
         (object (is-a cell)
                 (x ?xp)
                 (y ?ym)
                 (name ?cell5))
         (object (is-a cell)
                 (x ?xm)
                 (y ?y)
                 (name ?cell6))
         (object (is-a cell)
                 (x ?xm)
                 (y ?yp)
                 (name ?cell7))
         (object (is-a cell)
                 (x ?xm)
                 (y ?ym)
                 (name ?cell8))
         =>
         (retract ?q)
         (modify-instance ?c (neighbors ?cell1 
                                        ?cell2 
                                        ?cell3 
                                        ?cell4 
                                        ?cell5 
                                        ?cell6 
                                        ?cell7 
                                        ?cell8)))
(defrule modify-color-scheme:live-pixel
         (stage (current update-constants))
         ?f <- (live-pixel ?r ?g ?b)
         =>
         (retract ?f)
         (bind ?*live-pixel* ?r ?g ?b))

(defrule modify-color-scheme:dead-pixel
         (stage (current update-constants))
         ?f <- (dead-pixel ?r ?g ?b)
         =>
         (retract ?f)
         (bind ?*dead-pixel* ?r ?g ?b))

(defrule modify-board-max
         (stage (current update-constants))
         ?f <- (board-max ?v)
         =>
         (retract ?f)
         (bind ?*board-max* ?v))
(defrule modify-board-min
         (stage (current update-constants))
         ?f <- (board-min ?v)
         =>
         (retract ?f)
         (bind ?*board-min* ?v))
