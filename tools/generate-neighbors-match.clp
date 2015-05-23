(deffunction select-op
             (?var ?index)
             (switch ?index
                     (case 0  then (format nil "?%s" ?var))
                     (case 1  then (format nil "=(n+1 ?%s)" ?var))
                     (case -1 then (format nil "=(n-1 ?%s)" ?var))
                     (default "")))
(progn (bind ?index 0)
       (bind ?contents (create$ 0 1 -1))
       (progn$ (?x ?contents)
               (bind ?xcond (select-op x ?x))
               (progn$ (?y ?contents)
                       (bind ?ycond (select-op y ?y))
                       (format t (if (= ?x ?y 0) then
                                   "(object (is-a board)%n(x %s)%n(y %s)%n(name ?cell%d))%n"
                                   else
                                   "(object (is-a board)%n(x %s)%n(y %s)%n(state ?state%d))%n")
                               ?xcond
                               ?ycond
                               ?index)
                       (bind ?index (+ ?index 1)))))
(exit)
