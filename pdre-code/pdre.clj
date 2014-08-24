(ns pdre)

(defmulti deriv
 "Multimethod to calculate the derivative of a regular expression re
  with respect to the character c."
    (fn [ re c ] (first re)))

(defmulti is-accepting
 "Multimethod- returns true if the given regular expression re is accepting."
    first)

(defmulti is-error
 "Multimethod- returns true if the given regular expression re is an error."
    first)

(defn re-matches-deriv
 "Returns true if s (a string or sequence of characters) matches the
  regular expression re."
    [ re s ]
    (loop [ r re st (seq s) ]
        (cond
            (is-error r) false
            (empty? st) (is-accepting r)
            true (recur (deriv r (first st)) (rest st)))))

(defmethod deriv 'error [ re c ] '(error))
(defmethod is-accepting 'error [ re ] false)
(defmethod is-error 'error [ re ] true)

(defmethod deriv 'accepting [ re c ] '(error))
(defmethod is-accepting 'accepting [ re ] true)
(defmethod is-error 'accepting [ re ] false)

(defmethod deriv 'char [ [ _ c ] x ]
    (if (= c x)
        '(accepting)
        '(error)))
(defmethod is-accepting 'char [ re ] false)
(defmethod is-error 'char [ re ] false)

(defmethod deriv 'maybe [ [ _ re ] x ]
    (deriv re x))
(defmethod is-accepting 'maybe [ re ] true)
(defmethod is-error 'maybe [ [ _ re ] ] false)

(defmethod deriv 'or [ [ _ & res ] x ]
    (let [ r (filter #(not (is-error %))
                (map #(deriv % x) res)) ]
        (cond
            (empty? r) '(error)
            (empty? (rest r)) (first r)
            true (cons 'or r))))
(defmethod is-accepting 'or [ [ _ & res ] ]
    (boolean (some is-accepting res)))
(defmethod is-error 'or [ [ _ & res ] ]
    (not (not-every? is-error res)))

