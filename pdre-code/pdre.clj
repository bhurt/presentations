(ns pdre)

(defmulti is-accepting
 "Multimethod- returns true if the given regular expression re is accepting."
    first)

(defmulti is-error
 "Multimethod- returns true if the given regular expression re is an error."
    first)

(defmulti deriv
 "Multimethod to calculate the derivative of a regular expression re
  with respect to the character c."
    (fn [ re c ] (first re)))

(defn re-matches-deriv
 "Returns true if s (a string or sequence of characters) matches the
  regular expression re."
    [ re s ]
    (loop [ r re st (seq s) ]
        (cond
            (is-error r) false
            (empty? st) (is-accepting r)
            true (recur (deriv r (first st)) (rest st)))))

(defmethod is-accepting 'error [ re ] false)
(defmethod is-error 'error [ re ] true)
(defmethod deriv 'error [ re c ] '(error))

(defmethod is-accepting 'accepting [ re ] true)
(defmethod is-error 'accepting [ re ] false)
(defmethod deriv 'accepting [ re c ] '(error))

(defmethod is-accepting 'char [ re ] false)
(defmethod is-error 'char [ re ] false)
(defmethod deriv 'char [ [ _ c ] x ]
    (if (= c x)
        '(accepting)
        '(error)))

(defmethod is-accepting 'maybe [ re ] true)
(defmethod is-error 'maybe [ [ _ re ] ] false)
(defmethod deriv 'maybe [ [ _ re ] x ]
    (deriv re x))

(defmethod is-accepting 'zero-or-more [ _ ] true)
(defmethod is-error 'zero-or-more [ _ ] false)
(defmethod deriv 'zero-or-more [ [ _ re ] x ]
    (let [ re' (deriv re) ]
        (if (is-error re')
            '(error)
            `(re-seq ~re' (zero-or-more ~re)))))

(defn- re-union [ re1 re2 ]
    (cond
        (is-error re1) re2
        (is-error re2) re1
        true `(or ~re1 ~re2)))

(defmethod is-accepting 're-seq [ [ _ re1 re2 ] ]
    (and (is-accepting re1) (is-accepting re2)))
(defmethod is-error 're-seq [ [ _ re1 re2 ] ]
    (or (is-error re1) (is-error re2)))
(defmethod deriv 're-seq [ [ _ re1 re2 ] x ]
    (if (is-accepting re1)
        (re-union
            `(re-seq ~(deriv re1 x) ~re2)
            (deriv re2 x))
        (let [ re1' (deriv re1 x) ]
            (if (is-error re1')
                '(error)
                `(re-seq ~re1 ~re2)))))

(defmethod is-accepting 'or [ [ _ re1 re2 ] ]
    (or (is-accepting re1) (is-accepting re2)))
(defmethod is-error 'or [ [ _ re1 re2 ] ]
    (and (is-error re1) (is-error re2)))
(defmethod deriv 'or [ [ _ re1 re2 ] x ]
    (re-union (deriv re1 x) (deriv re2 x)))

(defn re-intersection [ re1 re2 ]
    (if (or (is-error re1) (is-error re2))
        '(error)
        `(and ~re1 ~re2)))

(defmethod is-accepting 'and [ [ _ re1 re2 ] ]
    (and (is-accepting re1) (is-accepting re2)))
(defmethod is-error 'and [ [ _ re1 re2 ] ]
    (or (is-error re1) (is-error re2)))
(defmethod deriv 'and [ [ _ re1 re2 ] x ]
    (re-intersection (deriv re1 x) (deriv re2 x)))

(defmethod deriv 'not [ [ _ re ] x ]
    `(not (deriv re x)))
(defmethod is-accepting 'not [ [ _ re ] ]
    (not (is-accepting re)))
(defmethod is-error 'not [ [ _ re ] ] false)

(def ^:dynamic max-state nil)
(def ^:dynamic state-to-idx nil)
(def ^:dynamic idx-to-state nil)
(def ^:dynamic accepting-states nil)

(defn- make-state [ re' ]
    (let [ idx' (swap! max-state inc) ]
        (swap! state-to-idx assoc re' idx')
        (swap! idx-to-state assoc idx' re')
        idx'))

(defn- make-trans [ re ]
    (into-array Integer/TYPE
        (for [ c (map char (range 0 127)) ]
            (let [ re' (deriv re c) ]
                (if (is-error re')
                    -1
                    (or
                        (get @state-to-idx re')
                        (make-state re')))))))

(defn- make-states [ idx ]
    (when (<= idx @max-state)
        (let [  re (get @idx-to-state idx)
                trans (make-trans re) ]
            (when (is-accepting re)
                (swap! accepting-states conj idx))
            (cons trans (make-state (inc idx))))))

(defn make-dfa [ re ]
    (binding [  max-state (atom 0)
                state-to-idx (atom { re 0 })
                idx-to-state (atom { 0 re })
                accepting-states (atom #{}) ]
        (let [ table (into-array (make-states 0)) ]
            [ table @accepting-states ])))

(defn dfa-matches [ dfa s ]
    (let [ [ table accpts ] dfa ]
        (loop [ state 0 st s ]
            (cond
                (= state -1) false
                (empty? st)
                    (boolean (accpts state))
                true
                    (recur
                        (aget (aget table state)
                                (int (first st)))
                        (rest st))))))

