;; interval sets of longs
(defn interval-contains [x [a b]] (<= a x b))

(defn interval-set-contains [x interval-set]
  (first (filter (partial interval-contains x) interval-set)))

(defn check-set [set]
  (let [sorted (sort set)]
    (doseq [i (range 0 (- (count sorted) 1))]
      (let [[a1 b1] (nth sorted i)
            [a2 b2] (nth sorted (inc i))]
        (when (>= b1 a2)
          (throw (ex-info "intervals overlap"
                          {:int1 [a1 b1] :int2 [a2 b2]})))
        (when (= (inc b1) a2)
          (throw (ex-info "intervals consecutive"
                          {:int1 [a1 b1] :int2 [a2 b2]})))))))

(defn complement [set]
  (let [sorted (sort set)
        [max-upper comple]
        (reduce (fn [[lower-limit intervals] [a b]]
                  [(when-not (= b Long/MAX_VALUE) (inc b))
                   (if (= a lower-limit)
                     intervals
                     (conj intervals [lower-limit (dec a)]))]
                  ) [Long/MIN_VALUE []] sorted)]
    (if max-upper
      (conj comple [max-upper Long/MAX_VALUE])
      comple)))

(defn insert [set [a b]]
  (let [sorted (sort set)
        a-interval (interval-set-contains a sorted)
        b-interval (interval-set-contains b sorted)
        lower (if a-interval (min a (first a-interval)) a)
        upper (if b-interval (max b (second b-interval)) b)
        [less greater] (split-with (fn [[_ b1]] (< b1 lower)) sorted)]
    (concat
     less
     [[lower upper]]
     (drop-while (fn [[_ b2]] (<= b2 upper)) greater))))

(defn normalize [[i1 i2 & set]]
  (cond
    (nil? i1)
    ()

    (nil? i2)
    (list i1)

    (= (last i1) (dec (first i2)))
    (normalize (cons [(first i1) (last i2)] set))

    :else
    (cons i1 (normalize (conj set i2)))))

(defn union [s1 s2] (normalize (reduce insert s1 s2)))

(defn intersection [s1 s2] (normalize (complement (union (complement s1) (complement s2)))))

(def long? (partial instance? Long))

(def all-values [Long/MIN_VALUE Long/MAX_VALUE])

(defn all-values? [[a b]] (and (= a Long/MIN_VALUE) (= b Long/MAX_VALUE)))

(defn intervals->set [intervals]
  (reduce
   (fn [acc [v interval-set]]
     (cond
       (empty? interval-set)
       (reduced :empty-env)

       (all-values? (first interval-set))
       acc

       :else
       (assoc acc v interval-set)))
   {}
   intervals))

(defn env-complement [env]
  (cond
    (= env :empty-env)
    {}

    (= env {})
    :empty-env

    :else
    (intervals->set (map (juxt key (comp complement val)) env))))

(defn env-intersection [env1 env2]
  (cond
    (= env1 :empty-env)
    :empty-env

    (= env2 :empty-env)
    :empty-env

    :else
    (intervals->set (map (fn [v] [v (intersection (env1 v [all-values]) (env2 v [all-values]))])
                         (distinct (concat (keys env1) (keys env2)))))))

(defn env-union [env1 env2]
  (cond
    (= env1 :empty-env)
    env2

    (= env2 :empty-env)
    env1

    :else
    (intervals->set (map (fn [v] [v (union (env1 v [all-values]) (env2 v [all-values]))])
                         (distinct (concat (keys env1) (keys env2)))))))

(defn exp->set [e]
  (cond
    (= true e)
    {}

    (= false e)
    :empty-env

    (list? e)
    (let [[op p1 p2 p3] e]
      (when (and ('#{= <= >= < >} op) (or (not (symbol? p1)) (not (long? p2))))
        (throw (ex-info "only var to long supported" {:op op :p1 p1 :p2 p2})))
      (case op
        = {p1 [[p2 p2]]}
        <= (if-not (= p2 Long/MAX_VALUE) {p1 [[Long/MIN_VALUE p2]]} {})
        >= (if-not (= p2 Long/MIN_VALUE) {p1 [[p2 Long/MAX_VALUE]]} {})
        < (if-not (= p2 Long/MIN_VALUE) {p1 [[Long/MIN_VALUE (dec p2)]]} :empty-env)
        > (if-not (= p2 Long/MAX_VALUE) {p1 [[(inc p2) Long/MAX_VALUE]]} :empty-env)
        not
        (env-complement (exp->set p1))
        and
        (env-intersection (exp->set p1) (exp->set p2))
        or
        (env-union (exp->set p1) (exp->set p2))

        if
        (let [env-cond (exp->set p1)
              env-t (exp->set p2)
              env-f (exp->set p3)]
          (cond
            (= env-cond :empty-env)
            env-f

            (= env-cond {})
            env-t

            :else
            (env-union
             (env-intersection env-cond env-t)
             (env-intersection (env-complement env-cond) env-f))
            )
          )

        (throw (ex-info "exp->set: no match" {:op op :p1 p1 :p2 p2}))))))

(def swapped-op
  '{= =
    <= >=
    >= <=
    < >
    > <})

(defn normalize-exp [[op p1 p2]]
  (if (and (long? p1) (symbol? p2))
    (conj (list p2 p1) (swapped-op op))
    (list op p1 p2)))

(def negated-op
  '{= not=
    not= =
    <= >
    >= <
    < >=
    > <=})

(defn negate-comparison [[op & params]]
  (conj
   params
   (or (negated-op op)
       (throw (ex-info "negate-comparison: operator not supported"
                       {:op op :params params})))))

(comment
  (complement [[5 10] [11 20]])
  (check-set [[5 10] [11 20]])
  (-> [[5 10]]
      complement
      complement)
  (complement [[Long/MIN_VALUE 4] [11 Long/MAX_VALUE]])
  (complement [])
  (complement [[Long/MIN_VALUE 5]])
  (complement [[Long/MIN_VALUE Long/MAX_VALUE]])
  (complement [[5 Long/MAX_VALUE]])

  (normalize [[1 2] [3 3] [4 4]])
  (normalize [])
  (normalize [[Long/MIN_VALUE Long/MAX_VALUE]])
  (normalize [])
  (normalize [[1 3]])
  (normalize [[1 3] [5 7] [8 9]])
  (normalize [[1 2] [4 5] [7 9]])
  (normalize [all-values])
  (-> (insert [[3 4] [11 9223372036854775807]] [5 7])
      normalize)

  (insert [[1 2] [4 6] [9 11]] [2 9])
  (insert [[1 2] [4 6] [9 11]] [2 5])
  (insert [[1 2] [4 6] [9 11]] [2 12])
  (insert [[1 2] [4 6] [9 11]] [5 6])
  (insert [[1 2] [4 6] [9 11]] [0 16])
  (insert [[1 2] [4 6] [9 11]] [0 0])
  (insert [[1 2] [4 6] [9 11]] [3 3])
  (union [[1 2] [4 6] [9 11]] (complement [[1 2] [4 6] [9 11]]))

  (intersection [[1 2] [4 6] [9 10]] [[2 9] [11 11]])
  (intersection [[2 4]] [[7 9]])
  (interval-set-contains 2 (intersection [[1 2] [4 6] [9 10]] [[2 9] [11 11]]))

  (interval-set-contains 3 [[2 4]])
  (interval-set-contains 30 [[2 4]])

  (exp->set '(= x 5))
  (exp->set '(<= x 5))
  (exp->set '(>= x 5))
  (exp->set '(not (= x 5)))
  (exp->set '(< x 5))
  (exp->set '(< x -9223372036854775808))
  (exp->set '(> x 9223372036854775807))
  (exp->set '(<= x 9223372036854775807))
  (exp->set '(not (< x -9223372036854775808)))
  (exp->set '(not (> x 9223372036854775807)))
  (exp->set '(not (<= x 9223372036854775807)))
  (exp->set '(not (= x 5)))

  (exp->set '(and (= x 5) (= x 5)))
  (exp->set '(and (= x 5) (not (= x 5))))
  (exp->set '(not (and (= x 5) (not (= x 5)))))
  (exp->set '(and (< x -9223372036854775808) (= y 2)))
  (exp->set '(and (<= x 9223372036854775807) (= y 2)))
  (exp->set '(and (= x 5) (and (= y 9) (not (= x 5))))) ; should be empty env
  (exp->set '(or (<= x 5) (= x 6)))
  (exp->set '(or
              (and (= x 5) (= y 6))
              (and (= x 15) (= y 16))))

  (exp->set '(and (and (= x 5) (not (= x 5))) (= y 7)))
  (exp->set '(or (and (= x 5) (not (= x 5))) (= y 7)))
  (exp->set '(and (= x 5) (and (= y 4) (not (= x 5)))))
  (exp->set '(and (= x 5) (and (= y 9) (not (= x 5)))))
  (exp->set '(or (<= x 5) (= x 6)))
  (exp->set '(or
              (and (= x 5) (= y 6))
              (and (= x 15) (= y 16))))

  (exp->set '(and (= x 5) (or
                           (and (= x 5) (= y 6))
                           (and (= x 15) (= y 16)))))

  (exp->set             '(and (= x 5) (and (= y 6) (not (= x 5)))))
  (exp->set '(or
              (and (= x 5) (and (= y 6) (not (= x 5))))
              (and (= x 15) (= y 16))))


  (exp->set '(if (< x -9223372036854775808) (= x 5) (= x 6)))
  (exp->set '(if (<= x 9223372036854775807) (= x 5) (= x 6)))
  (exp->set '(if (and (>= x 5) (<= x 10))
               (<= x 7)
               (>= x 3)))
  (exp->set '(and true true))
  (exp->set '(if (= x 7) (= x 5) (= x 6)))
  (exp->set false)
  (env-intersection
   (exp->set '(and (>= x 5) (<= x 10))) (exp->set '(<= x 7)))
  (env-intersection
   (env-complement (exp->set '(and (>= x 5) (<= x 10)))) (exp->set '(>= x 3)))

  (defn f [x] (if (and (>= x 5) (<= x 10))
                (<= x 7)
                (>= x 3)))
  (f Long/MIN_VALUE)

  (exp->set '(and (not (and (>= x 5) (<= x 10)))
                  (> x 0)))


  (normalize-exp '(= 4 x))
  (normalize-exp '(= x 4))
  (normalize-exp '(<= 4 x))

  (negate-comparison '(= x 5))
  (negate-comparison '(not= x 5))
  (negate-comparison '(== x 5))


  )
