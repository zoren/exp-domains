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

(defn normalize [set]
  (reduce (fn [acc [a b]] (if (and (last acc) (= (inc (second (last acc))) a))
                            (conj (butlast acc) [(first (last acc)) b])
                            (conj acc [a b]))) [] set))

(defn union [s1 s2] (normalize (reduce insert s1 s2)))

(defn intersection [s1 s2] (normalize (complement (union (complement s1) (complement s2)))))

(defn comparison->set [op c]
  (case op
    = [c c]
    <= [Long/MIN_VALUE c]
    >= [c Long/MAX_VALUE]
    < (when-not (= c Long/MIN_VALUE) [Long/MIN_VALUE (dec c)])
    > (when-not (= c Long/MAX_VALUE) [(inc c) Long/MAX_VALUE])
    ))

(defn exp->set [[op p1 p2]]
  (if ('#{= <= >= < >} op)
    (do
      (when-not (and (symbol? p1) (instance? Long p2))
        (throw (ex-info "only var to const supported" {:p1 p1 :p2 p2})))
      (if-let [set (comparison->set op p2)]
        {p1 [set]}
        {p1 []})
      )
    (case op
      not (into {} (map (juxt key (comp complement val)) (exp->set p1)))
      and (merge-with intersection (exp->set p1) (exp->set p2))
      or (merge-with union (exp->set p1) (exp->set p2))

      (throw (ex-info "exp->set: no match" {:op op :p1 p1 :p2 p2})))))

(def negated-op
  '{= =
    <= >
    >= <
    < >=
    > <=})

(defn normalize-exp [[op p1 p2]]
  (if (and (instance? Long p1) (symbol? p2))
    (conj (list p2 p1) (negated-op op))
    (list op p1 p2)))

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
  (normalize *1)
  (normalize [])
  (normalize [[Long/MIN_VALUE Long/MAX_VALUE]])

  (insert [[1 2] [4 6] [9 11]] [2 9])
  (insert [[1 2] [4 6] [9 11]] [2 5])
  (insert [[1 2] [4 6] [9 11]] [2 12])
  (insert [[1 2] [4 6] [9 11]] [5 6])
  (insert [[1 2] [4 6] [9 11]] [0 16])
  (insert [[1 2] [4 6] [9 11]] [0 0])
  (insert [[1 2] [4 6] [9 11]] [3 3])

  (union [[1 2] [4 6] [9 11]] (complement [[1 2] [4 6] [9 11]]))

  (intersection [[1 2] [4 6] [9 10]] [[2 9] [11 11]])
  (interval-set-contains 2 (intersection [[1 2] [4 6] [9 10]] [[2 9] [11 11]]))

  (interval-set-contains 3 [[2 4]])
  (interval-set-contains 30 [[2 4]])

  (exp->set '(= x 5))
  (exp->set '(<= x 5))
  (exp->set '(>= x 5))
  (exp->set '(not (= x 5)))
  (exp->set '(and (= x 5) (= x 5)))
  (exp->set '(and (= x 5) (not (= x 5))))
  (exp->set '(or (<= x 5) (= x 6)))
  (exp->set '(or
              (and (= x 5) (= y 6))
              (and (= x 15) (= y 16))))
  (normalize-exp '(= 4 x))
  (normalize-exp '(= x 4))
  (normalize-exp '(<= 4 x))

  )
