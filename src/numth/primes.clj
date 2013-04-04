(ns numth.primes)

(defprotocol Primer
  (factor [_ n] "Returns a collection (with repeats) of n's prime factors.")
  (prime? [_ n] "Returns true if argument is a prime.")
  (primes [_] "Returns an infinite seq of primes in order."))

(defn naive-primes-seq
  "Returns a lazy seq of all primes. Uses naive division checks. Holds
  on to its own head."
  []
  (letfn [(primes-tail [so-far]
            (loop [x (+ 2 (last so-far))]
              (let [prime? (->> so-far
                                (take-while #(<= (* % %) x))
                                (filter #(zero? (rem x %)))
                                (empty?))]
                (if prime?
                  ;; this is terrible; we're building up a lazy seq
                  ;; and a vector at the same time
                  (cons x (lazy-seq (primes-tail (conj so-far x))))
                  (recur (+ 2 x))))))]
    (list* 2 3 (primes-tail [3]))))

(defrecord NaivePrimer [ps]
  Primer
  (factor [this n]
    (let [first-factor
          (->> ps
               (take-while #(<= (* % %) n))
               (filter #(zero? (rem n %)))
               (first))]
      (if first-factor
        (cons first-factor (factor this (/ n first-factor)))
        [n])))
  (primes [_] ps)
  (prime? [_ n]
    (->> ps
         (take-while #(<= (* % %) n))
         (filter #(zero? (rem n %)))
         (empty?))))

(defn naive-primer [] (->NaivePrimer (naive-primes-seq)))


;; I think this might take up an awful amount of memory. Also it does
;; an integer overflow pretty quick. Ergh.
(defn factorizations
  "Returns a lazy sequence of [n [xs ...]]"
  []
  (letfn [(map->list [factor-map]
            (for [[k v] (sort-by key factor-map)
                  x (repeat v k)]
              x))

          (map->num [factor-map] (apply * (map->list factor-map)))

          (mapcomp [a b]
            (compare (map->num a) (map->num b)))
          ;; factors is a map from numbers to their factorization-maps.
          ;; things is a map from sets of primes to sorted-sets of
          ;; factorization maps that are upcoming.
          ;;
          ;; Not sure we're actually using the values of things here?
          ;; Why do we need it? Can we use it to save space in the factors
          ;; map?
          (step [factors things next-num]
            (lazy-seq
             (if-let [factorization (factors next-num)]
               ;; composite
               (cons [next-num (map->list factorization)]
                     (let [already (things (set (keys factorization)))

                           new-factorizations
                           (for [k (keys factorization)
                                 :let [new-factorization
                                       (update-in factorization [k] inc)]
                                 :when (not (contains? already new-factorization))]
                             new-factorization)

                           new-factors
                           (into (dissoc factors next-num)
                                 (for [new-factorization new-factorizations]
                                   [(map->num new-factorization)
                                    new-factorization]))

                           new-already
                           (into (disj already factorization)
                                 new-factorizations)

                           new-things (assoc things (set (keys factorization)) new-already)]
                       (step new-factors new-things (inc next-num))))
               ;; new prime
               (cons [next-num [next-num]]
                     (let [new-primesets
                           (for [[primeset] things]
                             (conj primeset next-num))

                           new-things
                           (into (assoc things #{next-num} (sorted-set-by mapcomp {next-num 2}))
                                 (for [primeset new-primesets]
                                   [primeset
                                    (sorted-set-by mapcomp (zipmap primeset (repeat 1)))]))

                           new-factors
                           (into (assoc factors (* next-num next-num) {next-num 2})
                                 (for [primeset new-primesets]
                                   [(apply * primeset)
                                    (zipmap primeset (repeat 1))]))]
                       (step new-factors new-things (inc next-num)))))))]
    (cons [1 []] (step {} {} 2))))