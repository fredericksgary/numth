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