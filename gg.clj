(ns gg
  (:gen-class))

;;;               ;;;
;;;coding practice;;;
;;;               ;;;

(require '[clojure.string    :as str])
(require '[clojure.java.io   :as io])
(require '[clojure.math.numeric-tower :as math])
(require '[clojure.math.combinatorics :as comb])


;;; utility functions useful to just about any program:START ;;;
(defn drop-nth [n coll]
   (keep-indexed #(if (not= %1 n) %2) coll))

(defn permutations_w_rep [s]
  (into (comb/permutations s) (map (partial repeat (count s)) s)))

(defn swapij [v i j] (assoc v, j (v i), i (v j)))

;;; utility functions useful to just about any program:CLOSE ;;;

;;; from Anna Sobolewska's email:START

(defn game [n l]
  (reduce bit-xor
          (reduce concat
                  (let [bases (range n (+ n l))]
                    (map (fn [base]
                           (let [step (dec l)
                                 end  (+ base (* step (- base n)))]
                             (range base (inc end) step)))
                         bases)))))

;;; from Anna Sobolewska's email:CLOSE


;;; ds&alg practice:START ;;;

(def A1 [[1  2  3  4 ]
         [5  6  7  8 ]
         [9  10 11 12]
         [13 14 15 16]])

(def B1 [[1  2  3  4 ]
         [5  6  7  8 ]
         [9  10 11 12]
         [13 14 15 16]])

(defn mm [A B] ;pg77
  (let [n   (count (first A))
        rec (fn rec [n i j]
              (if (= 1 n)
                [(* (get (get A i) j) (get (get B i) j))]
                222
                
              ))
        
       ]
    (rec 1 1 1)
))

(defn maximum_subarray_linear [c]
  (let [max_subarrs (loop [i 0, l 0, r 0, sum 0, acc {}]
                      (if (= i (count c))
                        (if (< l i) (conj acc [l r]) acc)
                        (let [sumj (+ sum (nth c i))]  ;(println "i:" i ",l:" l ",r:" r ",sum:" sum ",sumj:" sumj ",acc:" acc)
                          (if (<= sumj 0)              ;note: < works too, but use <= to cut parts of the max subarr that amount to 0, < if you want to include them (resulting in longer max subarrs)
                            (recur (inc i), (inc i), (inc i), 0, (if (< l i) (conj acc [l r]) acc))
                            (if (< sum sumj)
                              (recur (inc i), l, i, sumj, acc)
                              (recur (inc i), l, r, sum, acc))))))
        max_subarrs (map #(vector (reduce + (subvec c (first %) (inc (second %)))) %) max_subarrs)]
    max_subarrs)) ;just choose the max_subarr with maximum sum, may be more than 1, so you can further choose by earliest, or remove those of only 1 element, etc, it's more flexible than the single result in the recursive version below

(defn maximum_subarray  ;this one's recursive, solution above is has better complexity (n)
  ([c]
    (maximum_subarray c 0 (dec (count c))))
  ([c i j]
    (if (zero? (- j i))
      [i j]
      (let [mi  (+ i (int (/ (- j i) 2)))
            lm  (maximum_subarray c i mi)
            rm  (maximum_subarray c (inc mi) j)
            mm  (let [mmli (loop [li     (dec mi)
                                  ri     (inc mi)
                                  maxli  mi 
                                  maxsum (reduce + (subvec c maxli (inc ri)))]
                             (if (< li i)
                               maxli
                               (let [sum (reduce + (subvec c li (inc ri)))]
                                 (if (> sum maxsum)
                                   (recur (dec li), ri, li,    sum   )
                                   (recur (dec li), ri, maxli, maxsum)))))
                      mmri (loop [li     mi
                                  ri     (inc (inc mi))
                                  maxri  (dec ri) 
                                  maxsum (reduce + (subvec c li (inc maxri)))]
                             (if (> ri j)
                               maxri
                               (let [sum (reduce + (subvec c li (inc ri)))]
                                 (if (> sum maxsum)
                                   (recur li, (inc ri), ri,    sum   )
                                   (recur li, (inc ri), maxri, maxsum)))))]
                  [mmli mmri])
             ls  (reduce + (subvec c (first lm) (inc (second lm))))
             rs  (reduce + (subvec c (first rm) (inc (second rm))))
             ms  (reduce + (subvec c (first mm) (inc (second mm))))]
      (let [mxs (max ls rs ms)]
        (cond (= mxs ls) lm (= mxs rs) rm (= mxs ms) mm))))))



;;; ds&alg practice:CLOSE ;;;

;;; rosettacode:START ;;;

;https://www.youtube.com/watch?v=XKu_SEDAykw
;https://www.youtube.com/watch?v=DFG-XuyPYUQ
(defn insertion_sort [s]
  (reduce (fn [acc e]
            (let [[lte gte] (split-with (partial > e) acc)]
              (concat lte [e] gte)))
          []
          s))


(defn accumulator_factory [n]
  (let [acc (atom n)]
    (fn [m] (swap! acc + m))))

;(defn spiral_matrix [n]

;Harshad or Niven series
;(defn harshads [n]

(defn next_harshad [n]
  (letfn [(harshad? [n]
            (let [digits (map #(Integer/parseInt (str %)) (str n))]
              (zero? (mod n (reduce + digits)))))]
    (loop [n (inc n)] (if (harshad? n) n (recur (inc n))))))

(def harshads (lazy-cat '(1 2) (map next_harshad (rest harshads))))

;;; rosettacode:CLOSE ;;;


;;; macro practice:START ;;;

(defmacro make_adder [x]
  `(fn [y#] (+ ~x y#)))

(defmacro safe_math_expr [exp]
 `(try
    ~exp
    (catch ArithmeticException e# false)))

(defmacro our_and
  ([] true)
  ([x] x)
  ([h & t]
   `(let [curr# ~h]
      (if curr#
        (our_and ~@t)
        curr#))))

(defmacro repeater [n codeline]
  `(do ~@(repeat n codeline)))

(defmacro autogensym [codeline]
 `(let [val# ~codeline] val#))

(defmacro inspect_caller_locals []
  (->>
    (keys &env)
    (map (fn [k] [`'~k k]))
    (into {})))

;;; macro practice:CLOSE ;;;

;;; 4clojure:START ;;;

(defn p43 [c n]
  (for [i (range n)] (take-nth n (drop i c))))

(defn p43-first-try-should-use-take-nth [c n]
  (let [len   (count c)
        cong  (fn [k] (take (/ len n) (iterate #(+ % n) k)))
        congs (map cong (range n))]
    (map #(map (partial get (vec c)) %) congs)))

(defn p44 [n c]
  (let [len (count c)]
    (take len (drop (mod n len) (cycle c)))))

;;;p45 easy

(defn p46 [f]
  (fn [& args] (apply f (reverse args))))

;;;p47 easy
;;;p48 easy

(defn p49 [s]
  (fn [n s] (cons (take n s) (cons (drop n s) nil))))

(defn p50 [c]
  (vals (group-by type c)))

;;;p51 question
;;;p52 question

  ;(map #(nth c %)

;(defn p53 is below)
(defn longest-subseq [c]
  (letfn [(longest_subseq [pred c]
            (let [idxs (reduce #(if (> (count %2) (count %1)) %2 %1)
                               (conj (filter #(some identity %)
                                             (partition-by nil? (map #(if (pred (first %1) (second %1)) %2 nil)
                                                                     (partition 2 1 c)
                                                                     (range (count c)))))
                                     []))]
              (if (not (empty? idxs))
                (map (partial nth c) (concat idxs [(inc (last idxs))])))))]
    (longest_subseq < c)))

(defn longest-subseq-non-functional-style [c]
  (let [c (concat c (if (empty? c) [] [(dec (last c))]))]
    (loop [i nil, j 0, wi nil, wj nil]    ;loop on j (i marks the start of the latest subseq) ;(println "j:" j ",i:" i ",wi:" wi ",wj:" wj)
      (if (< j (dec (count c)))
        (if (< (nth c j) (nth c (inc j)))
          (if (nil? i)
            (recur j, (inc j), wi, wj)
            (recur i, (inc j), wi, wj))
          (if (nil? i)
            (recur i, (inc j), wi, wj)
            (if (or (nil? wi) (> (- j i) (- wj wi)))
              (recur nil, (inc j), i, j)
              (recur nil, (inc j), wi, wj))))
        (if wi (take (inc (- wj wi)) (drop wi c)) [])))))

(defn p53 [s]
  (let [max-by (fn [f & args] ;would use max-key, but max-key returns the last max element when more than 1 of them, and we want the first one
                 (first (reduce (fn [[winner winner_score] arg]
                                  (let [arg_score (f arg)]
                                    (if (> arg_score winner_score)
                                      [arg arg_score]
                                      [winner winner_score])))
                                (let [e (first args)] [e (f e)])
                                (rest args))))
        mxs (filter #(not (nil? (first %)))
                    (partition-by type
                                  (map-indexed #(when %2 %1)
                                               (map (fn [a b] (< a b))
                                                    s
                                                    (conj (vec (drop 1 s)) -1)))))]  
   (if (not (empty? mxs))
     (let [mx (apply max-by count mxs)] 
       (take (+ 2 (- (last mx) (first mx)))
             (drop (first mx) s)))
     [])))

(defn p54 [n c]
  (loop [i 1, c c, acc_rslt [], acc_partition []]
    (if-let [e (first c)]
      (if (zero? (mod i n))
        (recur (inc i), (rest c), (conj acc_rslt (conj acc_partition e)), [])
        (recur (inc i), (rest c), acc_rslt, (conj acc_partition e)))
      acc_rslt)))

(defn p54 [n s]
  (let [f (fn [[acc s]] [(concat acc [(take n s)]), (drop n s)])]
    (first (nth (iterate f ['() s])
                (int (/ (count s) n))))))

(defn p55 [c]
  (loop [c c, acc {}]
    (if-let [e (first c)]
      (recur (rest c), (if (acc e) (update acc e inc) (conj acc [e 1])))
      acc)))

(defn p55 [s]
  (reduce #(if (contains? %1 %2)
             (update %1 %2 inc)
             (assoc %1 %2 1))
          {}
          s))

(defn p55 [s]
  (apply merge-with + (map (fn [e] {e 1})
                           s)))

(defn p56 [s]
  (reverse (second (reduce (fn [[acc_e acc_s] e]
                             (if (acc_e e)
                               [acc_e acc_s]
                               [(conj acc_e e) (conj acc_s e)]))
                           [#{} '()]
                           s))))

(defn p56 [s]
  (first (reduce (fn [[acc_seq acc_set]  e] 
                   (if (contains? acc_set e)
                     [acc_seq acc_set]
                     [(conj acc_seq e) (conj acc_set e)]))
                 [[] #{}]
                 s)))

;;;p57 elementary

(defn p58 [& fs]
  (fn [& args] (reduce #(%2 %1) (apply (last fs) args) (reverse (butlast fs)))))

(defn p59 [& fs]
  (fn [& args]
    (map #(apply % args) fs)))

(defn p60
  ([f s]
    (p60 f, (first s), (rest s)))
  ([f i [h & t :as s]]
    (lazy-seq (cons i (when (seq s) (p60 f, (f i h), t))))))

(defn p61 [ks vs]
  (apply hash-map (interleave ks vs)))

(defn p62 [f x]
  (cons x (lazy-seq (p62 f (f x)))))

(defn p63 [f c]
  (apply merge-with into (for [e c] {(f e) [e]})))

;;;p64 elementary

(defn p65 [c]
  (if (= (get (conj c [:a "a"]) :a) "a")
    :map
    (if (= (get (conj c :a) :a) :a)
      :set
      (if (empty? c)
        (if (= [1 2] (conj (conj c 1) 2))
          :vector
          :list)
        (let [nh (if (= 1 (first c)) 2 1)]
          (if (= nh (first (conj c nh)))
            :list
            :vector))))))

(defn p66 [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn next_prime [n]
  (letfn [(prime? [n]
            (let [limit (Math/ceil (Math/sqrt n))]
              (loop [k 2]
                (cond (> k limit)        true
                      (zero? (mod n k))  false
                      :else              (recur (inc k))))))]
    (loop [n (inc n)] (if (prime? n) n (recur (inc n))))))

(def primes (lazy-cat '(2 3) (map next_prime (rest primes))))
(def nonnegints (lazy-cat '(0 1) (map inc (rest nonnegints)))) ;why does this work? rest is lazy, see: http://stackoverflow.com/a/4288542 

(defn p67 [n]
  (take n primes))

;;;p68 elementary

(defn p69 [f m & maps]
  (letfn [(merge_maps [m1 m2] (reduce (fn [m [k v]]
                                        (if (contains? m k)
                                          (update m k f v) 
                                          (assoc m k v)))
                                      m1
                                      (map identity m2)))]
    (reduce merge_maps m maps)))

(defn p69 [f & ms]
  (letfn [(merge_maps [a b]
            (letfn [(upd_map [m [k v]]
                      (if (contains? m k)
                        (update m k f v)
                        (assoc m k v)))]
            (reduce upd_map a (map identity b))))]
    (reduce merge_maps ms)))

(defn p70 [s] (sort-by str/lower-case (re-seq #"[a-zA-Z]+" s)))

;;;p71 elementary
;;;p72 elementary

(def p73b0
[[:e :e :e]
 [:e :e :e]
 [:e :e :e]])

(def p73b1
[[:x :e :o]
 [:x :e :e]
 [:x :e :o]])

(defn p73 [b]
  (let [same? (fn [s] (if (= 1 (count (partition-by identity s))) (first s)))
        diag1 [(nth (nth b 0) 0) (nth (nth b 1) 1) (nth (nth b 2) 2)]
        diag2 [(nth (nth b 0) 2) (nth (nth b 1) 1) (nth (nth b 2) 0)]]
    (if-let [h (some #{:o :x} (map same? b))]
      h
      (if-let [v (some #{:o :x} (map same? (apply map vector b)))]
        v
        (if-let [d (#{:o :x} (same? diag1))]
          d
          (when-let [d (#{:o :x} (same? diag2))]
            d))))))

(defn p74 [s]
  (let [nums     (map #(Integer/parseInt %) (str/split s #","))
        psquare? (fn [n] (let [sqrt (Math/sqrt n)] (= sqrt (Math/floor sqrt))))
        psquares (filter psquare? nums)]
    (apply str (interpose "," psquares))))

(defn p75 [n]
  (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
    (count (filter #(= 1 (gcd n %)) (rest (range (inc n)))))))

;;;p76 question

(defn p77 [c]
  (filter #(> (count %) 1) (map (comp set val) (group-by frequencies c))))

(defn p77--first-try [c]
  (let [rslt (loop [acc {}, c c]
               (if-let [s (first c)]
                 (let [m (frequencies s)]
                   (if (acc m)
                     (recur (update acc m conj s) (rest c))
                     (recur (conj acc [m #{s}]) (rest c))))
                 acc))]
    (set (filter #(> (count %) 1) (vals rslt)))))

(defn p78 [f & args]
  (loop [rslt (apply f args)]
    (if (fn? rslt)
      (recur (rslt))
      rslt)))

(def p79t1 '([1]
            [2 4]
           [5 1 4]
          [2 3 4 5]))

(defn p79 [t]
  (let [f (fn [v2 v1]
            (map #(+ %1 (min (first %2) (second %2)))
                 v1
                 (partition 2 1 v2)))]
    (first (reduce f (reverse t)))))

(defn p80 [n]
  (= n (reduce + (filter #(zero? (rem n %)) (range 1 n)))))

(defn p81 [a b]
  (set (for [e a :when (b e)] e)))

;(defn p82 [acc ws]
;  (let [char_to_dot    (fn [s i] (str (subs s 0 i) "." (subs s (inc i) (count s))))
;        ins_dot        (fn [s i] (str (subs s 0 i) "." (subs s i (count s))))
;        del_char       (fn [s i] (str (subs s 0 i) (subs s (inc i) (count s))))
;        replace_match? (fn [w1 w2]
;                         (if (= (count w1) (count w2))
;                           (loop [i 0]
;                             (if (= i (count w1))
;                               false
;                               (if (re-matches (re-pattern (char_to_dot w1 i)) w2)
;                                 true
;                                 (recur (inc i)))))))
;        insert_match?  (fn [w1 w2]
;                         (if (#{1 -1} (- (count w1) (count w2)))
;                           (let [wa (if (< (count w1) (count w2)) w1 w2)
;                                 wb (if (< (count w1) (count w2)) w2 w1)]
;                             (loop [i 0]
;                               (if (= i (inc (count wa)))
;                                 false
;                                 (if (re-matches (re-pattern (ins_dot wa i)) wb)
;                                   true
;                                   (recur (inc i))))))))
;        delete_match?  (fn [w1 w2]
;                         (if (#{1 -1} (- (count w1) (count w2)))
;                           (let [wa (if (< (count w1) (count w2)) w1 w2)
;                                 wb (if (< (count w1) (count w2)) w2 w1)]
;                             (loop [i 0]
;                               (if (= i (count wb))
;                                 false
;                                 (if (= wa (del_char wb i))
;                                   true
;                                   (recur (inc i))))))))
;        match?         (fn [w1 w2]
;                         (or (replace_match? w1 w2) (insert_match? w1 w2) (delete_match? w1 w2)))
;        
;      ]
;    (if (empty? ws)
;      true
;      (let [acc    (if (empty? acc) [(first ws)] acc)
;            head   (first acc)
;            tail   (last acc)
;            wcs    (rest ws)]
;        (let [wms (filter identity (for [wc wcs] (if (match? w wc) wc)))]
;          
;          ;(println (apply str "wms for " w ": " wms))
;)))))
      
(defn p83 [& bs]
  (boolean (and (some identity bs) (some (complement identity) bs))))

(defn p84 [rel]
  (letfn [(compose_rel_on_itself [rel] (clojure.set/union
            rel
            (set (apply concat (filter (complement empty?)
                                         (for [bp1 rel] (filter (complement empty?) (for [bp2 rel :when (= (second bp1) (first bp2))] [(first bp1) (second bp2)]))))))))]
    (loop [rel rel]
      (let [rel_compo (compose_rel_on_itself rel)]
        (if (= (count rel) (count rel_compo))
          rel
          (recur rel_compo))))))

(defn p84 [r]
  (let [tr1e (fn [r e]                                     ;transition 1 element
               (set (map #(vector (first e) (second %))
                         (get (group-by first r) (second e)))))
        tr1r (fn [r]                                       ;transition relation once
               (reduce #(clojure.set/union %1 (tr1e r %2))
                       #{}
                       r))]
    (loop [r r, tr_rslt (tr1r r)]
      (let [s (clojure.set/union r tr_rslt)]
        (if (= r s)
          s
          (recur s, (tr1r s)))))))

;note:4clojure site complains that conj doesn't take 1 arg, so use this instead there:
;              (reduce #(let [x (tr1e r %2)]
;                         (if (empty? x) %1 (apply conj %1 x)))

(defn p85-first-try-recursive [c]
  (if (empty? c)
    #{#{}}
    (let [x (first c), c (rest c), pset (p85-first-try-recursive c)]
      (if x
        (clojure.set/union pset (map #(conj %1 x) pset))
        pset))))

(defn p85 [s]
  (let [cnt_s    (count s)
        cnt_ps   (dec (math/expt 2 cnt_s))
        bstr_len (count (Integer/toString cnt_ps 2))
        s        (zipmap (range cnt_s) (seq s))
        bit_str  (fn [n]
                   (let [sbs (Integer/toString n 2)]  ;sbs=significant bits
                     (str (apply str (repeat (- bstr_len (count sbs)) \0))
                          sbs)))
        idxs_of_ones_in_bin_str
                 (fn [n]  
                   (keep-indexed #(when (= %2 \1) %1)
                                 (bit_str n)))]
    (loop [i 1, acc #{#{}}]
      (if (> i cnt_ps)
        acc
        (recur (inc i),
               (conj acc (set (vals (select-keys s
                                                 (idxs_of_ones_in_bin_str i))))))))))

(defn p85 [s]
  (reduce #(into %1 (for [ss %1] (conj ss %2))) ;ss=subset
          #{#{}}
          s))

(defn powerset [c] (p85 c))

(defn p86 [n]
  (loop [n n, acc #{}]
    (cond (contains? acc 1) true
          (contains? acc n) false
          :else             (let [next_n (reduce + 
                                                 (map #(* % %)
                                                      (map #(Integer/parseInt (str %))
                                                           (str n))))]
                              (recur next_n, (conj acc n))))))

(defn p86 [n]
  (letfn [(nxt [n] (reduce + (map #(* % %)
                                  (map #(Integer/parseInt (str %))
                                       (str n)))))]
    (loop [acc #{n}, n (nxt n)]
      (cond (= 1 n)            true
            (contains? acc n)  false
            :else              (recur (conj acc n), (nxt n))))))

(defn p88 [a b]
  (set (for [e (clojure.set/union a b) :when (p83 (a e) (b e))] e)))

(defn p91 [g]  ;a:adjacent, g:graph, e:edge, v:vertex, es,vs:edges,vertices, o:outer (last added vs to our acc), acc:connected vertices found so far
  (let [get_aes (fn [g v] (filter #(or (= v (first %)) (= v (second %))) g))
        vs      (set (distinct (apply concat g)))]
    (loop [acc #{}, ovs #{(first vs)}]
      (if (= vs acc)
        true
        (if (empty? ovs)
          false
          (let [ovs_avs  (set (flatten (mapcat #(get_aes g %) ovs)))
                nxt_ovs (reduce clojure.set/difference [ovs_avs ovs acc])]
            (recur (apply conj acc ovs), nxt_ovs)))))))

(defn p92 [s]
  (let [numerals {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
        nums     (partition 2 1 (concat (map numerals s) [0]))]
    (reduce (fn [sum [a b]]
              ((if (< a b) - +) sum a))
            0
            nums)))

(defn p92-first-try [s]
  (letfn [(getval ([l]
                    (case l \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000))
                  ([l n]
                    (let [a (getval l) b (getval n)]
                      (if (< a b) (- a) a))))]
    (if (= 1 (count s))
      (getval (first s))
      (reduce + (map getval s (take (count s) (str (apply str (drop 1 s)) (last s))))))))


(defn p93 [tree]
  (if (every? (complement sequential?) tree)
    [tree]
    (mapcat p93 tree)))

(def gol1 ["      "  
           " ##   "
           " ##   "
           "   ## "
           "   ## "
           "      "])

(defn p94 [board]                                                        ;looks good publish later
  (let [board         (let [empty_row (vec (repeat (+ 2 (count (first board))) 0))]
                        (conj (into [empty_row]                          ;digitize board, and add dummy border of zeros on all 4 sides of the rectangle
                                    (mapv (fn [row] (into [0]
                                                          (conj (mapv #(if (= \# %) 1 0) row)
                                                                0)))
                                           board))
                              empty_row))
        partition_row (fn [row] (partition 3 1 row))
        tops          (mapv partition_row (drop-last 2 board))
        board         (mapv partition_row (drop 1 (drop-last board)))
        bottoms       (mapv partition_row (drop 2 board))
        sum_neighs    (fn [tp mp lp] (- (reduce + (concat tp mp lp)) (nth mp 1)))
        new_val       (fn [tp mp lp] (let [neigh_sum (sum_neighs tp mp lp)]
                                       (if (zero? (nth mp 1))
                                         (if (= 3 neigh_sum)                      \# \space)
                                         (if (or (< neigh_sum 2) (> neigh_sum 3)) \space \#))))]
    (map #(apply str %)
         (map (fn [tpr mpr lpr] (map new_val tpr mpr lpr))                ;t=top,m=middle,l=low,p=partitons,r=row
              tops board bottoms))))

(defn p94 [board]                ;r=row, c=col, v=value of an row,col     ;looks good publish later
  (let [w  (count (first board)) ;nb=neighbor, cnt=count, cur=current
        h  (count board)
        nb_rcs  (fn [r c]
                  (filter (fn [[r c]]
                            (and (>= r 0) (>= c 0) (< r h) (< c w)))
                          (let [op_permuts (clojure.set/difference
                                             (set (comb/selections #{inc dec identity} 2))
                                             #{[identity identity]})]
                          (map (fn [[f1 f2] [r c]] [(f1 r) (f2 c)])
                               op_permuts
                               (repeat (count op_permuts) [r c])))))
        cur_rcv  (fn [r c & {:keys [as_int] :or {as_int false}}]
                   (let [v (nth (nth board r) c)]
                     (if as_int
                       (if (= \# v) 1 0)
                       v)))
        new_rcv  (fn [r c]
                   (let [nb_cnt (reduce +
                                        (map #(cur_rcv (first %) (second %) :as_int true)
                                             (nb_rcs r c)))]
                     (if (= \# (cur_rcv r c))
                       (if (or (< nb_cnt 2) (> nb_cnt 3)) \space \#)
                       (if (= nb_cnt 3) \# \space))))
       ]
    (new_rcv 5 5))) ;now just map over the board and format result as a board

(defn p89 [gg] ;todo, this is not a solution yet, implement Eulerian circuit test only ;todo later: get one such actual ciruit, to know if it has such a path, see Eulerian circuit theorem: https://www.cs.sfu.ca/~ggbaker/zju/math/euler-ham.html
  (let [[g ec]  (loop [g #{}, ec {}, gg gg]  ;g:graph, ec:edge count, e.g. multiedge is > 1, otherwise 1
                  (if-let [e (first gg)]
                    (if (or (g e) (g (reverse e)))
                      (recur g,          (update ec (set e) inc), (rest gg))
                      (recur (conj g e), (conj ec [(set e) 1]),   (rest gg)))
                    [g ec]))
        vd      (frequencies (flatten gg)) ;note: might want to use 'mapcat identity' instead of flatten, in case the values were collections, e.g. [[[1] [2]], [[2] [3]]]
       ]
    (and (p91 g) (every? #{2} (vals vd)))))

(def p89g4 [[1 2] [2 3] [3 4] [4 1]])
(def p89g5 [[:a :b] [:a :c] [:c :b] [:a :e]
            [:b :e] [:a :d] [:b :d] [:c :e]
            [:d :e] [:c :f] [:d :f]])
(def p89g6 [[1 2] [2 3] [2 4] [2 5]])

(defn connected? [es]
  (let [ns     (distinct (apply concat es))
        ns_cnt (count ns)
        ngb    (fn [n e]
                 (cond (= n (first e))  (second e)
                       (= n (second e)) (first e)
                       :else            nil))
        ngbs   (fn [n]
                 (let [rslt (filter some? (map (partial ngb n) es))]
                   (when (not (empty? rslt)) rslt)))]
    (loop [vns #{(first ns)}, reached_ns #{(first ns)}] ;vns=visited nodes, reached means the latest visited in the current iteration
      (cond (= ns_cnt (count vns))
              true
            (empty? reached_ns)
              false
            :else
              (let [vns_ngbs (set (reduce #(clojure.set/union %1 (ngbs %2))
                                           #{}
                                           reached_ns))
                    nxt_vns  (clojure.set/difference vns_ngbs vns)
                   ]
                (recur (clojure.set/union vns nxt_vns),
                       nxt_vns))))))

(defn p91 [es] (connected? (vec es)))
















(def p91g4 #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4] [3 4]})

(defn p91 [g]
  (let [ns_ (set (distinct (flatten (vec g))))

       ]
    ns_))
        















(defn p89 [es] ;see Eulerian circuits and paths ;TODO also need to check graph connectedness, note that SegFaultAX's solution is wrong, as it doesn't test connectedness, but passes the tests since it discards loops
  (and (connected? es)
       (some? (#{0 2} (count (filter odd? 
                (map count (vals (group-by identity
                  (apply concat (vals 
                    (group-by identity (apply concat es)))))))))))))

(defn p95 [s]
  (or (nil? s)
      (and (sequential? s)
           (= 3 (count s))
           (p95 (nth s 1))
           (p95 (nth s 2)))))

(defn p96
  ([t]
    (p96 (first (next t)) (second (next t))))
  ([tl tr]
    (if (or (nil? tl) (nil? tr))
      (= tl tr)
        (and (= (first tl) (first tr))
             (let [tll (first (next tl)), tlr (second (next tl)), trl (first  (next tr)), trr (second (next tr))]
               (p96 tll trr)
               (p96 tlr trl))))))

(defn p97 [n]
  (if (< n 2)
    [1]
    (loop [i 3, acc [[1] [1 1]]]
      (if (> i n)
        (last acc)
        (recur (inc i),
               (conj acc
                     (concat [1] (map #(apply + %) (partition 2 1 (last acc))) [1])))))))

(defn p98 [f d]
  (apply set (map set (vals (group-by f d)))))

(defn p99 [a b]
  (map #(Integer/parseInt (str %)) (str (* a b))))

(defn p100 [& args]  ;note: site's solution is wrong, try (p100 2 4 8 16 32) to see why
  (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
          (lcm [a b] (/ (* a b) (gcd a b)))]
    (reduce lcm args)))

;;;p101 TODO, need to understand Lehvenstein distance better

(defn p102 [s]
  (let [c (take-nth 2 (partition-by #(= \- %) s))]
    (reduce #(str %1 (str/capitalize (apply str %2)))
            (apply str (first c))
            (rest c))))

(defn p103 [k s]
  (cond (= 1 k)         (set (map (comp set vector) s))
        (= (count s) k) #{s}
        (> k (count s)) #{}
        :else           (let [e (first s) t (set (rest s))]
                          (clojure.set/union (set (map #(conj % e) (p103 (dec k) t)))
                                             (p103 k t)))))



(defn p104 [n]
  (let [numerals {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 
                  "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}]
    (loop [n n, [[l v] & nums :as all] (reverse (sort-by val numerals)), acc []]
      (cond (zero? n)  (apply str acc)
            (> v n)    (recur n, nums, acc)
            :else      (recur (- n v), all, (conj acc l))))))

(defn p105 [c]                                            ;looks good, publish later
  (reduce (fn [acc [[k] vals]] (conj acc [k vals]))
          (into {} (map vec (partition 2 (concat (interpose [] (filter keyword? c)) [[]])))) ;produces map of all keys present with empty vectors, e.g. {:a [] :b [], etc}
          (partition 2 (map #(if (and (> (count %) 1) (every? keyword? %)) [(last %)] %)     ;with 2, we intentionally drop the last empty keyword if present (empty keywords are already handled above)
                            (partition-by type c)))))

(defn p106 [a b]
  (let [step (juxt #(+ 2 %) #(* 2 %) #(if (even? %) (/ % 2) %))]
    (loop [i 1, acc #{a}]
      (if (contains? acc b)
        i
        (recur (inc i), (set (mapcat step acc)))))))

(defn p107 [n]
 (fn [x] (if (zero? n) 1 (last (take n (iterate #(* x %) x))))))

(defn p108 [& cs]
  (let [firsts  (for [c cs] (first c))
        largest (last (sort firsts))]
  (if (apply = firsts)
    largest
    (recur (map #(if (< %1 largest) (rest %2) %2)
                     firsts
                     cs)))))

(defn p108 [& seqs]
  (let [firsts  (map first seqs)
        largest (apply max firsts)]
    (if (apply = firsts)
      largest
      (recur (map #(if (< (first %) largest) (rest %) %)
                  seqs)))))

;;;p109 non-existent

(defn p110 [c]
  (letfn [(f [c] (mapcat #(vector (count %) (first %)) (partition-by identity c)))]
    (lazy-seq (cons (f c) (p110 (f c))))))

(defn p110 [p]
  (rest (iterate (fn [p]
                   (mapcat #(vector (count %) (first %))
                           (partition-by identity p)))
                 p)))
                   
  ;p111:START
(def cwpuzzle
["c _ _ _"
 "d _ # e"
 "r y _ _"]
)

(defn p111 [w p]
  (let [p                (map #(take-nth 2 %) (map #(replace {\space "" \_ \.} %) p))
        lines_to_regexes (fn [p]
                           (map #(apply str %)
                             (remove #(= \# (first %))
                                     (mapcat #(partition-by #{\#} %) p))))
        slots (concat (lines_to_regexes p) (lines_to_regexes (apply map str p)))]
    ((complement nil?) (some #(re-matches (re-pattern %) w) slots))))
  ;p111:CLOSE

;;;p112 TODO, below just makes a copy of the arg by consing recursively, does not solve the problem

(defn copy_seq_by_consing [s]
  (letfn [(f [[h & t]]
            (cond (nil? h)        nil
                  (sequential? h) (cons (f h) (f t))
                  :else           (cons h (f t))))]
    (if (empty? s) s (f s))))

;;;p113 TODO, weird, need to understand this first: https://clojuredocs.org/clojure.core/proxy

(defn p114 [n p [h & t :as s]]    ;looks good publish later
  (if (empty? s)
    nil
    (let [n (if (p h) (dec n) n)]
      (if (> n 0)
        (lazy-seq (cons h (p114 n p t)))))))

(defn p115 [n]
  (let [digits (map #(Integer/parseInt (str %)) (str n))
        len    (int (/ (count digits) 2))
        l      (take      len digits)
        r      (take-last len digits)]
    (= (apply + l) (apply + r))))

(defn p115 [n]
  (apply = (map (fn [h] (reduce + (map #(Integer/parseInt (str %)) h)))
                (let [s (str n), len (int (/ (count s) 2))]
                  [(take len s) (take-last len s)]))))

;;;p116 TODO seems derivative, but no reason you shouldn't do it

  ;p117:START
(defn digitize_grid [grid] (mapv (fn [s] (mapv #(case % \# 1 \M 6 \C 7 0) s)) grid))

(defn find_m [grid]
  (loop [grid grid, rownum 0]
    (if-let [row (first grid)]
      (let [colnum (.indexOf row 6)]
        (if (not= -1 colnum)
          [rownum colnum]
          (recur (rest grid), (inc rownum)))))))

(defn replace_in_grid [grid [rownum colnum] x]
      (vec (concat
        (take rownum grid)
        [(assoc (nth grid rownum) colnum x)]
        (drop (inc rownum) grid))))

(defn move_m [grid [rownum colnum] direction]
  (let [n (case direction    ;n for neighbor
        :u (and (> rownum 0)                          (nth (nth grid (dec rownum)) colnum))
        :d (and (< rownum (dec (count grid)))         (nth (nth grid (inc rownum)) colnum))
        :l (and (> colnum 0)                          (nth (nth grid rownum) (dec colnum)))
        :r (and (< colnum (dec (count (first grid)))) (nth (nth grid rownum) (inc colnum))))]
    (case n
      false nil
      1     nil
      7     777
      (replace_in_grid
        (replace_in_grid grid [rownum colnum] 1) ;mark current M coord with #, to avoid backtracking
        (case direction
          :u [(dec rownum) colnum]
          :d [(inc rownum) colnum]
          :l [rownum (dec colnum)]
          :r [rownum (inc colnum)])
        6))))
    
(defn p117 [grid]
  (loop [grids [grid]]
    (if (empty? grids)
      false
      (let [move_grids (vec (mapcat
                         (fn [grid]
                           (let [m_pos      (find_m grid)
                                 move_rslts (map #(move_m grid m_pos %) [:u :d :l :r])]
                               (filter (complement nil?) move_rslts)))
                         grids))]
        (if (not= -1 (.indexOf move_grids 777))
          true
          (recur move_grids))))))

(def p117grid (digitize_grid
      ["  #  ##"
       " #  M   "
       " ####  "
       " # C # "
       "       "]))
;;;p117:CLOSE

(defn p118 [f s]
  (lazy-seq
    (when-let [s (seq s)]
      (cons (f (first s)) (p118 f (rest s))))))

(defn p118-first-try [f [h & t :as s]] ;works, but while t is lazy, it's resolved thru next, not rest
  (lazy-seq                            ;so the first element of t is realized, and that's less lazy
    (if-not (empty? s)                 ;than map, so we do it as above instead
      (cons (f h) (p118 f t)))))       ;see: http://stackoverflow.com/questions/16726360/is-it-possible-to-do-destructured-head-tail-separation-of-lazy-sequences-in-cloj

(defn p119 [p b]
  (let [permuts (fn [l]
                  (loop [acc [], i 0]
                    (if (< i l)
                      (recur (apply conj acc (loop [acc [], j 0]
                                               (if (< j l)
                                                 (recur (conj acc [i j]), (inc j))
                                                 acc)))
                             (inc i))
                      acc)))
        l      (count (first b))
        gc     (fn [x y] (nth (nth b y) x))   ;get coordinate
        gl     (fn [x y xdf ydf xif yif]      ;get line, xdf: x decrease func, xif: x increase func
                 (letfn [(mf [xsf ysf]
                          (loop [acc [], x (xsf x), y (ysf y)]
                            (if (or (< x 0) (< y 0) (> x (dec l)) (> y (dec l)))
                              (reverse acc)
                              (recur (conj acc (gc x y)), (xsf x), (ysf y)))))]
                   (concat (mf xdf ydf) (mf xif yif))))
        lsfs   [[dec identity inc identity] [identity dec identity inc] ;line shift funcs, produce horizontal, vertical, and diagonals
                [dec dec inc inc]           [dec inc inc dec]]]
   (set (for [coord (permuts l)
              :when (and (= :e (apply gc (reverse coord)))  ;we reverse xy here and below because the problem wants it in "get-in" format
                    (let [neighborlines (map #(apply gl (second coord) (first coord) %) lsfs)
                          lineswin      (map #(and (= (dec l) (count %)) (every? #{p} %)) neighborlines)
                         ]
                      (some identity lineswin)))]
     coord))))

(defn p120 [c]
  (letfn [(f [n]
            (< n (reduce +
                         (map #(int (Math/pow (Integer/parseInt (str %)) 2))
                              (str n)))))]
    (reduce +
            (map #(if (f %) 1 0)
                 c))))

(defn p121 [form]
  (fn [values]
    (let [env (merge {'+ + '- - '* * '/ /} values)]
      ((fn eval- [f]
         (if (seq? f)
           (apply (get env (first f)) (map eval- (rest f)))
           (get env f f)))
       form))))

(defn p122 [s]
  (first (reduce (fn [[acc p] d] [(+ acc (* d p)), (* 2 p)])
                 [0 1]
                 (reverse (map #(Integer/parseInt (str %)) s)))))

;;;p123 non-existent

;;;p124 TODO

;;;p126 question

;;;p127:START
(def p127b0 [15 15 15 15 15])
(def p127b1 [1 3 7 15 31])
(def p127b2 [3 3])
(def p127b3 [7 3])
(def p127b4 [17 22 6 14 22])
(def p127b5 [18 7 14 14 6 3])
(def p127b6 [21 10 21 10])
(def p127b7 [0 31 0 31 0])

(defn makebitmap [s]
  (let [bitarr (fn [n] (mapv #(Integer/parseInt (str %)) (Integer/toString n 2)))
        len    (reduce (fn [maxn n]
                         (let [len (count (bitarr n))]
                           (if (> len maxn) len maxn)))
                       0
                       s)
        cs     (reduce (fn [acc n]
                         (let [r (bitarr n)]
                           (concat acc
                                   [(concat (repeat (- len (count r)) 0)
                                            r)])))
                       []
                       s)
        cs     (vec (map vec cs))
       ]
    cs))

(defn nxt_line [bm l d] ;bm=bitmap, l=line, d=direction (1-8, see paper for meaning)
  (let [w       (count (first bm))
        h       (count bm)
        l       (cond (= d 1) [[(dec (first (first  l)))  (dec (second (first l)))]
                               [(inc (first (second l)))  (dec (second (second l)))]]
                      (= d 2) [[(dec (first (first  l)))  (dec (second (first l)))]
                               [(dec (first (second l)))  (inc (second (second l)))]]
                      (= d 3) [[(dec (first (first  l)))  (inc (second (first l)))]
                               [(inc (first (second l)))  (inc (second (second l)))]]
                      (= d 4) [[(inc (first (first  l)))  (dec (second (first l)))]
                               [(inc (first (second l)))  (inc (second (second l)))]]
                      (= d 5) [[(dec (first (first  l)))  (dec (second (first l)))]
                               [     (first (second l))   (dec (second (second l)))]]
                      (= d 6) [[     (first (first  l))   (dec (second (first l)))]
                               [(inc (first (second l)))  (dec (second (second l)))]]
                      (= d 7) [[(dec (first (first  l)))  (inc (second (first l)))]
                               [     (first (second l))   (inc (second (second l)))]]
                      (= d 8) [[     (first (first  l))   (inc (second (first l)))]
                               [(inc (first (second l)))  (inc (second (second l)))]])
        trav_l  (fn []    ;trav_l=traverse line, returns map of line and length if line is all 1s, nil otherwise
                  (let [ld (if (= (first (first l)) (first (second l))) :h :v)
                        lt (if (= :v ld)
                             {:row    (first  (first l))
                              :col    (second (first l))
                              :len    (inc (- (first (second l))
                                              (first (first l))))
                              :incf   #(update % :row inc)
                              :end    (first (second l))
                              :endf   #(= (:row %) (:end %))
                             }
                             {:row    (first  (first l))
                              :col    (second (first l))
                              :len    (inc (- (second (second l))
                                              (second (first l))))
                              :incf   #(update % :col inc)
                              :end    (second (second l))
                              :endf   #(= (:col %) (:end %))
                             })]
                    (loop [lt lt]
                      (let [v (nth (nth bm (:row lt)) (:col lt))]   ;(println (str "v:" v))
                        (if (zero? v)
                          nil
                          (if ((:endf lt) lt)
                            {:l l, :n (:len lt)}
                            (recur ((:incf lt) lt))))))))]
    (if (or (some #(<= % -1) (flatten l))
            (some #(>= %  w) (map second l))
            (some #(>= %  h) (map first l)))
      nil
      (trav_l))))

(defn p127 [bm]
  (let [bm  (makebitmap bm)     ;bitmap
        ds  (range 1 9)         ;directions (see paper note for meaning)
        w   (count (first bm))  ;width of bitmap
        h   (count bm)]         ;height of bitmap
    (loop [row 0, col 0, mx 0]
      (if (= row h)
        mx
        (recur (if (= (inc col) w) (inc row) row)
               (if (= (inc col) w) 0 (inc col))
               (if (= 0 (nth (nth bm row) col))
                 mx
                 (let [rslt (apply max
                              (map (fn [d]
                                     (loop [l (nxt_line bm [[row col] [row col]] d)
                                            n 1]
                                       (if-not l
                                         n
                                         (recur (nxt_line bm (:l l) d)
                                                (+ n (:n l))))))
                                   ds))]
                    (if (> rslt mx) rslt mx))))))))
;;;p127:CLOSE 

(defn p128 [[a b]]
  (let [ranks (zipmap (map identity (str (apply str (range 2 10)) "TJQKA")) (range 0 (inc 12)))
        suits (zipmap (map identity "HQSDC") [:heart :queen :spade :diamond :club])]
    {:suit (suits a) :rank (ranks b)}))

;;;p129 non-existent


;;;p130:START
(def p130t0
  '(a
     (b
       (c
         (d)
         (e))
       (f
         (g)
         (h)))
     (i
       (j
         (k)
         (c))
       (m
         (n)
         (o)))))

(def p130t1
  ['a
     ['b
       ['c
         ['d]
         ['e]]
       ['f
         ['g]
         ['h]]]
     ['i
       ['j
         ['k]
         ['l]]
       ['m
         ['n]
         ['o]]]])

(defn treesearch [n t]  ;n=node, t=tree (format must be as p130's)
  (letfn [(f [t acc]
            (cond (nil? t)           nil
                  (= n (first t))    acc
                  (nil? (second t))  nil
                  :else              (map #(filter (fn [x] (if (sequential? x) (not (empty? x)) (identity x)))
                                                   (f %1 (conj acc %2)))
                                          (rest t)
                                          (range 1 (inc (count (rest t)))))))]
    (if (= n (first t))  ;p=path to newroot, chain of nths in tree to get to it
      []
      (let [rslt (filter (complement empty?) (p93 (f t [])))]
        (if (not (empty? rslt))
          (first rslt))))))

(defn subtreeforpath [t p] (reduce #(nth %1 %2) t p))

(defn drop-path [t p]
  (if (empty? t)
    nil
    (if (empty? p)
      t
      (let [i (first p)]
        (concat (take i t)
                (if (> (count p) 1) [(drop-path (nth t i) (rest p))])
                (drop (inc i) t))))))

(defn p130 [n t]
  (letfn [(f [t p]                          ;previous tree and path
            (let [np (rest p)               ;next path (to next root)
                  nt (drop-path t p)]       ;next tree (with prev root removed from next root's children)
              (if (empty? np)
                nt
                (concat (subtreeforpath nt np)
                        [(f nt np)]))))]
    (if-let [p (treesearch n t)]
      (concat (subtreeforpath t p)
              [(f t p)]))))
;;;p130:CLOSE

(defn p131 [& ss]
  (let [sums (map (fn [s]
                    (set (map #(reduce + %) (disj (powerset s) #{})))) 
                  ss)]
    (not (empty? (apply clojure.set/intersection sums)))))

(defn p132 [f v c]
  (mapcat (fn [[a b]]
            (if (and a b (f a b))
              [a v]
              [a]))
          (partition-all 2 1 c)))

;;;p133 non-existent

(defn p134 [k m]
  (and (contains? m k) (nil? (get m k))))

(defn p135 [& args]  ;looks good publish later
  (reduce (fn [acc [op arg]] (op acc arg))
          (first args)
          (partition 2 (rest args))))

;;;p136 non-existent

(defn p137 [n b]
  (if (zero? n)
    0
    (loop [acc [], n n]
      (if (zero? n)
        (reverse acc)
        (recur (conj acc (mod n b)), (int (/ n b)))))))

(defn p138 [a b]
  (letfn [(ndf [d] (cond (= :dr d)  :dl         ;next direction func
                         (= :dl d)  :ul
                         (= :ul d)  :ur
                         (= :ur d)  :dr))
          (rcf [d] (cond (= :dr d)  [dec inc]   ;funcs to generate next row col for provided direction
                         (= :dl d)  [dec dec]
                         (= :ul d)  [inc dec]
                         (= :ur d)  [inc inc]))
          (add_sides [nn d sl acc]
            (if (empty? nn)
              acc
              (loop [acc (let [[rf cf] (rcf d), lastn (last acc)]
                           (concat acc [{:r (rf (:r lastn))
                                         :c (cf (:c lastn)) 
                                         :n (first nn)
                                         :d (ndf (:d lastn))}]))
                     i       0
                     ss      0
                     nn      (rest nn)
                     d       (ndf d)]
                (if (= ss 2) 
                  [acc nn]
                  (let [[rf cf] (rcf d), prevrslt (last acc)]
                    (recur (concat acc [{:r (rf  (:r prevrslt))
                                         :c (cf  (:c prevrslt))
                                         :n (if-let [n (first nn)] n "*")
                                         :d d}])
                           (if (= i (dec sl)) 0 (inc i))
                           (if (= i (dec sl)) (inc ss) ss)
                           (rest nn)
                           (if (= i (dec sl)) (ndf (:d prevrslt)) d)))))))
          (squares [start limit]
            (loop [n start, acc [n]]
              (let [square (* n n)]
                (if (> square limit)
                  acc
                  (recur square, (conj acc square))))))
          (numsseq_to_str [s]
            (vec (map str (apply str (map str s)))))
          (coordsmap_to_strsseq [csm]
            ;;TODO
          )
         ]
    (let [nn (squares a b)]
      (let [nums_and_positions
             (loop [acc  [{:r 0, :c 0, :n (first nn), :d :dr}]
                    nn   (vec (rest nn))
                    sl   1]
                 (if (empty? nn)
                   acc
                   (let [[acc nn] (add_sides nn
                                             (:d (last acc))
                                             sl
                                             acc)]
                     (recur acc, nn, (inc sl)))))]
      nums_and_positions))  ;;TODO convert this to the final result, the strings vec
))

;;;p142 non-existent

;;;p140 TODO need to learn about Karnaugh maps
;;;p141 TODO

(defn p143 [a b]
  (reduce + (map * a b)))

(defn p144 [v & fs]
  (reductions #(%2 %1) v (cycle fs))) 

(defn p144-first-try [v & fs]
  (lazy-seq (cons v (apply p144 ((first fs) v) (concat (rest fs) [(first fs)])))))

(defn p144 [v & fs]
  (let [rec (fn rec [v cy]
              (let [nv ((first cy) v)]
                (lazy-seq (cons nv (rec nv (rest cy))))))]
    (lazy-seq (cons v (rec v (cycle fs))))))

;;;p145 question

(defn p146 [mm]
  (for [[kp m] mm]
    (for [[kc v] m]
      [kp kc v])))

(defn p146 [mm]
  (into {} (for [[kp m] mm [kc v] m] [[kp kc] v])))

(defn p147
  ([]
    (p147 [1]))
  ([c]
    (letfn [(nxt [c]
              (concat [(first c)]
                      (for [i (range 1 (count c))] (+' (nth c (dec i)) (nth c i)))
                      [(last c)]))]
      (lazy-seq (cons c (p147 (nxt c)))))))

(defn p148 [n a b] ;this solution is better, a and b need not be coprime, see lcm below, using exclusion principle ;looks good publish later
  (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
          (lcm [a b] (/ (* a b) (gcd a b)))
          (sum-one-to-k [k] (/ (*'  k (inc k)) 2))
          (f [x] (*' x (sum-one-to-k (quot (dec n) x))))]
    (- (+ (f a) (f b)) (f (lcm a b)))))

(defn p148-first-try-not-fast [n a b]
  (let [drop-every-nth (fn [coll n] (apply concat (partition-all (dec n) n coll)))
        rb (range 1 (inc (bigint (/ (dec n) b))))
        rb (drop-every-nth rb a)]
    (+ (reduce + (map #(* a %) (range 1 (inc (bigint (/ (dec n) a))))))
       (reduce + (map #(* b %) rb)))))

;;;p149 non-existent

(defn p150-first-try [n]
  (let [nextp (fn [n]
                (let [s   (str n)
                      len (count s)
                      sl  (subs s 0 (Math/ceil (/ len 2)))
                      tl  (inc (Integer/parseInt sl))
                      dk  (if (odd? len) 1 0)
                      dk  (if (every? #{\9} sl) (inc dk) dk)]
                  (Integer/parseInt (apply str tl
                                               (drop dk (reverse (str tl)))))))
        n (let [s   (str n)
                len (count s)]
            (if (odd? len)
              (nextp n)
              (let [sl (subs s 0 (Math/ceil (/ len 2)))
                    sr (subs s (Math/ceil (/ len 2)))
                    sm (Integer/parseInt (apply str sl (reverse sl)))
                   ]
                (if (> sm n)
                  sm
                  (nextp n)))))
       ]
  (iterate nextp n)))

;(defn p150 [n]


;;;p151 non-existent

;;;p152:START;;;
(defn update_range [v f from]
  (loop [i from, acc v]
    (if (< i (count v))
      (recur (inc i), (update acc i f))
      acc)))

(defn p152permuts [ss]
  (let [mxis  (map (comp dec count) ss)
        p_ixs (loop [acc [(vec (repeat (count ss) 0))]]
                (if (= (last acc) mxis)
                  acc
                  (recur (conj acc (loop [i (dec (count ss))]
                                     (if (= -1 i)
                                       (/ 1 0)
                                       (if (< (nth (last acc) i) (nth mxis i))
                                         (update_range (update (last acc) i inc) (constantly 0) (inc i))
                                         (recur (dec i)))))))))]
    p_ixs ;note: this is just the permutation indexes (within their rows, which are vectors) but we don't need anything else for p152, so leaving it as is for now
))

(def p152v1 [[1 2 3]
             [2 3 1 2 1]
             [3 1 2]])

(def p152v2 [[3 1 2]
             [1 2 3 1 3 4]
             [2 3 1 3]])

(def p152v4 '[[B D A C B]
              [D A B C A]
              [A B C A B]
              [B C A B C]
              [A D B C A]])

(def p152v8 [[8 6 7 3 2 5 1 4]
             [6 8 3 7]
             [7 3 8 6]
             [3 7 6 8 1 4 5 2]
                   [1 8 5 2 4]
                   [8 1 2 4 5]])


(defn shift_vv [vv w vv_shift]
  (mapv #(into (apply vector (repeat %2 nil))
               (into %1 (apply vector (repeat (- w (count %1) %2) nil))))
        vv
        vv_shift))

(defn trim_vv [vv]
  (let [lr  (map #(let [p (partition-by nil? (conj (apply conj [nil] %) nil))]
                   [(dec (count (first p))) (dec (count (nth p 2)))])
                 vv)
        li (apply max (map first lr))
        ri (apply max (map second lr))
        w  (count (first vv))]
    (if (> (- w (+ ri li)) 1)
      (mapv #(vec (drop-last ri (drop li %)))
            vv))))
                  
(defn latinsquare? [sq]
  (let [l      (count (first sq))
        elems  (set (flatten sq))]
    (and (not (contains? elems nil))
         (= l (count elems))
         (every? identity (mapv #(= l (count (distinct %))) sq))
         (every? identity (mapv #(= l (count (distinct %))) (apply mapv vector sq))))))

(defn subrectangle [r w h l x y]
  (let [r (vec (take l (drop y r)))
        r (mapv (comp vec (partial drop x)) r)
        r (mapv (comp vec (partial take l)) r)]
    r))

(defn p152 [vv]
  (let [w            (apply max (map count vv))
        h            (count vv)
        vv_maxshifts (map #(- w (count %)) vv)
        ps           (p152permuts (map #(vec (range (inc %))) vv_maxshifts))
        rs           (map (partial shift_vv vv w) ps)]
    (loop [rs rs, acc #{}]
      (if-let [r (first rs)]
        (let [acc  (loop [y 0, acc acc]
                     (if (> (- h y) 1)
                       (let [acc  (loop [x 0, acc acc]
                                    (if (> (- w x) 1)
                                      (let [ls     (drop 2 (range (inc (min (- w x) (- h y)))))
                                            latsqs (for [l ls
                                                         :let [sq (subrectangle r w h l x y)]
                                                         :when (latinsquare? sq)] sq)]
                                        (recur (inc x), (reduce conj acc latsqs)))
                                      acc))]
                         (recur (inc y), acc))
                       acc))]
          (recur (rest rs), acc))
        (apply conj (for [[k v] (group-by count acc)] {k (count v)}))))))

(defn p152_alternate_thing [vv] ;here counting all latinsquares occurrences, not only uniques as above
  (let [w            (apply max (map count vv))
        h            (count vv)
        vv_maxshifts (map #(- w (count %)) vv)
        ps           (p152permuts (map #(vec (range (inc %))) vv_maxshifts))
        rs           (map (partial shift_vv vv w) ps)
        acc_base     (apply hash-map (mapcat #(vector % 0) (drop 2 (range (inc h)))))]
    (loop [rs rs, acc acc_base]
      (if-let [r (first rs)]
       ;(do (println r)
        (let [accy (loop [y 0, accy acc_base]
                     (if (> (- h y) 1)
                       (let [accx (loop [x 0, accx acc_base]
                                   ;(println "x:" x ",y:" y)
                                    (if (> (- w x) 1)
                                      (let [ls    (drop 2 (range (inc (min (- w x) (- h y)))))
                                            latls (for [l ls
                                                        :let [sq (subrectangle r w h l x y)]
                                                        :when (latinsquare? sq)]
                                                    (do ;(println "l:" l ",w:" w ",h:" h ",r:" r)
                                                        ;(println sq)
                                                        l))
                                           ]
                                       ;(println latls)
                                        (recur (inc x), (reduce #(update %1 %2 inc) accx latls)))
                                      accx))]
                        ;(println "merge: " accy accx)
                         (recur (inc y), (merge-with + accy accx)))
                       accy))]
          (recur (rest rs), (merge-with + acc accy)))
       ;)
        acc))))

;;;p152:CLOSE;;;

(defn p153 [ss]                              ;functional but not efficient
  (= (reduce + (map count ss))
     (count (reduce clojure.set/union ss))))

(defn p153-efficient-but-ugly [ss]
  (loop [acc #{}, ss ss]
    (if-let [s (first ss)]
      (let [cnt_if_disj (+ (count acc) (count s))
            acc         (clojure.set/union acc s)]
        (if (= (count acc) cnt_if_disj)
          (recur acc, (rest ss))
          false))
      true)))

;;;p154 non-existent

;;;p155 non-existent

(defn p156 [v ks]
  (apply hash-map (interleave ks (repeat v))))

(defn p157 [c]  ;looks good publish later, note the line below to keep result type same as c's type
  (let [[jf mf] (if (= (type []) (type c)) [vector mapv] [list map])]
    (mf jf c (range (inc (count c))))))

(defn p158 [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))

;;;p161 question
;;;p162 question

;;;p164:START
(def p164d1
'{:states #{q0 q1 q2 q3}
              :alphabet #{a b c}
              :start q0
              :accepts #{q1 q2 q3}
              :transitions {q0 {a q1}
                            q1 {b q2}
                            q2 {c q3}}}
)

(def p164d1b
'{:states #{q0 q1 q2 q3}
              :alphabet #{a b c}
              :start q0
              :accepts #{q1 q2 q3 q4}
              :transitions {q0 {a q1 b q2}
                            q1 {c q3}
                            q2 {c q3}
                            q3 {d q4}
                           }}
)

(def p164d2
'{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
              :alphabet #{e h i l o y}
              :start q0
              :accepts #{q2 q4 q7}
              :transitions {q0 {h q1}
                            q1 {i q2, e q3}
                            q3 {l q5, y q4}
                            q5 {l q6}
                            q6 {o q7}}})


(defn p164-first-try
  ([dfa]
    (p164-first-try dfa [{:n (:start dfa)
                          :ts (get (:transitions dfa) (:start dfa))
                          :s ""}]))
  ([dfa curr_situation]  ;curr_situation meaning see ps below
    (letfn [(f [ps]               ;'pendings', states we 'are' at, with transitions that remain to be tried: [{:n node :ts transitions_left_to_try :s "string so far"}]
              (loop [ps ps]       ;(println "ps:" ps)
                (if (empty? ps)
                  nil
                  (let [n  (:n (first ps)), ts (:ts (first ps)), s (:s (first ps))
                        ps (if (empty? ts)
                             (vec (rest ps))
                             (loop [ts ts, ps ps]
                               (if-let [t (first ts)]
                                 (let [ps  (update-in (update-in ps
                                                                 [0 :ts]
                                                                 dissoc (first t))
                                                      [0 :s]
                                                      str (first t))
                                       ps  (if (empty? (:ts (first ps))) (vec (rest ps)) ps)]
                                    (recur (rest ts)
                                           (conj ps {:n  (second t)
                                                     :ts (get (:transitions dfa) (second t))
                                                     :s  (str s (first t))})))
                                 ps)))]
                    (if (get (:accepts dfa) n)
                      [s ps]
                      (recur ps))))))]
      (when-let [rslt (f curr_situation)]
        (lazy-seq
          (cons (first rslt)
                (p164-first-try dfa (second rslt))))))))

(defn p164 [dfa]
  (letfn [(run1 [[st1 acc]]
            (for [[tr st2] ((:transitions dfa) st1)]
              [st2 (str acc tr)]))
          (accepted [[st acc]]
            (when ((:accepts dfa) st) acc))]
    (mapcat #(keep accepted %)
            (take-while not-empty
                        (iterate #(mapcat run1 %)
                                 (run1 [(:start dfa) ""]))))))

(defn p164 [dfa]  ;s=state, t=transition, tc=transition char, ts=transitioned state after tc
  (letfn [(st1 [[acc s]] ;step accumulated path, of the form: ["acc'd str", state]
            (let [ts (get (get dfa :transitions) s)]
              (map (fn [[tc ts]] [(str acc tc), ts]) ts)))]
    (map first
         (filter (fn [[acc s]] (get (get dfa :accepts) s))
                 (mapcat identity
                         (take-while #(not (empty? %))
                                     (iterate #(mapcat st1 %)
                                              [["" (get dfa :start)]])))))))

;(defn p164 [] ;skeleton of the solution, note how we map over an iterate: we just keep composing lazy-seq producing functions over other lazy-seq producing functions
;  (let [pf (iterate (fn [[i v]]  ;pf=producing function
;                      [(inc i) (conj v (inc i))])
;                    [0 []])]
;    (map first pf)))

;;;p164:CLOSE

(defn p166 [op a b]
  (cond (op a b)              :lt
        (op b a)              :gt
        :else                 :eq))

(defn p168
  ([f]     (p168 f 0 0 -1 -1))
  ([f m n] (p168 f m n -1 -1))
  ([f m n s t]
    (letfn [(row [st ed]
              (when-not (zero? ed)
                (lazy-seq (cons st (row (inc st) (dec ed))))))]
      (map #(map (partial f %) (row n t))
           (row m s)))))

(defn p168-first-try
  ([f]     (p168 f 0 0 -1 -1))
  ([f m n] (p168 f m n -1 -1))
  ([f m n s t]
     (let [row (fn row [m n t]
                 (when-not (zero? t)
                   (lazy-seq (cons (f m n) (row m (inc n) (dec t))))))]
       (when-not (zero? s)
         (lazy-seq (cons (row m n t)
                         (p168 f (inc m) n (dec s) t)))))))

(defn p168
  ([f m n s t]
    (letfn [(rows [i j]
              (letfn [(row  [i j] (lazy-seq (cons (f i j) (row i (inc j)))))]
                (lazy-seq (cons (row i j) (rows (inc i) j)))))]
      (cond (nil? m)  (rows 0 0)
            (nil? s)  (rows m n)
            :else     (map (partial take t) (take s (rows m n))))))
  ([f m n] (p168 f m n nil nil))
  ([f]     (p168 f nil nil nil nil)))

(defn p171 [c]    ;rather good, publish later
  (if (empty? c)
    []
    (let [c  (distinct (sort c))
          c  (into (into [(- (first c) 2)] c) [(+ (last c) 2)])] ;we add before and after numbers guaranteed to trigger the if within the map's function below, to ensure the first and last elements are part of the result
      (partition 2 (butlast (rest (flatten 
        (remove #(= :dummy %) (map #(if (= 1 (- %2 %1)) :dummy (vector %1 %2)) c (rest c))))))))))

(defn p171 [s]
  (let [s        (sort s)
        mrkrs    (partition 2 1 [(count s)]
                   (conj (keep-indexed (fn [i [a b]]
                                           (when (not (#{0 1} (- b a))) (inc i)))
                                       (partition 2 1 s))
                         0))]
    (if (empty? s)
      s
      (map (fn [ss] [(first ss) (last ss)])
        (map (fn [ss]
               (let [i (first ss), j (last ss)]
                 (take (- j i) (drop i s))))
             mrkrs)))))

;;;p173 question

;cis  (for [[c i] (partition 2 (interleave s (range (inc (count s))))) :when (#{\{ \}} c)] c)

(defn p177-loop-more-efficient-less-pretty [s]
  (let [c   (vec (for [e s :when (#{\{ \}} e)] e))
        f   (fn [c]
              (loop [c c, i 0, acc []]
                (if-let [a (first c)]
                  (if-let [b (second c)]
                    (recur (rest c), (inc i), (if (= [\{ \}] [a b]) (conj acc i) acc))
                    acc)
                  acc)))
        r    (fn [c i] (vec (concat (subvec c 0 i) (subvec c (inc i)))))
        g    (fn [c idxs] (reduce #(r (r %1 (inc %2)) %2) c (reverse idxs)))
       ]
    (loop [pc c, c (g c (f c))]
      (cond (or (empty? pc) (empty? c))   true
            (= pc c)                      false
            :else                         (recur c (g c (f c)))))))

(defn p177 [s]
  (let [opncls {\( \) \[ \] \{ \}}
        oc_chars (set "()[]{}")]
  (empty?
    (reduce (fn [[h & t :as stack] c]
              (cond (= (opncls h) c)   t
                    (oc_chars c)       (conj stack c)
                    :else              stack))
            '()
            s))))

(defn p177-first-try [s]
  (let [opn_cs  #{\{ \[ \(}            ;open chars
        cls_cs  #{\) \] \}}            ;close chars
        opncls  {\{ \} \[ \] \( \)}    ;close char match for each open char
        flo     (fn [c from]           ;find last open char's index, backwards from from (inclusive)
                  (loop [i from]
                    (cond (< i 0)             nil
                          (opn_cs (nth c i))  i
                          :else               (recur (dec i)))))
        r       (fn [c i]              ;remove elem at index within vector
                  (vec (concat (subvec c 0 i) (subvec c (inc i)))))]
  (loop [c (vec (for [e s :when ((clojure.set/union opn_cs cls_cs) e)] e)),
         i (flo c (dec (count c)))]
    (cond (empty? c)                                true
          (or (nil? i) (= i (dec (count c))))       false
          (not= (opncls (nth c i)) (nth c (inc i))) false
          :else (recur (r (r c (inc i)) i), (flo c (dec i)))))))

(defn p177 [s]
  (let [opn_cs        #{\{ \( \[}
        cls_cs        #{\} \) \]}
        cls_opn_match {\} \{, \) \(, \] \[ }]
    (loop [acc '(), s s]
      (if-let [c (first s)]
        (cond (cls_cs c)
                (when-let [b (peek acc)]
                  (when (= b (cls_opn_match c))
                    (recur (pop acc), (rest s))))
              (opn_cs c)
                (recur (conj acc c), (rest s))
              :else
                (recur acc, (rest s)))
        (empty? acc)))))

(defn p178 [h]
  (let [cvs              (let [ranks (map second h)]
                           (map #(sort (map (zipmap "A23456789TJQKB" (range 1 15)) %))
                              [ranks (replace {\A \B} ranks)]))
        all_same_suit    (apply = (map first h))
        all_in_sequence  (some true? (map #(= % (range (first %) (+ (first %) (count h))))
                                          cvs))
        sames            (vals (frequencies (map second h)))
       ]
    (cond (and all_same_suit all_in_sequence)                               :straight-flush
          all_same_suit                                                     :flush
          all_in_sequence                                                   :straight
          (some #{4} sames)                                                 :four-of-a-kind
          (and (some #{3} sames) (some #{2} sames))                         :full-house
          (some #{3} sames)                                                 :three-of-a-kind
          (and (= 3 (count sames)) (some #{2} (vals (frequencies sames))))  :two-pair
          (some #{2} sames)                                                 :pair
          :else                                                             :high-card)))

(defn p195 [n]
  (cond (zero? n) #{""} (= 1 n) #{"()"} :else
    (let [oc \(, cc \)
          f (fn f [acc rem_os rem_cs]
              (cond (zero? rem_os)    (str acc (apply str (repeat rem_cs cc)))
                    (= rem_os rem_cs) (f (str acc oc) (dec rem_os) rem_cs)
                    :else             [(f (str acc oc) (dec rem_os) rem_cs     )
                                       (f (str acc cc) rem_os       (dec rem_cs))]))
         ]
      (set (flatten (f (str oc) (dec n) n))))))

;;; 4clojure:CLOSE ;;;




;(defn quicksort [s]
;  (letfn [(qsp [s pi] ;qsp=quicksort pass, pi=pivot index
;            (let [l (count s)]
;              (loop [i 0, pi pi, s s]
;                (if (= i l)
;                  s
;                  (if (< (nth s i) (nth s pi))
;                    (if (<= i pi)
;                      (recur (inc i), pi pi, s)
;                      (recur i, (dec pi), (concat (take (inc i)
;         ]
;
;)



(defn mergesort [c]
  (letfn [(join_sorted [a b]
            (loop [a a, b b, acc []]
              (cond (empty? a) (into acc b)
                    (empty? b) (into acc a)
                    :else      (let [x (first a), y (first b), take_from_a? (<= x y)]
                                 (recur (if take_from_a? (rest a) a)
                                        (if take_from_a? b (rest b))
                                        (if take_from_a? (conj acc x) (conj acc y)))))))]
    (let [len (count c)] (if (<= len 1) c (let [splt (split-at (int (/ len 2)) c)] (join_sorted (mergesort (first splt)) (mergesort (second splt))))))))

;;; ds&alg practice:CLOSE ;;;


