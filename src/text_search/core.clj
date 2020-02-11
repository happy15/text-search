(ns text-search.core)

(defn search-naive [pattern text]
  "This naive version has very high time complexity of O(mn^2), as m = len(pattern) and n = len(text):
  1. brutal force itself has very high time complexity of O(mn).
  2. (subs text 1) introduce additional layer of time complexity of O(len(text)), as there is a arraycopy in (subs)"
  (do
    (println pattern (count pattern))
    (println text (count text))
    (let [pat-len (count pattern)]
      (loop [text  text
             acc   []
             i     0
             times 0]
        (if (< (count text) pat-len)
          (do
            (println "times=" times)
            acc)
          (if (= (subs text 0 pat-len) pattern)
            (recur (subs text 1) (conj acc i) (inc i) (+ times pat-len))
            (recur (subs text 1) acc (inc i) (+ times pat-len))))))))

(defn search-naive-chars [pattern text]
  "This version beats (search-naive) because of elimination of (subs text 1). Thus O(mn)."
  (let [pat     (vec pattern)
        txt     (vec text)
        pat-len (count pat)
        txt-len (count txt)]
    (loop [acc  []
           i    0]
      (if (> (+ i pat-len) txt-len)
        acc
        (if (= (subvec txt i (+ i pat-len)) pat)
          (recur (conj acc i) (inc i))
          (recur acc (inc i)))))))

(defn error [msg]
  (throw (Exception. msg)))

(defn lps [pat]
  "Returns Longest Prefix Suffix (LPS) for each prefix of pat except itself."
  (let [pat-len (count pat)]
    (loop [j       0
           i       1
           acc     [0]]
      (do
        ;(prn j i acc (dec j) (get lps (dec j)))
        (if (= i pat-len)
          acc
          (if (= (get pat j) (get pat i))
            (recur (inc j) (inc i) (conj acc (inc j)))
            (if (= j 0)
              (recur 0 (inc i) (conj acc 0))
              (recur (get acc (dec j)) i acc))))))))

(defn search-kmp [pattern text]
  "This kmp implementation needs the whole body of text before processing, thus not suitable for matching on stream."
  (let [pat     (vec pattern)
        txt     (vec text)
        pat-len (count pat)
        txt-len (count txt)
        lps     (lps pat)]
    (prn pattern pat-len)
    (prn text txt-len)
    (prn lps)
    (loop [ti    0
           pi    0
           acc   []
           times 0]
      (do
        (println (str "#" times ":") ti (get txt (+ ti pi)) pi acc)
        (if (> (+ ti pat-len) txt-len)
          (do
            (println "times =" times)
            acc)
          (if (= pi pat-len)
            ;found one match
            (let [lps (get lps (dec pi))]
              (recur (+ ti (- pi lps)) 0 (conj acc ti) (inc times)))
            (if (= (get pat pi) (get txt (+ ti pi)))
              (recur ti (inc pi) acc (inc times))
              (if (= pi 0)
                ;first character or pattern already mismatched,
                ;thus comparing to next text character with first of pattern character on next iteration.
                (recur (inc ti) 0 acc (inc times))
                ;try right shift pattern according to lps
                (let [lps (get lps (dec pi))]
                  (recur (+ ti (- pi lps)) lps acc (inc times)))))))))))

(defn search-kmp2 [pattern text]
  "This KMP implementation differs from (search-kmp) in that ti points to the end of text sub string."
  (let [pat     (vec pattern)
        txt     (vec text)
        pat-len (count pat)
        txt-len (count txt)
        lps     (lps pat)]
    (prn pattern pat-len)
    (prn text txt-len)
    (prn lps)
    (loop [ti    0
           pi    0
           acc   []
           times 0]
      (do
        ;(when (> times 50) (error "boom"))
        (println (str "#" times ":") ti (get txt ti) pi acc)
        (if (>= ti txt-len)
          (do
            (println "times =" times)
            (println "len(text) =" ti)
            (if (= pi pat-len)
              (conj acc (- ti pat-len))
              acc))
          (if (= pi pat-len)
            ;found one match
            (let [lps (get lps (dec pi))]
              (recur ti lps (conj acc (- ti pat-len)) (inc times)))
            (if (= (get pat pi) (get txt ti))
              (recur (inc ti) (inc pi) acc (inc times))
              (if (= pi 0)
                ;first character or pattern already mismatched,
                ;thus comparing to next text character with first of pattern character on next iteration.
                (recur (inc ti) 0 acc (inc times))
                ;try right shift pattern according to lps
                (let [lps (get lps (dec pi))]
                  (recur ti lps acc (inc times)))))))))))


(defn search-kmp-seq [pattern text-seq]
  "This KMP implementation can handle text as seq (thus stream), which is capabel of processing large file stream."
  (let [pat     (vec pattern)
        pat-len (count pat)
        lps     (lps pat)]
    (println pattern pat-len)
    (println lps)
    (loop [ts    text-seq
           ti    0
           pi    0
           acc   []
           times 0]
      (let [tc (first ts)]
        (do
          ;        (when (> times 50) (error "boom"))
          (println (str "#" times ":") ti tc pi acc)
          (if (nil? tc)
            ;finished reading of text sequence, return result
            (do
              (println "times =" times)
              (println "len(text) =" ti)
              (if (= pi pat-len)
                ;there may be a match at last
                (conj acc (- ti pat-len))
                acc))
            (if (= pi pat-len)
              ;found one match
              (recur ts
                ti
                (get lps (dec pi))
                (conj acc (- ti pat-len))
                (inc times))
              (if (= (get pat pi) tc)
                (recur (rest ts) (inc ti) (inc pi) acc (inc times))
                (if (= pi 0)
                  ;first character or pattern already mismatched,
                  ;thus comparing to next text character with first of pattern character on next iteration.
                  (recur (rest ts)
                    (inc ti)
                    0
                    acc
                    (inc times))
                  ;try right shift pattern according to lps
                  (recur ts
                    ti
                    (get lps (dec pi))
                    acc
                    (inc times)))))))))))


(defn search-kmp-seq2 [pattern text-seq]
  "This KMP implementation differs from (search-kmp-seq) in that it uses (cond) instead of (if)."
  (let [pat     (vec pattern)
        pat-len (count pat)
        lps     (lps pat)]
    (println pattern pat-len)
    (println lps)
    (loop [ts    text-seq
           ti    0
           pi    0
           acc   []
           times 0]
      (let [tc (first ts)]
        (do
          ;        (when (> times 50) (error "boom"))
          (println (str "#" times ":") ti tc pi acc)
          (cond
            ;finished reading of text sequence, return result
            (nil? tc)                                                    (do
                                                                           (println "times =" times)
                                                                           (println "len(text) =" ti)
                                                                           (if (= pi pat-len)
                                                                             ;there may be a match at last
                                                                             (conj acc (- ti pat-len))
                                                                             acc))
            ;found one match
            (= pi pat-len)                                               (recur ts
                                                                           ti
                                                                           (get lps (dec pi))
                                                                           (conj acc (- ti pat-len))
                                                                           (inc times))
            (= (get pat pi) tc)                                          (recur (rest ts) (inc ti) (inc pi) acc (inc times))
            ;first character or pattern already mismatched,
            ;thus comparing to next text character with first of pattern character on next iteration.
            (= pi 0)                                                     (recur (rest ts) (inc ti) 0 acc (inc times))
            ;try right shift pattern according to lps
            :else                                                        (recur ts ti
                                                                           (get lps (dec pi)) acc
                                                                           (inc times))))))))


(def pattern "aaaaab")
(def text "aaaaaaaaaaaaaaaab")

(def pattern "ababa")
(def text "abababababababa")

(def pattern "ababc")
(def text "ababababababababc")

(count pattern)
(count text)

(search-naive pattern text)

(search-naive-chars pattern text)

(search-kmp pattern text)

(search-kmp2 pattern text)

(search-kmp-seq pattern (lazy-seq text))

(search-kmp-seq2 pattern (lazy-seq text))


(vec pattern)

(lps (vec pattern))

(lps (vec (char-array "aaaaab")))
