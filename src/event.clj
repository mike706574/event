(ns event
  "Contains the solution."
  (:require
   [clojure.spec :as s]))

;; Function
(defn overlapping-events
  "Given a sequence of events, returns all pairs of overlapping events."
  [events]
  (letfn [(events-overlap?
            [{start-1 ::start end-1 ::end} {start-2 ::start end-2 ::end}]
            (and (< (compare start-1 end-2) 0)
                 (< (compare start-2 end-1) 0)))
          (pairs
            [coll]
            (loop [[head & tail] coll
                   pairs []]
              (if (nil? tail)
                pairs
                (recur tail (apply conj
                                   pairs
                                   (map #(vec head %) tail))))))]
    (filter
     #(apply events-overlap? %)
     (pairs events))))

(defn somewhat-faster-overlapping-events
  "Given a sequence of events, returns all pairs of overlapping events."
  [events]
  (letfn [(events-overlap?
            [{start-1 ::start end-1 ::end} {start-2 ::start end-2 ::end}]
            (and (< (compare start-1 end-2) 0)
                 (< (compare start-2 end-1) 0)))]
    (loop [[head & tail] events
           pairs []]
      (if (nil? tail)
        pairs
        (recur tail (apply conj
                           pairs
                           (into []
                                 (comp
                                  (map #(vector head %))
                                  (filter #(apply events-overlap? %)))
                                 tail)))))))
;; Specs
(def comparable? (partial instance? Comparable))

(s/def ::name string?)
(s/def ::start comparable?)
(s/def ::end comparable?)

(defn end-after-start?
  [{:keys [::start ::end]}]
  (> (compare end start) -1))

(s/def ::event (s/and (s/keys :req [::name ::start ::end])
                      end-after-start?))

(defn factorial
  [n]
  (reduce * (range 1 (inc n))))

(defn count-combinations
  [combo-count group-size]
  (/ (factorial group-size)
     (* (factorial combo-count)
        (factorial (- group-size combo-count)))))

(s/fdef overlapping-events
  :args (s/cat :events (s/coll-of ::event))
  :ret (s/coll-of ::event)
  :fn #(<= 0
           (-> % :args :game count)
           (count-combinations 2 (-> % :args count))))
