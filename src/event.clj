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
            (and (neg? (compare start-1 end-2))
                 (neg? (compare start-2 end-1))))
          (pairs
            [coll]
            (loop [[head & tail] coll
                   pairs []]
              (if (nil? tail)
                pairs
                (recur tail (apply conj
                                   pairs
                                   (map #(vector head %) tail))))))]
    (filter
     #(apply events-overlap? %)
     (pairs events))))

(defn somewhat-faster-overlapping-events
  "Given a sequence of events, returns all pairs of overlapping events."
  [events]
  (letfn [(events-overlap?
            [{start-1 ::start end-1 ::end} {start-2 ::start end-2 ::end}]
            (and (neg? (compare start-1 end-2))
                 (neg? (compare start-2 end-1))))]
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

(defn even-faster-overlapping-events
  "Given a sequence of events, returns all pairs of overlapping events.

  Sorts the events by start time, then iterates over them, gathering subsequent
  overlapping events after the current event until an event that does not
  conflict is found."
  [events]
    (letfn [(events-overlap?
            [{start-1 ::start end-1 ::end} {start-2 ::start end-2 ::end}]
            (and (< (compare start-1 end-2) 0)
                 (< (compare start-2 end-1) 0)))]
      (let [sorted-events (vec (sort-by ::start (shuffle events)))]
        (reduce
         (fn [pairs n]
           (let [n-event (get sorted-events n)]
             (loop [m (inc n)
                    pairs pairs]
               (let [m-event (get sorted-events m)]
                 (if (events-overlap? n-event m-event)
                   (recur (inc m) (conj pairs [n-event m-event]))
                   pairs)))))
         []
         (range (count sorted-events))))))


;; Specs
(def comparable? (partial instance? Comparable))

(s/def ::name string?)

;; Changing these to integers for now so we can generate events...
(s/def ::start integer?)
(s/def ::end integer?)

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
