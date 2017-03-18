(ns event
  "Contains solutions."
  (:require
   [clojure.spec :as s]))

;; Functions
(defn events-overlap?
  [{start-1 ::start end-1 ::end} {start-2 ::start end-2 ::end}]
  (and (neg? (compare start-1 end-2))
       (neg? (compare start-2 end-1))))

(defn brute-force-overlapping-events
  "Given a sequence of events, returns all pairs of overlapping events.

  Checks if all possible pairs of events overlap."
  [events]
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
                               tail))))))

(defn sort-first-overlapping-events
  "Given a sequence of events, returns all pairs of overlapping events.

  Sorts the events by start time and end time, then iterates through them. For
  each event, gathers subsequent overlapping events until an event that does not
  conflict is found."
  [events]
  (loop [[head & tail] (sort-by ::start events)
         pairs []]
    (if (seq tail)
      (recur tail (into pairs
                        (comp (take-while
                               (partial events-overlap? head))
                              (map #(vector head %)))
                        tail))
      pairs)))

;; Specs
(def comparable? (partial instance? Comparable))

(s/def ::name string?)

(s/def ::start
  (s/spec comparable?
          :gen #(s/gen integer?)))
(s/def ::end
  (s/spec comparable?
          :gen #(s/gen integer?)))

(defn end-after-start?
  [{:keys [::start ::end]}]
  (> (compare end start) 0))

(s/def ::event (s/and (s/keys :req [::name ::start ::end])
                      end-after-start?))

(defn event
  "Constructs an event."
  [nm start end]
  {:event/name nm :event/start start :event/end end})

(s/fdef event
  :args (s/cat :name string? :start comparable? :end comparable?)
  :ret ::event)

(defn factorial
  [n]
  (reduce * (range 1 (inc n))))

(s/fdef factorial
  :args (s/cat :n integer?)
  :ret integer?)

(defn count-combinations
  [combo-count group-size]
  (/ (factorial group-size)
     (* (factorial combo-count)
        (factorial (- group-size combo-count)))))

(s/fdef count-combinations
  :args (s/cat :combo-count integer?
               :group-size integer?)
  :ret integer?)

(s/fdef brute-force-overlapping-events
  :args (s/cat :events (s/coll-of ::event))
  :ret (s/coll-of (s/tuple ::event ::event))
  :fn #(<= 0
           (-> % :ret count)
           (count-combinations 2 (-> % :args :events count))))

(s/fdef events-overlap?
  :args (s/cat :event-1 ::event :event-2 ::event))

(defn as-sets
  [pairs]
  (into #{} (map #(into #{} %) pairs)))

(s/fdef sort-first-overlapping-events
  :args (s/cat :events (s/coll-of ::event))
  :ret (s/coll-of (s/tuple ::event ::event))
  :fn (s/and
       ;; Check that we don't have too many pairs
       #(<= 0
            (-> % :ret count)
            (count-combinations 2 (-> % :args :events count)))
       ;; Check that we get the same result as brute force function
       #(= (-> % :ret as-sets)
           (-> (-> % :args :events) brute-force-overlapping-events as-sets))))
