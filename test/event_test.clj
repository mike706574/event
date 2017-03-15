(ns event-test
  (:require
   [clojure.spec :as s]
   [clojure.spec.test :as stest]
   [clojure.test :refer [deftest testing is]]
   [event :as event :refer [overlapping-events
                            somewhat-faster-overlapping-events]]))

(defn new-event
  "Constructs an event."
  [nm start end]
  {:event/name nm :event/start start :event/end end})

(s/fdef new-event
  :args (s/cat :name :event/name :start :event/start :end :event/end)
  :ret :event/event)

(defn year
  "Creates a java.util.Date for a specific year."
  [year]
  (.getTime (doto (java.util.Calendar/getInstance)
              (.clear)
              (.set year 0 1))))

(defn day
  "Creates a java.util.Date for a specific day."
  [year month day]
  (.getTime (doto (java.util.Calendar/getInstance)
              (.clear)
              (.set year month day))))

(defn specific-time
  "Creates a java.util.Date for a time, down to the second."
  [year month day hour min sec]
  (.getTime (doto (java.util.Calendar/getInstance)
              (.clear)
              (.set year month day hour min sec))))

(defn print-conflicts
  [pairs]
  (doseq [[a b] pairs]
    (println (str "Event \"" (:event/name a) "\" conflicts with \"" (:event/name b) "\"."))))

(def events
  [(new-event "Party" (specific-time 2017 4 13 12 0 0) (specific-time 2017 4 13 14 0 0))
   (new-event "Meeting" (specific-time 2017 3 9 8 30 0) (specific-time 2017 3 9 9 0 0))
   (new-event "Haircut" (specific-time 2017 3 9 9 15 0) (specific-time 2017 3 9 9 45 0))
   (new-event "Dentist" (specific-time 2017 3 9 9 0 0) (specific-time 2017 3 9 9 30 0))
   (new-event "80s" (year 1980) (year 1989))
   (new-event "82-84" (year 1982) (year 1984))
   (new-event "90-93" (year 1990) (year 1993))
   (new-event "January 1990" (day 1990 0 1) (day 1990 0 31))])

(stest/instrument)

(comment
  (time (do (-> (take 500 (repeat (new-event "January 1990" (day 1990 0 1) (day 1990 0 31))))
                (somewhat-faster-overlapping-events))
            nil))
  ;; => "Elapsed time: 422.343765 msecs"

  (time (do (-> (take 300 (repeat (new-event "January 1990" (day 1990 0 1) (day 1990 0 31))))
                (somewhat-faster-overlapping-events))
            nil)))


(defn just-names
  [pairs]
  (map #(map :event/name %) pairs))

(defn as-sets
  [pairs]
  (into #{} (map #(into #{} %) pairs)))

;; NOTE: These tests aren't the greatest in the world - we probably don't
;; care about the order in which the pairs are returned.
(deftest finding-overlapping-events
  (testing "no events"
    (is (= []
           (somewhat-faster-overlapping-events []))
        "When there are no events, nothing will overlap."))

  (testing "one event"
    (is (= []
           (somewhat-faster-overlapping-events [{:event/name "Dentist" :event/start 1 :event/end 2}]))
        "When there is only one event, it has no other events to overlap with."))

  (testing "two events, first event starts and ends before second event begins"
    (is (= #{}
           (-> [{:event/name "Dentist" :event/start 1 :event/end 2}
                {:event/name "Haircut" :event/start 3 :event/end 4}]
               (somewhat-faster-overlapping-events)
               (as-sets)))))

  (testing "two events, first event starts after second event ends"
    (is (= #{}
           (-> [{:event/name "Dentist" :event/start 3 :event/end 4}
                {:event/name "Haircut" :event/start 1 :event/end 2}]
               (somewhat-faster-overlapping-events)
               (as-sets)))))

  (testing "first event ends when second event starts"
    (is (= #{}
           (-> [{:event/name "Dentist" :event/start 1 :event/end 2}
                {:event/name "Haircut" :event/start 2 :event/end 3}]
               (somewhat-faster-overlapping-events)
               (as-sets)))))

  (testing "first event starts when second event ends"
    (is (= #{}
           (-> [{:event/name "Dentist" :event/start 3 :event/end 4}
                {:event/name "Haircut" :event/start 1 :event/end 2}]
               (somewhat-faster-overlapping-events)
               (as-sets)))))

  (testing "an event will always overlap with itself"
    (is (= [[{:event/name "Dentist" :event/start 1 :event/end 2}
             {:event/name "Dentist" :event/start 1 :event/end 2}]]
           (somewhat-faster-overlapping-events
            [{:event/name "Dentist" :event/start 1 :event/end 2}
             {:event/name "Dentist" :event/start 1 :event/end 2}]))))

  (testing "first event ends during second event"
    (is (= [[{:event/name "Dentist" :event/start 1 :event/end 3}
             {:event/name "Haircut" :event/start 2 :event/end 4}]]
           (somewhat-faster-overlapping-events
            [{:event/name "Dentist" :event/start 1 :event/end 3}
             {:event/name "Haircut" :event/start 2 :event/end 4}]))))

  (testing "first event starts during second event"
    (is (= [[{:event/name "Dentist" :event/start 2 :event/end 4}
             {:event/name "Haircut" :event/start 1 :event/end 3}]]
           (somewhat-faster-overlapping-events
            [{:event/name "Dentist" :event/start 2 :event/end 4}
             {:event/name "Haircut" :event/start 1 :event/end 3}]))))

  (testing "four duplicate events produce six overlapping pairs "
    (let [event {:event/name "Dentist" :event/start 2 :event/end 4}]
      (is (= (take 6 (repeat [event event]))
             (somewhat-faster-overlapping-events
              (take 4 (repeat event)))))))

  (testing "ten duplicate events produce forty-five overlapping pairs "
    (let [event {:event/name "Dentist" :event/start 2 :event/end 4}]
      (is (= (take 45 (repeat [event event]))
             (somewhat-faster-overlapping-events
              (take 10 (repeat event)))))))

  (testing "fours events, two overlapping pairs"
    (is (= [[(new-event "A" 1 3) (new-event "B" 2 4)]
            [(new-event "C" 6 10) (new-event "D" 7 8)]]
           (somewhat-faster-overlapping-events
            [(new-event "A" 1 3)
             (new-event "B" 2 4)
             (new-event "C" 6 10)
             (new-event "D" 7 8)]))))

  (testing "fours events, two overlapping pairs"
    (is (= [[(new-event "A" 1 3) (new-event "B" 2 4)]
            [(new-event "B" 2 4) (new-event "C" 3 6)]]
           (somewhat-faster-overlapping-events
            [(new-event "A" 1 3)
             (new-event "B" 2 4)
             (new-event "C" 3 6)
             (new-event "D" 10 12)]))))

  (testing "fours events, two overlapping pairs"
    (is (= [["A" "B"]
            ["B" "C"]]
           (-> [(new-event "A" 1 3)
                (new-event "B" 2 4)
                (new-event "C" 3 6)
                (new-event "D" 10 12)]
               (somewhat-faster-overlapping-events)
               (just-names)))))

  (is (= [["Haircut" "Dentist"]
          ["80s" "82-84"]
          ["90-93" "January 1990"]]
         (-> events
             (somewhat-faster-overlapping-events)
             (just-names))))
  )
