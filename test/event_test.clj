(ns event-test
  (:require
   [clojure.spec :as s]
   [clojure.spec.test :as stest]
   [clojure.test :refer [deftest testing is]]
   [event :as event :refer [brute-force-overlapping-events
                            sort-first-overlapping-events]]))


(deftest check-events-overlap?
  (stest/instrument)
  (stest/check 'event/events-overlap?)
  (stest/unstrument))

(deftest check-overlapping-events
  (stest/instrument)
  (stest/check 'event/events-overlap?)
  (stest/unstrument))

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

(defn just-names
  [pairs]
  (into (empty pairs) (map #(into (empty %) (map :event/name %)) pairs)))

(defn as-sets
  [pairs]
  (into #{} (map #(into #{} %) pairs)))

(deftest no-events
  (testing "no events"
    (is (= []
           (cleaner-overlapping-events []))
        "When there are no events, nothing will overlap.")))

(deftest one-event
  (testing "one event"
    (is (= []
           (cleaner-overlapping-events [{:event/name "Dentist" :event/start 1 :event/end 2}]))
        "When there is only one event, it has no other events to overlap with.")))

(deftest two-events-first-before
  (testing "two events, first event starts and ends before second event begins"
    (is (= #{}
           (-> [{:event/name "Dentist" :event/start 1 :event/end 2}
                {:event/name "Haircut" :event/start 3 :event/end 4}]
               cleaner-overlapping-events
               as-sets)))))

(deftest two-events-first-after
  (testing "two events, first event starts after second event ends"
    (is (= #{}
           (-> [{:event/name "Dentist" :event/start 3 :event/end 4}
                {:event/name "Haircut" :event/start 1 :event/end 2}]
               cleaner-overlapping-events
               as-sets)))))

(deftest two-events-first-adjacent
  (testing "first event ends when second event starts"
    (is (= #{}
           (-> [{:event/name "Dentist" :event/start 1 :event/end 2}
                {:event/name "Haircut" :event/start 2 :event/end 3}]
               cleaner-overlapping-events
               as-sets)))))

(deftest two-events-second-adjacent
  (testing "first event starts when second event ends"
    (is (= #{}
           (-> [{:event/name "Dentist" :event/start 3 :event/end 4}
                {:event/name "Haircut" :event/start 1 :event/end 2}]
               cleaner-overlapping-events
               as-sets)))))

(deftest event-overlap-identity
  (testing "an event will always overlap with itself"
    (is (= [[{:event/name "Dentist" :event/start 1 :event/end 2}
             {:event/name "Dentist" :event/start 1 :event/end 2}]]
           (cleaner-overlapping-events
            [{:event/name "Dentist" :event/start 1 :event/end 2}
             {:event/name "Dentist" :event/start 1 :event/end 2}])))))

(deftest first-ends-during-second
  (testing "first event ends during second event"
    (is (= #{#{{:event/name "Dentist" :event/start 1 :event/end 3}
               {:event/name "Haircut" :event/start 2 :event/end 4}}}
           (-> [{:event/name "Dentist" :event/start 1 :event/end 3}
                {:event/name "Haircut" :event/start 2 :event/end 4}]
               cleaner-overlapping-events
               as-sets)))))

(deftest first-starts-during-second
  (testing "first event starts during second event"
    (is (= #{#{{:event/name "Dentist" :event/start 2 :event/end 4}
                {:event/name "Haircut" :event/start 1 :event/end 3}}}
           (-> [{:event/name "Dentist" :event/start 2 :event/end 4}
              {:event/name "Haircut" :event/start 1 :event/end 3}]
               cleaner-overlapping-events
               as-sets)))))

(deftest four-duplicates
  (testing "four duplicate events produce six overlapping pairs "
    (let [event {:event/name "Dentist" :event/start 2 :event/end 4}]
      (is (= (take 6 (repeat [event event]))
             (cleaner-overlapping-events
              (take 4 (repeat event))))))))

(deftest ten-duplicates
  (testing "ten duplicate events produce forty-five overlapping pairs "
    (let [event {:event/name "Dentist" :event/start 2 :event/end 4}]
      (is (= (take 45 (repeat [event event]))
             (cleaner-overlapping-events
              (take 10 (repeat event))))))))

(deftest four-events-two-overlaps
  (testing "four events, two overlapping pairs"
    (is (= [[(event "A" 1 3) (event "B" 2 4)]
            [(event "C" 6 10) (event "D" 7 8)]]
           (cleaner-overlapping-events
            [(event "A" 1 3)
             (event "B" 2 4)
             (event "C" 6 10)
             (event "D" 7 8)])))))

(deftest four-events-double-overlap
  (testing "four events, two overlapping pairs"
    (is (= [[(event "A" 1 3) (event "B" 2 4)]
            [(event "B" 2 4) (event "C" 3 6)]]
           (cleaner-overlapping-events
            [(event "A" 1 3)
             (event "B" 2 4)
             (event "C" 3 6)
             (event "D" 10 12)])))))

(deftest implementation-equivalence
  (let [samples [#:event{:name "Va6YkJR7", :start -17, :end 1}
                 #:event{:name "3", :start -68, :end -4}
                 #:event{:name "", :start 14, :end 478}
                 #:event{:name "91WI6ml", :start -13, :end 1}
                 #:event{:name "5zEicWF", :start -1, :end 0}
                 #:event{:name "7", :start -109, :end -1}
                 #:event{:name "ybgZp6H9", :start -83, :end -13}
                 #:event{:name "6XO", :start -7, :end 236}
                 #:event{:name "1RB3i", :start -11, :end -2}
                 #:event{:name "NP2hciE4", :start -106, :end 0}
                 #:event{:name "r3", :start -10, :end -5}
                 #:event{:name "", :start -827, :end 6}
                 #:event{:name "NA5119V07", :start -1, :end 6}
                 #:event{:name "c14z1cp", :start -7, :end 103}]]
    (is
     (= (-> samples brute-force-overlapping-events as-sets)
        (-> samples cleaner-overlapping-events as-sets)))))

(deftest sorting-bug
  (is
   (= #{#{#:event{:name "C", :start -1, :end 0}
          #:event{:name "A", :start -1, :end 0}}}
      (-> [#:event{:name "A", :start -1, :end 0}
           #:event{:name "B", :start -1, :end -1}
           #:event{:name "C", :start -1, :end 0}]
          cleaner-overlapping-events
          as-sets))))



(def events
  [(event "Party" (specific-time 2017 4 13 12 0 0) (specific-time 2017 4 13 14 0 0))
   (event "Meeting" (specific-time 2017 3 9 8 30 0) (specific-time 2017 3 9 9 0 0))
   (event "Haircut" (specific-time 2017 3 9 9 15 0) (specific-time 2017 3 9 9 45 0))
   (event "Dentist" (specific-time 2017 3 9 9 0 0) (specific-time 2017 3 9 9 30 0))
   (event "80s" (year 1980) (year 1989))
   (event "82-84" (year 1982) (year 1984))
   (event "90-93" (year 1990) (year 1993))
   (event "January 1990" (day 1990 0 1) (day 1990 0 31))])

(deftest using-dates
  (is (= #{#{"Haircut" "Dentist"}
           #{"80s" "82-84"}
           #{"90-93" "January 1990"}}
         (-> events
             cleaner-overlapping-events
             as-sets
             just-names))))
