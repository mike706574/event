(ns work
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.test :refer [deftest testing is]]
   [clojure.spec :as s]
   [clojure.spec.test :as stest]
   [clojure.spec.gen :as gen]))

;; We need to model an event. At this point I would probably go ask the customer
;; what exactly an event is, but I'll make some (hopefully reasonable)
;; assumptions instead.

;; At a minimum for this problem, events have a start and an end time. If we're
;; pretending to write a real calendar application, they will probably have
;; other properties describing the event - I'll just add a name, which will
;; make it easier to keep track of which events are which.

;; I like referring to things using names, so I'll use a map instead of a tuple
;; (really a vector) containing 3 elements. I could also define an Event record,
;; but it seems like overkill and it wouldn't change the implementation much
;; anyways.

;; So here's an event:

{::name "Dentist"
 ::start "?"
 ::end "?"}

;; We need something to represent the time. The problem doesn't specify what,
;; but it really doesn't matter what we use as long as it's comparable. I'll use
;; numbers for testing and switch to using java.util.Date instances later.

;; So this is what an event that starts at 5 and ends at 6 looks like:

{::name "Dentist"
 ::start 5
 ::end 6}

;; I'm also going to assume that if an event ends at the same time another
;; starts, they don't overlap.

;; Let's write a spec for events - under the assumption that the times
;; are represented as integers.
(s/def ::name (s/and string? #(<= 1 (count %) 40)))
(s/def ::start integer?)
(s/def ::end integer?)
(defn end-after-start?
  [{:keys [::start ::end]}]
  (> (compare end start) -1))

(s/def ::event (s/and (s/keys :req [::name ::start ::end])
                      end-after-start?))

;; We can now generate an event:
(gen/generate (s/gen ::event))
;; => #:event{:name "3ZrVhCy2sbAoWB4Jeg6",
;;            :start 25197,
;;            :end -321318}

;; To me, the most obvious, brute force solution is to take all possible                                                                    
;; possible pairs of events and determine if they overlap one by one. We'll need                                                              
;; twos maller functions:   

;; 1) a predicate for determining if two events overlap
;; 2) a function for generating all possible pairs of events

;; Then we'll compose those together in the function that finds all the
;; overlapping pairs of events.

;; Let's start by writing a predicate for checking if two events overlap.
(declare events-overlap?)

;; We can spec the predicate function...
(s/fdef events-overlap?
  :args (s/cat :event-1 ::event :event-2 ::event)
  :ret boolean?)

;; Write some tests by hand.
(deftest detecting-overlapping-events
  (testing "first event starts and ends before second event begins"
    (is (not (events-overlap?
              {::name "Dentist" ::start 1 ::end 2}
              {::name "Haircut" ::start 3 ::end 4}))
        "When an event ends before another starts, they do not overlap."))
  (testing "first event starts after second event ends"
    (is (not (events-overlap?
              {::name "Dentist" ::start 3 ::end 4}
              {::name "Haircut" ::start 1 ::end 2}))
        "When an event ends before another starts, they do not overlap."))
  (testing "first event ends when second event starts"
    (is (not (events-overlap?
              {::name "Dentist" ::start 1 ::end 2}
              {::name "Haircut" ::start 2 ::end 3}))
        "When an event ends at the same time as another starts, they do not overlap."))
  (testing "first event starts when second event ends"
    (is (not (events-overlap?
              {::name "Dentist" ::start 3 ::end 4}
              {::name "Haircut" ::start 1 ::end 2}))
        "When an event ends at the same time as another starts, they do not overlap."))
  (testing "first event ends during second event"
    (is (events-overlap?
         {::name "Dentist" ::start 1 ::end 3}
         {::name "Haircut" ::start 2 ::end 4})
        "When an event ends during another, they overlap."))
  (testing "first event starts during second event"
    (is (events-overlap?
         {::name "Dentist" ::start 2 ::end 4}
         {::name "Haircut" ::start 1 ::end 3})
        "When an event ends during another, they overlap.")))

;; Implement the predicate - O(1)
(defn events-overlap?
  "Returns true if the two events overlap, otherwise false."
  [{start-1 ::start end-1 ::end} {start-2 ::start end-2 ::end}]
  (and (< (compare start-1 end-2) 0)
       (< (compare start-2 end-1) 0)))

;; We can also test with test.check:
;; (stest/check `events-overlap?)
;; => ({:spec
;;      #object[clojure.spec$fspec_impl$reify__14282 0x111c4195 "clojure.spec$fspec_impl$reify__14282@111c4195"],
;;      :clojure.spec.test.check/ret
;;      {:result true, :num-tests 1000, :seed 1489531969194},
;;      :sym event/events-overlap?})

;; We can probably use clojure.math.combinatorics/combinations to get all the
;; pairs, so we'll start working on the big function:
(declare overlapping-events)

;; I can think of one property we can always check: the number of overlapping
;; pairs will always be between 0 and the number of combinations of size 2 taken 
;; from the sequence of all events.

;; We can some mathy functions to calculate that upper bound:
(defn factorial
  "Returns the factorial of n."
  [n]
  (reduce * (range 1 (inc n))))

(defn count-combinations
  "Returns the number of combinations of combo-count items taken from a set
   containing group-size items."
  [combo-count group-size]
  (/ (factorial group-size)
     (* (factorial combo-count)
        (factorial (- group-size combo-count)))))

(def count-two-item-combinations (partial count-combinations 2))

;; Spec for the function:
(s/fdef overlapping-events
  :args (s/coll-of ::event)
  :ret (s/coll-of ::event)
  :fn #(<= 0
           (-> % :args :game count)
           (count-two-item-combinations (-> % :args count))))

;; Another assumption needs to be made here: if two events have the same name,
;; are they two different events?

;; I'm going to assume that if the sequence of events contains two items with
;; the same name, they are different events.

;; Because of this, we can't just use clojure.math.combinatorics/combinations to
;; generate all the possible pairs from the event sequence because it doesn't
;; seem to like multiple duplicates:
(combo/combinations [1 1 1] 2)
;; => ((1 1))

;; This should do what we want:
(defn pairs
  [coll]
  (loop [[head & tail] coll
         pairs (list)]
    (if (nil? tail)
      pairs
      (recur tail (apply conj
                         pairs
                         (map #(list head %) tail))))))

;; Check it:
(pairs [1 1 1])
;; => ((1 1) (1 1) (1 1))

;; It works!

;; Make a helper function for constructing events:
(defn new-event
  "Constructs an event."
  [nm start end]
  {::name nm ::start start ::end end})

;; Write some tests by hand.

;; NOTE: These tests aren't the greatest in the world - we probably don't
;; care about the order in which the pairs are returned, but we're asserting
;; it here.
(deftest finding-find--overlapping-events
  (testing "no events"
    (is (= []
           (overlapping-events []))
        "When there are no events, nothing will overlap."))

  (testing "one event"
    (is (= []
           (overlapping-events [{::name "Dentist" ::start 1 ::end 2}]))
        "When there is only one event, it has no other events to overlap with."))

  (testing "two events, first event starts and ends before second event begins"
    (is (= []
           (overlapping-events
            [{::name "Dentist" ::start 1 ::end 2}
             {::name "Haircut" ::start 3 ::end 4}]))))

  (testing "two events, first event starts after second event ends"
    (is (= []
           (overlapping-events
            [{::name "Dentist" ::start 3 ::end 4}
             {::name "Haircut" ::start 1 ::end 2}]))))

  (testing "first event ends when second event starts"
    (is (= []
           (overlapping-events
            [{::name "Dentist" ::start 1 ::end 2}
             {::name "Haircut" ::start 2 ::end 3}]))))

  (testing "first event starts when second event ends"
    (is (= []
           (overlapping-events
            [{::name "Dentist" ::start 3 ::end 4}
             {::name "Haircut" ::start 1 ::end 2}]))))

  (testing "an event will always overlap with itself"
    (is (= [[{::name "Dentist" ::start 1 ::end 2}
             {::name "Dentist" ::start 1 ::end 2}]]
           (overlapping-events
            [{::name "Dentist" ::start 1 ::end 2}
             {::name "Dentist" ::start 1 ::end 2}]))))

  (testing "first event ends during second event"
    (is (= [[{::name "Dentist" ::start 1 ::end 3}
             {::name "Haircut" ::start 2 ::end 4}]]
           (overlapping-events
            [{::name "Dentist" ::start 1 ::end 3}
             {::name "Haircut" ::start 2 ::end 4}]))))


  (testing "first event starts during second event"
    (is (= [[{::name "Dentist" ::start 2 ::end 4}
             {::name "Haircut" ::start 1 ::end 3}]]
           (overlapping-events
            [{::name "Dentist" ::start 2 ::end 4}
             {::name "Haircut" ::start 1 ::end 3}]))))

  (testing "four duplicate events produce six overlapping pairs "
    (let [event {::name "Dentist" ::start 2 ::end 4}]
      (is (= (take 6 (repeat [event event]))
             (overlapping-events
              (take 4 (repeat event)))))))

  (testing "ten duplicate events produce forty-five overlapping pairs "
    (let [event {::name "Dentist" ::start 2 ::end 4}]
      (is (= (take 45 (repeat [event event]))
             (overlapping-events
              (take 10 (repeat event)))))))

  (testing "fours events, two overlapping pairs"
    (is (= [[(new-event "C" 6 10) (new-event "D" 7 8)]
            [(new-event "A" 1 3) (new-event "B" 2 4)]]
           (overlapping-events
            [(new-event "A" 1 3)
             (new-event "B" 2 4)
             (new-event "C" 6 10)
             (new-event "D" 7 8)]))))

  (testing "fours events, two overlapping pairs"
    (is (= [[(new-event "B" 2 4) (new-event "C" 3 6)]
            [(new-event "A" 1 3) (new-event "B" 2 4)]]
           (overlapping-events
            [(new-event "A" 1 3)
             (new-event "B" 2 4)
             (new-event "C" 3 6)
             (new-event "D" 10 12)]))))

  (testing "fours events, two overlapping pairs"
    (is (= [[(new-event "B" 2 4) (new-event "C" 3 6)]
            [(new-event "A" 1 3) (new-event "B" 2 4)]]
           (overlapping-events
            [(new-event "A" 1 3)
             (new-event "B" 2 4)
             (new-event "C" 3 6)
             (new-event "D" 10 12)])))))

;; Here's the final function:
(defn overlapping-events
  "Given a sequence of events, returns all pairs of overlapping events in O(n!) time."
  [events]
  (filter
   #(apply events-overlap? %)
   (pairs events)))

;; Now, we'll try it out with java.util.Dates instead of numbers.

(defn year
  "Creates a java.util.Date for a specific year."
  [year]
  (.getTime
   (doto (java.util.Calendar/getInstance)
     (.clear)
     (.set year 0 1))))

(defn day
  "Creates a java.util.Date for a specific day."
  [year month day]
  (.getTime
   (doto (java.util.Calendar/getInstance)
     (.clear)
     (.set year month day))))

(defn time
  "Creates a java.util.Date for a time, down to the second."
  [year month day hour min sec]
  (.getTime
   (doto (java.util.Calendar/getInstance)
     (.clear)
     (.set year month day hour min sec))))

(defn print-conflicts
  [pairs]
  (doseq [[a b] pairs]
    (println (str "Event \"" (::name a) "\" conflicts with \"" (::name b) "\"."))))

(def events
  [(new-event "Party" (time 2017 4 13 12 0 0) (time 2017 4 13 14 0 0))
   (new-event "Meeting" (time 2017 3 9 8 30 0) (time 2017 3 9 9 0 0))
   (new-event "Haircut" (time 2017 3 9 9 15 0) (time 2017 3 9 9 45 0))
   (new-event "Dentist" (time 2017 3 9 9 0 0) (time 2017 3 9 9 30 0))
   (new-event "80s" (year 1980) (year 1989))
   (new-event "82-84" (year 1982) (year 1984))
   (new-event "90-93" (year 1990) (year 1993))
   (new-event "January 1990" (day 1990 0 1) (day 1990 0 31))])

(-> events
    (overlapping-events)
    (print-conflicts))

;; => Event "90-93" conflicts with "January 1990".
;;    Event "80s" conflicts with "82-84".
;;    Event "Haircut" conflicts with "Dentist".

;; It "seems" to work.
