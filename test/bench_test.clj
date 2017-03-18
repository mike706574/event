(ns bench-test
  (:require
   [clojure.spec :as s]
   [clojure.spec.test :as stest]
   [clojure.spec.gen :as gen]
   [clojure.test :refer [deftest testing is]]
   [criterium.core :as crit]
   [event :as event :refer [brute-force-overlapping-events
                            sort-first-overlapping-events]]))

(def rand-event #(gen/generate (s/gen ::event/event)))

(defn bench-both
  [size]
  (stest/unstrument)
  (let [events (take size (repeatedly rand-event))]
    (crit/with-progress-reporting
      (crit/bench (brute-force-overlapping-events events)))
    (crit/with-progress-reporting
      (crit/bench (sort-first-overlapping-events events)))))

#_ (bench-both 100)
