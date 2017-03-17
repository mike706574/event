(ns bench-test
  (:require
   [clojure.spec :as s]
   [clojure.spec.test :as stest]
   [clojure.spec.gen :as gen]
   [clojure.test :refer [deftest testing is]]
   [event :as event :refer [overlapping-events
                            somewhat-faster-overlapping-events
                            even-faster-overlapping-events]]))

(def rand-event #(gen/generate (s/gen ::event/event)))

;; Extremely inadequate "benchmark"
(let [fs [overlapping-events
          somewhat-faster-overlapping-events
          even-faster-overlapping-events]
      events (take 1000 (repeatedly rand-event))]
  (doseq [f fs]
    (print (with-out-str (time (f events))))))
