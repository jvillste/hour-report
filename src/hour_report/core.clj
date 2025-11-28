(ns hour-report.core
  (:require
   [clj-time.core :as clj-time]
   [clj-time.format :as format]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer :all]
   [hour-report.spreadsheet :as spreadsheet]
   [jsonista.core :as jsonista]
   [medley.core :as medley]))


;;;;;;;; text report

(defn format-minutes [minutes]
  (str (int (/ minutes 60))
       ":"
       (mod minutes 60)))

(defn hour-minute-to-date-time [date {:keys [hour minute]}]
  (clj-time/date-time (clj-time/year date)
                      (clj-time/month date)
                      (clj-time/day date)
                      hour
                      minute))



(defn report-day [{log-lines :logLines
                   end-time :endTime
                   date :date}]
  {:date date
   :hours (medley/map-vals (fn [log-lines]
                             {:total-minutes (reduce +
                                                     (map (comp clj-time/in-minutes :duration)
                                                          log-lines))
                              :descriptions (into #{} (remove nil? (map :taskName2 log-lines)))})
                           (group-by :taskName
                                     (filter :isWork
                                             (map (fn [[line-1 line-2]]
                                                    (assoc line-1 :duration (if (clj-time/before? (:start-time line-1)
                                                                                                  (:start-time line-2))
                                                                              (clj-time/interval (:start-time line-1)
                                                                                                 (:start-time line-2))
                                                                              (clj-time/minutes 0))))
                                                  (partition 2 1
                                                             (conj (mapv (fn [log-line]
                                                                           (let [hour-minute-start-time (:startTime log-line)]
                                                                             (assoc log-line
                                                                                    :start-time
                                                                                    (clj-time/date-time 2020 1 1
                                                                                                        (:hour hour-minute-start-time)
                                                                                                        (:minute hour-minute-start-time)))))
                                                                         log-lines)
                                                                   {:start-time (clj-time/date-time 2020 1 1
                                                                                                    (:hour end-time)
                                                                                                    (:minute end-time))}))))))})

(deftest test-report-day
  (is (= {:date "2022-01-31",
          :hours {"core" {:total-minutes 71, :descriptions #{"hour-report"}}}}
         (report-day {:date "2022-01-31",
                      :endTime {:hour 17, :minute 41},
                      :logLines
                      [{:taskName2 "hour-report",
                        :startTime {:hour 5, :minute 21},
                        :duration {:hour 10, :minute 39},
                        :isWork true,
                        :taskName "core"}
                       {:startTime {:hour 6, :minute 32},
                        :duration {:hour 0, :minute -52},
                        :isWork false,
                        :taskName "muuta"}],
                      :id "0.5228482764214277",
                      :reportedHours {:hour 7, :minute 30}}))))

(defn print-hours [hours]
  (doseq [[task {:keys [total-minutes descriptions]}] hours]
    (println (format "%20s %s %s"
                     task
                     (format-minutes total-minutes)
                     (string/join ", " descriptions))))
  (println (format "%20s %s" "Total:" (format-minutes (reduce + (map :total-minutes (vals hours)))))))

(defn print-day-reports [days]
  (doseq [day days]
    (let [date (format/parse (format/formatter "YYYY-MM-dd")
                             (subs (:date day)
                                   0
                                   10))
          day-of-week (clj-time/day-of-week date)]
      (when (= 1 day-of-week)
        (println "\n-------------------\n"))
      (println (str ({1 "ma"
                      2 "ti"
                      3 "ke"
                      4 "to"
                      5 "pe"
                      6 "la"
                      7 "su"}
                     day-of-week)
                    " "
                    (format/unparse (format/formatter "d.M")
                                    date))))
    (print-hours (:hours day))))

(defn combine-days [days]
  (apply merge-with (fn [hours-1 hours-2]
                      {:total-minutes (+ (:total-minutes hours-1)
                                         (:total-minutes hours-2))
                       :descriptions (set/union (:descriptions hours-1)
                                                (:descriptions hours-2))})
         (map :hours days)))

(defn read-json [file-name]
  (jsonista/read-value (slurp file-name)
                       (jsonista/object-mapper {:decode-key-fn keyword})))

(defn read-days-from-file [file-name]
  (->> (jsonista/read-value (slurp file-name)
                            (jsonista/object-mapper {:decode-key-fn keyword}))
       :days
       vals))

(defn print-totals! [days date-prefix]
  (->> days
       (sort-by :date)
       (map report-day)
       (filter (fn [day] (.startsWith (:date day)
                                      date-prefix)))
       (combine-days)
       (medley/map-vals (fn [task] (dissoc task :descriptions)))
       (print-hours)))

(defn print-report! [days date-prefix]
  (->> days
       (sort-by :date)
       (map report-day)
       (filter (fn [day] (.startsWith (:date day)
                                      date-prefix)))
       (print-day-reports))

  (println "Totals:")

  (print-totals! days date-prefix))


;;;;;;;;  excel

(defn hours-total [hours]
  (reduce + (map :total-minutes (vals hours))))

(defn round-to-half-hour [minutes]
  (int (* (Math/round (float (/ minutes 30)))
          30)))

(deftest test-round-to-half-hour
  (is (= 0
         (round-to-half-hour 14)))

  (is (= 30
         (round-to-half-hour 15)))

  (is (= 30
         (round-to-half-hour 29)))

  (is (= 30
         (round-to-half-hour 30)))

  (is (= 30
         (round-to-half-hour 31)))

  (is (= -30
         (round-to-half-hour -31))))

(defn round-hours [hours]
  (medley/map-vals (fn [entry]
                     (update entry :total-minutes round-to-half-hour))
                   hours))

(defn round-day-hours [day]
  (update day
          :hours
          round-hours))

(defn hours-difference [hours1 hours2]
  (medley/map-kv-vals (fn [task summary]
                        {:total-minutes (- (:total-minutes summary)
                                           (:total-minutes (get hours2 task)))})
                      hours1))

(deftest test-hours-difference
  (is (= {"work1" {:total-minutes 20}}
         (hours-difference {"work1" {:total-minutes 1020, :descriptions #{}}}
                           {"work1" {:total-minutes 1000, :descriptions #{}}}))))

(defn hours-sum [hours1 hours2]
  (merge (medley/map-kv-vals (fn [task summary]
                               (-> summary
                                   (update :total-minutes
                                           (fn [total-minutes]
                                             (+ total-minutes
                                                (or (:total-minutes (get hours2 task))
                                                    0))))
                                   (cond-> (or (:descriptions summary)
                                               (:descriptions (get hours2 task)))
                                     (update :descriptions
                                             (partial set/union (:descriptions (get hours2 task)))))))
                             hours1)
         (select-keys hours2
                      (set/difference (set (keys hours2))
                                      (set (keys hours1))))
         (select-keys hours1
                      (set/difference (set (keys hours1))
                                      (set (keys hours2))))))

(deftest test-hours-sum
  (is (= {"work1" {:total-minutes 2020, :descriptions #{"foo" "bar"}}}
         (hours-sum {"work1" {:total-minutes 1020, :descriptions #{"foo"}}}
                    {"work1" {:total-minutes 1000, :descriptions #{"bar"}}})))

  (is (= {"work1" {:total-minutes 2020, :descriptions #{"bar"}}}
         (hours-sum {"work1" {:total-minutes 1020}}
                    {"work1" {:total-minutes 1000, :descriptions #{"bar"}}})))

  (is (= {"work1" {:total-minutes 2020, :descriptions #{"bar"}}}
         (hours-sum {"work1" {:total-minutes 1020 :descriptions #{"bar"}}}
                    {"work1" {:total-minutes 1000}})))

  (is (= {"work1" {:total-minutes 2020}}
         (hours-sum {"work1" {:total-minutes 1020}}
                    {"work1" {:total-minutes 1000}})))

  (is (= {"work1" {:total-minutes 20, :descriptions #{}}}
         (hours-sum {"work1" {:total-minutes 1020, :descriptions #{}}}
                    {"work1" {:total-minutes -1000, :descriptions #{}}})))

  (is (= {"work1" {:total-minutes 2020, :descriptions #{}},
          "work3" {:total-minutes 1000, :descriptions #{}},
          "work2" {:total-minutes 1000, :descriptions #{}}}
         (hours-sum {"work1" {:total-minutes 1020, :descriptions #{}}
                     "work3" {:total-minutes 1000, :descriptions #{}}}
                    {"work1" {:total-minutes 1000, :descriptions #{}}
                     "work2" {:total-minutes 1000, :descriptions #{}}}))))

(defn remove-smaller-magnitude [minimum-magnitude hours]
  (medley/remove-vals #(>= minimum-magnitude (Math/abs (:total-minutes %)))
                      hours))

(deftest test-remove-smaller-magnitude
  (is (= {"work2" {:total-minutes 10, :descriptions #{}},
          "work3" {:total-minutes -10, :descriptions #{}}}
         (remove-smaller-magnitude 5
                                   {"work1" {:total-minutes 3, :descriptions #{}}
                                    "work2" {:total-minutes 10, :descriptions #{}}
                                    "work3" {:total-minutes -10, :descriptions #{}}}))))

(defn remove-zero-minutes [hours]
  (medley/remove-vals #(= 0 (:total-minutes %))
                      hours))

(deftest test-remove-zero-minutes
  (is (= {"work2" {:total-minutes 1000, :descriptions #{}}}
         (remove-zero-minutes {"work1" {:total-minutes 0, :descriptions #{}}
                               "work2" {:total-minutes 1000, :descriptions #{}}}))))

(defn day-report-for-compensation [task minutes day-reports]
  (->> day-reports
       (sort-by :date)
       (reverse)
       (medley/find-first (fn [day-report]
                            (if (< 0 minutes)
                              (contains? (:hours day-report)
                                         task)
                              (< 0
                                 (+ minutes
                                    (or (:total-minutes (get (:hours day-report)
                                                             task))
                                        0))))))))

(deftest test-day-report-for-compensation
  (is (= {:date "2022-08-02", :hours {"work1" {:total-minutes 130}}}
         (day-report-for-compensation "work1"
                                      30
                                      [{:date "2022-08-01"
                                        :hours {"work1" {:total-minutes 30}}}
                                       {:date "2022-08-02"
                                        :hours {"work1" {:total-minutes 130}}}])))

  (is (= {:date "2022-08-02", :hours {"work1" {:total-minutes 130}}}
         (day-report-for-compensation "work1"
                                      -100
                                      [{:date "2022-08-01"
                                        :hours {"work1" {:total-minutes 30}}}
                                       {:date "2022-08-02"
                                        :hours {"work1" {:total-minutes 130}}}]))))

(defn compensate [task minutes day-reports]
  (let [day-reports-by-date (medley/index-by :date day-reports)
        day-report-for-compensation (day-report-for-compensation task minutes day-reports)]
    (if (nil? day-report-for-compensation)
      (do (println (str "could not find a day to compensate " minutes " for " task))
          day-reports)
      (do (println "compensating " minutes " minutes for " task " on " (:date day-report-for-compensation))
          (vals (assoc day-reports-by-date
                       (:date day-report-for-compensation)
                       (update day-report-for-compensation
                               :hours
                               (partial hours-sum {task {:total-minutes minutes}}))))))))

(deftest test-compensate
  (is (= '({:date "2022-07-31", :hours {"work0" {:total-minutes 130}}}
           {:date "2022-08-01", :hours {"work1" {:total-minutes 30}}}
           {:date "2022-08-02", :hours {"work1" {:total-minutes 30, :descriptions #{"description"}}}})
         (compensate "work1"
                     -100
                     [{:date "2022-07-31"
                       :hours {"work0" {:total-minutes 130}}}
                      {:date "2022-08-01"
                       :hours {"work1" {:total-minutes 30}}}
                      {:date "2022-08-02"
                       :hours {"work1" {:total-minutes 130
                                        :descriptions #{"description"}}}}])))

  (is (= [{:date "2022-08-02",
           :hours
           {"work1" {:total-minutes 90, :descriptions #{"description"}}}}]
         (compensate "work1"
                     -100
                     [{:date "2022-08-02"
                       :hours {"work1" {:total-minutes 90
                                        :descriptions #{"description"}}}}]))))

(defn day-reports [days date-prefix]
  (->> days
       (filter (fn [day] (.startsWith (:date day)
                                      date-prefix)))
       (sort-by :date)
       (map report-day)))

(defn sort-tasks-by-hours [hours]
  (map first (sort-by (fn [[_task entry]]
                        (:total-minutes entry))
                      hours)))

(deftest test-sort-tasks-by-hours
  (is (= '("work2" "work1")
         (sort-tasks-by-hours {"work1" {:total-minutes 30}
                                 "work2" {:total-minutes 20}}))))

(defn rounded-day-reports [day-reports]
  (assert (not (empty? day-reports)))

  (let [rounded-day-reports (map round-day-hours
                                 day-reports)
        unrounded-totals (combine-days day-reports)
        rounded-totals (combine-days rounded-day-reports)
        compensation (round-hours (remove-smaller-magnitude 15
                                                            (hours-difference unrounded-totals
                                                                              rounded-totals)))
        compensated-rounded-day-reports (sort-by :date
                                                 (reduce (fn [day-reports [task entry]]
                                                           (compensate task (:total-minutes entry) day-reports))
                                                         rounded-day-reports
                                                         compensation))
        final-compensation (- (round-to-half-hour (hours-total (hours-difference (combine-days compensated-rounded-day-reports)
                                                                                 unrounded-totals))))

        final-day-reports (compensate (last (sort-tasks-by-hours unrounded-totals))
                                      final-compensation
                                      compensated-rounded-day-reports)

        difference-after-compensation (remove-zero-minutes (hours-difference (combine-days final-day-reports)
                                                                             unrounded-totals))]
    {:difference-before-compensating (hours-difference (combine-days rounded-day-reports)
                                                       (combine-days day-reports))
     :compensation compensation
     :final-compensation final-compensation
     :rounded-day-reports final-day-reports
     :difference-after-compensation difference-after-compensation
     :total-difference-after-compensation (hours-total difference-after-compensation)}))

(deftest test-rounded-day-reports
  (is (= '{:difference-before-compensating
           {"work1" {:total-minutes 16}, "work2" {:total-minutes 17}},
           :compensation
           {"work1" {:total-minutes -30}, "work2" {:total-minutes -30}},
           :final-compensation 30,
           :rounded-day-reports
           ({:date "2022-08-01",
             :hours
             {"work1" {:total-minutes 90, :descriptions #{}},
              "work2" {:total-minutes 90, :descriptions #{"comment"}}}}
            {:date "2022-08-02",
             :hours
             {"work1" {:total-minutes 90 :descriptions #{}}
              "work2" {:total-minutes 60 :descriptions #{}}}}),
           :difference-after-compensation
           {"work1" {:total-minutes 16}, "work2" {:total-minutes -13}},
           :total-difference-after-compensation 3}

         (rounded-day-reports [{:date "2022-08-01",
                                :hours {"work1" {:total-minutes 89, :descriptions #{}},
                                        "work2" {:total-minutes 88, :descriptions #{"comment"}}}}
                               {:date "2022-08-02",
                                :hours {"work1" {:total-minutes 75, :descriptions #{}},
                                        "work2" {:total-minutes 75, :descriptions #{}}}}]))))

(defn day-contains? [words day]
  (not (empty? (filter (fn [log-line]
                         (some (fn [query-word]
                                 (or (.contains (.toLowerCase (or (:taskName log-line) "")) query-word)
                                     (.contains (.toLowerCase (or (:taskName2 log-line) "")) query-word)))
                               words))
                       (:logLines day)))))


(comment
  (print-report! (read-days-from-file "temp/hours.hrt")
                 "2022-08")

  (spreadsheet/create-hours-spreadsheet "temp/template.xlsx"
                                        "temp/out.xlsx"
                                        {"Monthly Meetings" "monthly meeting"}
                                        (:rounded-day-reports (rounded-day-reports (day-reports (read-days-from-file "temp/hours.hrt")
                                                                                                "2022-08"))))
  )
