(ns hour-report.core
  (:require [flow-gl.gui.animation :as animation]
            [flow-gl.gui.visuals :as visuals]
            [fungl.application :as application]
            [fungl.layouts :as layouts]
            [clojure.java.io :as io]
            [flow-gl.graphics.buffered-image :as buffered-image]
            [fungl.component.text-area :as text-area]
            [jsonista.core :as jsonista]
            [clj-time.core :as clj-time]
            [clj-time.format :as format]
            [clojure.test :refer :all]
            [medley.core :as medley]
            [clojure.set :as set]))

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
  (is (= {"reititin" 26, "muuta" 572}
         (report-day [{:startTime {:hour 8, :minute 30},
                       :isWork true,
                       :taskName "reititin"}
                      {:startTime {:hour 8, :minute 56},
                       :isWork false,
                       :taskName "muuta"}]
                     {:hour 18, :minute 28}))))

(defn print-hours [hours]
  (doseq [[task {:keys [total-minutes descriptions]}] hours]
    (println (format "%20s %s %s"
                     task
                     (format-minutes total-minutes)
                     (apply str descriptions))))
  (println (format "%20s %s" "Total:" (format-minutes (reduce + (map :total-minutes (vals hours)))))))

(defn print-day-reports [days]
  (doseq [day days]
    (let [date (format/parse (format/formatter "YYYY-MM-dd")
                             (:date day))
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

(defn read-days-from-file [file-name]
  (->> (jsonista/read-value (slurp file-name)
                            (jsonista/object-mapper {:decode-key-fn keyword}))
       :days
       vals))


(defn read-days []
  (concat (read-days-from-file "/Users/jukka/OneDrive/OneDrive - Nitor Group/backup.git/tunnit 9.1017.8.2018.hrt")
          (read-days-from-file "/Users/jukka/OneDrive/OneDrive - Nitor Group/backup.git/tunnit.hrt 2019")
          (read-days-from-file "/Users/jukka/OneDrive/OneDrive - Nitor Group/backup.git/tunnit.hrt")))

(comment
  (let [month "2020-07"]
    (do (->> (read-days)
             (sort-by :date)
             (map report-day)
             (filter (fn [day] (.startsWith (:date day)
                                            month)))
             (print-day-reports))

        (println "Totals:")

        (->> (read-days)
             (sort-by :date)
             (map report-day)
             (filter (fn [day] (.startsWith (:date day)
                                            month)))
             (combine-days)
             (medley/map-vals (fn [task] (dissoc task :descriptions)))
             (print-hours))))


  (report-day (mapcat ))



  ) ;; TODO: remove-me

(defn text [string]
  (text-area/text  (str string)
                  [0 0 0 255]))

(defn base-view []
  (animation/swap-state! animation/set-wake-up 1000)
  @animation/state-atom
  (layouts/superimpose (visuals/rectangle-2 :fill-color [255 255 255 255])
                       (text "haa")))

(defn start []
  (prn "----------------") ;; TODO: remove-me

  (application/start-window base-view))
