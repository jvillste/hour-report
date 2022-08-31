(ns hour-report.spreadsheet
  (:require [dk.ative.docjure.spreadsheet :as spreadsheet]
            [clojure.java.io :as io]
            [medley.core :as medley]
            [clojure.test :refer [is deftest]]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn cell [row column]
  (or (.getCell row column)
      (.createCell row column)))


(defn read-row [row]
  (mapv spreadsheet/read-cell
        (spreadsheet/cell-seq row)))

(defn day-from-date [date]
  (Integer/parseInt (subs date
                          8)))

(deftest test-day-from-date
  (is (= 1
         (day-from-date "2022-08-01")))

  (is (= 9
         (day-from-date "2022-08-09"))))

(defn- enter-hours [input-stream-or-file-name task-mapping hours-by-task]
  (let [workbook (spreadsheet/load-workbook input-stream-or-file-name)
        sheet (first (spreadsheet/sheet-seq workbook))
        available-tasks (set (remove nil? (map (comp first read-row)
                                               (spreadsheet/row-seq sheet))))
        mapped-tasks (set (vals task-mapping))
        tasks-with-hours (set (keys hours-by-task))
        unmapped-tasks (set/difference tasks-with-hours
                                       mapped-tasks)
        unavailable-taks (set/difference (set (map (set/map-invert task-mapping) tasks-with-hours))
                                         available-tasks)]
    (assert (every? #{1}
                    (vals (frequencies (remove nil? (map (comp first read-row)
                                                         (spreadsheet/row-seq sheet))))))
            "Available tasks are not unique")

    (assert (empty? unmapped-tasks)
            (str "unmapped tasks: " (string/join ", " unmapped-tasks)))

    (assert (empty? unavailable-taks)
            (str "unavailable tasks: " (string/join ", " unavailable-taks)))

    (doseq [row (spreadsheet/row-seq sheet)]
      (when-let [row-hours (get hours-by-task
                                (task-mapping (first (read-row row))))]
        (let [hours-by-day (medley/map-keys day-from-date
                                            row-hours)]
          (doseq [day (range 1 32)]
            (when-let [entry (get hours-by-day day)]
              (when (not (= 0 (:total-minutes entry)))
                ;; (println "writing " (first (read-row row)) day (:total-minutes entry))
                (let [cell (cell row (+ 1 day))]
                  (spreadsheet/set-cell! cell
                                         (/ (:total-minutes entry)
                                            60))
                  (let [comments (sort (remove empty? (:descriptions entry)))]
                    (when (not (empty? comments))
;                      (println "adding coment " (first (read-row row)) day (string/join ", " comments))
                      (spreadsheet/set-cell-comment! cell
                                                     (string/join ", " comments)
                                                     :width 8))))))))))
    workbook))

(defn pivot-day-report [day-report]
  (into {}
        (for [[task entry] (:hours day-report)]
          [task {(:date day-report)
                 entry}])))

(deftest test-pivot-day-report
  (is (= {"work1" {"2022-08-01" {:total-minutes 90, :descriptions #{}}},
          "work2" {"2022-08-01" {:total-minutes 90, :descriptions #{"comment"}}}}
         (pivot-day-report {:date "2022-08-01",
                            :hours {"work1" {:total-minutes 90, :descriptions #{}},
                                    "work2" {:total-minutes 90, :descriptions #{"comment"}}}}))))

(defn hours-by-task [day-reports]
  (apply medley/deep-merge (map pivot-day-report day-reports)))

(deftest test-hours-by-task
  (is (= '({:date "2022-08-01",
            :hours {"work1" {:total-minutes 90, :descriptions #{}},
                    "work2" {:total-minutes 90, :descriptions #{"comment"}}}}
           {:date "2022-08-02",
            :hours {"work1" {:total-minutes 90, :descriptions #{}},
                    "work2" {:total-minutes 60, :descriptions #{}}}})
         (hours-by-task '({:date "2022-08-01",
                           :hours {"work1" {:total-minutes 90, :descriptions #{}},
                                   "work2" {:total-minutes 90, :descriptions #{"comment"}}}}
                          {:date "2022-08-02",
                           :hours {"work1" {:total-minutes 90, :descriptions #{}},
                                   "work2" {:total-minutes 60, :descriptions #{}}}})))))

(defn create-hours-spreadsheet [template-file-path output-file-path task-mapping day-reports]
  (with-open [output-stream (io/output-stream output-file-path)]
    (spreadsheet/save-workbook-into-stream! output-stream
                                            (enter-hours template-file-path
                                                         task-mapping
                                                         (hours-by-task day-reports)))))

(comment
  (create-hours-spreadsheet "temp/template.xlsx"
                            "temp/out.xlsx"
                            {"Posti Care - DeTector" "work1"
                             "Monokkeli kehitys" "work2"}
                            '({:date "2022-08-01",
                               :hours {"work1" {:total-minutes 90, :descriptions #{}},
                                       "work2" {:total-minutes 90, :descriptions #{"comment"}}}}
                              {:date "2022-08-02",
                               :hours {"work1" {:total-minutes 90, :descriptions #{}},
                                       "work2" {:total-minutes 60, :descriptions #{}}}}))
  )
