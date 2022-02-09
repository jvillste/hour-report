(ns hour-report.core
  (:require [clj-time.core :as clj-time]
            [clj-time.format :as format]
            [clojure.set :as set]
            [clojure.test :refer :all]
            [flow-gl.gui.visuals :as visuals]
            [fungl.component.text-area :as text-area]
            [fungl.layouts :as layouts]
            [jsonista.core :as jsonista]
            [medley.core :as medley]
            [flow-gl.gui.animation :as animation]
            [fungl.application :as application]
            [flow-gl.graphics.font :as font]
            [fungl.dependable-atom :as dependable-atom]
            [clojure.core.async :as async]))

(def sample-day {:date {:year 2021 :month 4 :day 13}
                 :end-time {:hour 16, :minute 52}
                 :events
                 [{:task-name "core"
                   :description "dalin kehitystä"
                   :start-time {:hour 5, :minute 20}
                   :work? true}
                  {:task-name "muuta"
                   :start-time {:hour 7, :minute 0}
                   :work? false}]})

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

(defn report-day [{:keys [events end-time date]}]
  {:date date
   :hours (medley/map-vals (fn [events]
                             {:total-minutes (reduce +
                                                     (map (comp clj-time/in-minutes :duration)
                                                          events))
                              :descriptions (into #{} (remove nil? (map :taskName2 events)))})
                           (group-by :task-name
                                     (filter :work?
                                             (map (fn [[line-1 line-2]]
                                                    (assoc line-1 :duration (if (clj-time/before? (:start-time line-1)
                                                                                                  (:start-time line-2))
                                                                              (clj-time/interval (:start-time line-1)
                                                                                                 (:start-time line-2))
                                                                              (clj-time/minutes 0))))
                                                  (partition 2 1
                                                             (conj (mapv (fn [log-line]
                                                                           (let [hour-minute-start-time (:start-time log-line)]
                                                                             (assoc log-line
                                                                                    :start-time
                                                                                    (clj-time/date-time 2020 1 1
                                                                                                        (:hour hour-minute-start-time)
                                                                                                        (:minute hour-minute-start-time)))))
                                                                         events)
                                                                   {:start-time (clj-time/date-time 2020 1 1
                                                                                                    (:hour end-time)
                                                                                                    (:minute end-time))}))))))})

(deftest test-report-day
  (is (= {:date {:year 2021, :month 4, :day 13},
          :hours {"core" {:total-minutes 100, :descriptions #{}}}}
         (report-day sample-day))))

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


(defn read-days []
  (concat #_(read-days-from-file "/Users/jukka/OneDrive/OneDrive - Nitor Group/backup.git/tunnit 9.1017.8.2018.hrt")
          #_(read-days-from-file "/Users/jukka/OneDrive/OneDrive - Nitor Group/backup.git/tunnit.hrt 2019")
          #_(read-days-from-file "/Users/jukka/OneDrive/OneDrive - Nitor Group/backup.git/tunnit 2022.hrt")
          (read-days-from-file "/Users/jukka/OneDrive/OneDrive - Nitor Group/backup.git/tunnit.hrt")
          (:days (read-json "temp/tunnit-jullela-19.hrt"))))

(defn print-report! [month]
  (->> (read-days)
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
       (print-hours)))

(defn day-contains? [words day]
  (not (empty? (filter (fn [log-line]
                         (some (fn [query-word]
                                 (or (.contains (.toLowerCase (or (:taskName log-line) "")) query-word)
                                     (.contains (.toLowerCase (or (:taskName2 log-line) "")) query-word)))
                               words))
                       (:logLines day)))))

(comment
  (->> (read-days)
       (filter (partial day-contains? ["jakeluhäiriö" "disruption"]))
       (map report-day)
       (combine-days)
       (print-hours))

  (print-day-reports (filter (fn [day]
                               (not (empty? (filter (fn [log-line]
                                                      (some (fn [query-word]
                                                              (or (.contains (.toLowerCase (or (:taskName log-line) "")) query-word)
                                                                  (.contains (.toLowerCase (or (:taskName2 log-line) "")) query-word)))
                                                            ["jakeluhäi" "disrupt"]))
                                                    (:logLines day)))))
                             (read-days)))


  (->> (read-days)
       (sort-by :date)
       (map report-day)
       (filter (fn [day] (.startsWith (:date day)
                                      month)))
       (print-day-reports))
  )



;; UI

(comment
  (font/available-names)
  )

(def font-size 40)
(def regular-font (font/create-by-name "CourierNewPSMT" font-size))
(def bold-font (font/create-by-name "CourierNewPS-BoldMT" font-size))

(defn text [string & [font]]
  (text-area/text  (str string)
                   [0 0 0 255]
                   (or font
                       regular-font)))

(defn box [content & [{:keys [fill-color] :or {fill-color [255 255 255 255]}}]]
  (layouts/box 10
               (visuals/rectangle-2 :fill-color fill-color
                                    :draw-color [200 200 200 255]
                                    :line-width 4
                                    :corner-arc-radius 30)
               content))


(defn focused [content]
  (layouts/box 5
               (visuals/rectangle-2 :fill-color [220 220 255 255]
;;                                    :draw-color [200 200 200 255]
;;                                    :line-width 4
                                    :corner-arc-radius 30)
               content))

(defn button-mouse-event-handler [on-pressed node event]
  (when (= :mouse-clicked (:type event))
    (on-pressed))
  event)

(defn button [label on-pressed]
  (assoc (layouts/box 15
                      (visuals/rectangle-2 :fill-color [200 200 255 255]
                                           :draw-color [100 100 200 255]
                                           :line-width 10
                                           :corner-arc-radius 30)
                      (text label))
         :mouse-event-handler [button-mouse-event-handler on-pressed]))

(defn bare-text-editor [text on-change & [{:keys [validate-new-text]}]]
  [text-area/text-area-3 {:style {:color [0 0 0 255]
                                  :font  regular-font}
                          :text text
                          :on-change on-change
                          :validate-new-text validate-new-text}])

(defn text-editor [text on-text-change]
  (box (layouts/with-minimum-size 300 nil
         (bare-text-editor text (fn [old-state new-state]
                                  (when (not= (:text new-state) (:text old-state))
                                    (on-text-change (:text new-state)))
                                  new-state)))))

(defn parse-integer [string]
  (try (Integer/parseInt string)
       (catch Exception e
         nil)))

(defn number-editor-keyboard-event-handler [on-enter on-escape node event]
  (when (and (= :descent (:phase event))
             (= :enter (:key event)))
    (on-enter)
    nil)

  (when (and (= :descent (:phase event))
             (= :escape (:key event)))
    (on-escape)
    nil)
  event)

(defn number-editor [given-number _on-change!]
  (let [state-atom (dependable-atom/atom {:number given-number
                                          :given-number given-number})]
    (fn [given-number on-change!]
      (let [state @state-atom]
        (when (not (= given-number (:given-number state)))
          (swap! state-atom
                 assoc
                 :number given-number
                 :given-number given-number))

        (-> (box (layouts/with-minimum-size 80 nil
                   (bare-text-editor (str (:number state))
                                     (fn [old-state new-state]
                                       (let [text-changed? (not= (:text new-state)
                                                                 (:text old-state))
                                             new-integer (parse-integer (:text new-state))]

                                         (cond (and text-changed?
                                                    (empty? (:text new-state)))
                                               (do (swap! state-atom assoc :number nil)
                                                   new-state)

                                               (and text-changed?
                                                    new-integer)
                                               (do (swap! state-atom assoc :number new-integer)
                                                   new-state)

                                               (and text-changed?
                                                    (not new-integer))
                                               old-state

                                               :else
                                               new-state)))))
                 {:fill-color (when (not (= given-number (:number state)))
                                [240 240 255 255])})
            (assoc :keyboard-event-handler [number-editor-keyboard-event-handler
                                            (fn []
                                              (if (nil? (:number state))
                                                (swap! state-atom assoc :number given-number)
                                                (on-change! (:number state))))

                                            (fn []
                                              (swap! state-atom assoc :number given-number))]))))))


(defn hor [margin & children]
  (apply layouts/horizontally-2 {:margin margin}
         children))

(defn chor [margin & children]
  (apply layouts/horizontally-2 {:margin margin :centered true}
         children))

(defn ver [margin & children]
  (apply layouts/vertically-2 {:margin margin}
         children))

(defn text-property-editor [on-change entity path]
  (text-editor (or (get-in entity
                           path)
                   "")
               #(on-change (assoc-in entity path %))))

(defn number-property-editor [on-change entity path]
  [number-editor
   (or (get-in entity
               path)
       "")
   #(do (prn 'number-property-editor-on-change %) ;; TODO: remove-me
        (on-change (assoc-in entity path %)))])

(defn event-view [event on-change on-remove on-continue]
  (hor 50
       (layouts/with-maximum-size 600 nil
         (ver 0
              (text-property-editor on-change event [:task-name])
              (text-property-editor on-change event [:description])))
       (chor 50
             (hor 0
                  (number-property-editor on-change event [:start-time :hour])
                  (number-property-editor on-change event [:start-time :minute]))
             (hor 20
                  (button "remove" on-remove)
                  (button "continue" on-continue)))))

(defn time-now []
  (let [now (clj-time/now)]
    {:hour (clj-time/hour now)
     :minute (clj-time/minute now)}))

(defn day-view [day on-change]
  (ver 50
       (hor 0
            (number-property-editor on-change day [:date :year])
            (number-property-editor on-change day [:date :month])
            (number-property-editor on-change day [:date :day]))
       #_(text (:date (pr-str day)))
       (for [[index event] (map-indexed vector (:events day))]
         (event-view event
                     (fn [new-event]
                       (on-change (update day :events assoc index new-event)))
                     (fn []
                       (on-change (assoc day :events (vec (concat (take index (:events day))
                                                                  (drop (inc index) (:events day)))))))
                     (fn []
                       (on-change (update day :events conj (assoc event
                                                                  :start-time (time-now)))))))
       (button "add"
               (fn []
                 (on-change (update day :events conj {:task-name ""
                                                      :description ""
                                                      :start-time (time-now)
                                                      :work? true}))))

       (chor 20
             (text "end time")
             (hor 0
                  (number-property-editor on-change day [:end-time :hour])
                  (number-property-editor on-change day [:end-time :minute])))))

(defn day-selector-mouse-event-handler [on-change index node event]
  (when (= :mouse-clicked (:type event))
    (on-change index))
  event)

(defn day-selector [state on-change]
  (let [days-with-index (for [[index day] (map-indexed vector (:days state))]
                          (assoc day :index index))
        days-by-year (medley/map-vals (fn [days-in-year]
                                        (group-by (fn [day]
                                                    (-> day :date :month))
                                                  days-in-year))
                                      (group-by #(-> % :date :year)
                                                days-with-index))]
    (ver 0
         (for [year (sort (keys days-by-year))]
           (ver 0
                (text (str year) bold-font)
                (chor 10
                      (for [month (sort (keys (get days-by-year year)))]
                        (chor 10
                              (text (str month) bold-font)
                              (chor 5
                                    (for [day (get-in days-by-year [year month])]
                                      (assoc (if (= (:selected-day-index state)
                                                    (:index day))
                                               (focused (text (str (-> day :date :day))))
                                               (text (str (-> day :date :day))))
                                             :mouse-event-handler [day-selector-mouse-event-handler on-change (:index day)])))))))))))

(defn base-view [state-atom]
  (let [state @state-atom]
    (layouts/superimpose (visuals/rectangle-2 :fill-color [255 255 255 255])
                         (layouts/with-margins 30 30 30 30
                           (ver 40
                                (hor 10 (button "add day"
                                                (fn [] (swap! state-atom
                                                              update
                                                              :days
                                                              conj
                                                              (let [now (clj-time/now)]
                                                                {:date {:year (clj-time/year now)
                                                                        :month (clj-time/month now)
                                                                        :day (clj-time/day now)}
                                                                 :end-time {:hour 16, :minute 0}
                                                                 :events
                                                                 [{:task-name ""
                                                                   :description ""
                                                                   :start-time {:hour (clj-time/hour now)
                                                                                :minute (clj-time/minute now)}
                                                                   :work? true}]})))))
                                (day-selector state (fn [new-day-index]
                                                      (swap! state-atom assoc :selected-day-index new-day-index)))
                                (day-view (get (:days state)
                                               (:selected-day-index state))
                                          (fn [new-day]
                                            (swap! state-atom
                                                   update
                                                   :days
                                                   (fn [days]
                                                     (assoc days (:selected-day-index state) new-day)))))
                                #_(text (pr-str @state-atom))

                                (text (pr-str (report-day (get (:days state)
                                                               (:selected-day-index state)))))
                                #_(text (pr-str (report-day-2 (get (:days state)
                                                                   (:selected-day-index state))))))))))

(defn ui []
  (let [state-atom (dependable-atom/atom {:days [sample-day
                                                 (assoc sample-day :date {:year 2021 :month 3 :day 1})]
                                          :selected-day-index 0})]
    (fn []
      @animation/state-atom ;; to refresh the view on :redraw events
      (#'base-view state-atom))))

(defonce event-channel-atom (atom nil))

(defn start []
  (reset! event-channel-atom (application/start-window #'ui :on-exit #(reset! event-channel-atom nil))))

(when @event-channel-atom
  (async/>!! @event-channel-atom
             {:type :redraw}))
