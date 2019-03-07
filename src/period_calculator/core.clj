(ns period-calculator.core
  (:gen-class)
  (:require [clojure.xml :as xml]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.periodic :as p]
            [clj-time.predicates :as pr]))

(defn parse-bool [x]
  (Boolean/parseBoolean x))

(defn parse-int [x]
  (Integer/parseInt x))

(defn xml->open-periods [x]
  (for [c     (xml-seq x)
        :when (= :OperationSchedule (:tag c))]
    (let [attrs          (:attrs c)
          [name & times] (:content c)
          name           (first (:content name))]
      {:rid   (:RID attrs)
       :start (some-> (:Start attrs) f/parse-local-date)
       :end   (some-> (:End attrs) f/parse-local-date)
       :type  (some-> (:Type attrs) parse-int)
       :name  name
       :times (mapv
               (comp
                (fn [x]
                  {:rid   (:RID x)
                   :start (some-> (:Start x) f/parse-local-time)
                   :end   (some-> (:End x) f/parse-local-time)
                   :mon   (some-> (:Mon x) parse-bool)
                   :tue   (some-> (:Tue x) parse-bool)
                   :wed   (some-> (:Weds x) parse-bool)
                   :thu   (some-> (:Thur x) parse-bool)
                   :fri   (some-> (:Fri x) parse-bool)
                   :sat   (some-> (:Sat x) parse-bool)
                   :sun   (some-> (:Sun x) parse-bool)})
                :attrs)
               times)})))

(defn gen-dates [open-period]
  (p/periodic-seq
   (:start open-period)
   (t/plus (:end open-period) (t/days 1))
   (t/days 1)))

(defn day-is-open [date time]
  (let [preds [[pr/monday? :mon]
               [pr/tuesday? :tue]
               [pr/wednesday? :wed]
               [pr/thursday? :thu]
               [pr/friday? :fri]
               [pr/saturday? :sat]
               [pr/sunday? :sun]]]
    (some?
     (some
      (fn [[f k]] (and (f date) (k time)))
      preds))))

(defn generate-date-times [open-period]
  (for [date  (gen-dates open-period)
        time  (:times open-period)
        :when (day-is-open date time)]
    (let [start-date-time (.toLocalDateTime date (:start time))
          end-date-time   (.toLocalDateTime date (:end time))]
      [start-date-time end-date-time])))

(defn within? [date [start end]]
  (t/within?
   (t/interval
    (.toDateTime start)
    (.toDateTime end))
   date))

(defn is-currently-open? [times date]
  (not (empty? (filter #(within? date %) times))))

(defn -main
  [& args]

  (time
   (do
     (println "downloading data...")

     (def url "https://gist.githubusercontent.com/RudiThoeni/b15a56c51257c1f98570d6af2a46c50d/raw/8bd23d03d3c661adec3ec80d987b8da06d94db3a/gistfile1.txt")

     (def xml (xml/parse url))

     (println :finished)))

  (println)

  (time
   (do
     (println "parsing data...")

     (def open-periods
       (xml->open-periods xml))

     (def date-times
       (mapcat generate-date-times open-periods))

     (println :finished)))

  (println)

  (time
   (do
     (println
      "2018-01-30T10:10"
      (is-currently-open?
       date-times
       (t/date-time 2018 01 30 10 10)))

     (println
      "now"
      (is-currently-open?
       date-times
       (t/now))))))
