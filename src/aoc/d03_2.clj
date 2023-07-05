(ns aoc.d03-2
  (:require [clojure.string :as str]))

(defn- make-column-triads [triad-of-lines]
  (apply map vector triad-of-lines))

(defn- get-data [file-name]
  (->> (slurp file-name)
       (str/split-lines)
       (map str/trim)
       (map #(map parse-long (str/split % #"\s+")))
       (partition 3)
       (map make-column-triads)
       (flatten)
       (partition 3)))

(defn- triangle [triad]
  (let [max-val (apply max triad)
        other (let [[a b] (split-with (partial not= max-val) triad)]
                (concat a (rest b)))]
    (when (< max-val (apply + other)) true)))

(defn -main []
  (->> (get-data "data/input03.txt")
       (keep triangle)
       (count)))

(comment
  (-main))
