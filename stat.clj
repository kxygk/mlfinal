(ns stat
  "Just some statistics on the training data.."
  (:use ible       
        clojure.data.csv
        clojure.math)
  (:require csv
            quickthing
            [tech.v3.dataset :as ds]
            [tech.v3.dataset.column-filters :as cf]))

(defn
  log2 [n]
  (/ (log n)
     (log 2)))

(defn
  sturges-bins
  [data]
  (-> data
      count
      log2
      int
      inc))
      
(defn histogram
  [data-vec]
  (let [filtered (->> data-vec
                      (filter some?))]
    ;;    filtered))
    (let[num-vals  (-> filtered
                       count)  
         min-val   (->> filtered
                        (apply min))
         max-val   (->> filtered
                        (apply max))
         val-range (- max-val
                      min-val)
         ;; number of bins is based on Sturges' formula
         ;; https://en.wikipedia.org/wiki/Histogram#Sturges'_formula
         n-bins    (min (-> filtered
                            set
                            count)
                        (-> filtered
                            sturges-bins))]
      (let [;; make values on the 0-1 range
            normalized (->> filtered
                            (mapv #(- %
                                      min-val))
                            (mapv #(/ %
                                      val-range)))]
        (->> normalized
             (mapv #(* %
                       (dec n-bins))) ;; TODO: check `dec`
             (mapv int))))))
#_
(-> training
      (get  "Danceability" )
      histogram
      frequencies)
;; => {0 2141,
;;     7 901,
;;     1 1689,
;;     4 1477,
;;     13 145,
;;     6 1028,
;;     3 1538,
;;     12 230,
;;     2 1706,
;;     11 395,
;;     9 709,
;;     5 1289,
;;     14 4,
;;     10 521,
;;     8 842}


(def numeric-columns
  (into []
        (-> training
            (ds/descriptive-stats {:stat-names [:col-name
                                                :datatype
                                                :min
                                                :mean
                                                :max
                                                :standard-deviation]})
            (ds/filter-column :datatype #(= % :float64))
            (get :col-name))))

(->> numeric-columns
    (mapv (fn [feature-str]
      (spit (str feature-str
                 ".svg")
            (-> training
                (get feature-str)
                histogram
                frequencies
                (quickthing/histogram 200
                                      60
                                      10)
                (quickthing/svg-wrap [200
                                      60])
                quickthing/svg2xml)))))

#_
(-> training
    (get  "Liveness"))


(def training (-> "train.csv"
                  ds/->dataset))



(def testing (-> "test.csv"
                  ds/->dataset))
(-> training
    (ds/descriptive-stats {:stat-names [:col-name
                                        :datatype
                                        :min
                                        :mean
                                        :max
                                        :standard-deviation]}))
;; => train.csv: descriptive-stats [29 6]:
;;    |        :col-name | :datatype |           :min |           :mean |           :max | :standard-deviation |
;;    |------------------|-----------|---------------:|----------------:|---------------:|--------------------:|
;;    |     Danceability |  :float64 |  0.0000000E+00 |  4.58602213E+00 | 9.00000000E+00 |      2.85898047E+00 |
;;    |           Energy |  :float64 |  8.3654270E-15 |  3.35839161E-01 | 1.00000000E+00 |      2.42682490E-01 |
;;    |              Key |  :float64 |  0.0000000E+00 |  5.23160351E+00 | 1.00000000E+01 |      3.45698563E+00 |
;;    |         Loudness |  :float64 | -4.6251000E+01 | -7.56137045E+00 | 8.29000000E-01 |      4.54094367E+00 |
;;    |      Speechiness |  :float64 |  0.0000000E+00 |  9.54616104E-02 | 9.64000000E-01 |      1.03067790E-01 |
;;    |     Acousticness |  :float64 |  1.3676310E-18 |  1.14840436E-01 | 9.88047936E-01 |      2.21515989E-01 |
;;    | Instrumentalness |  :float64 |  0.0000000E+00 |  5.49337142E-02 | 1.00000000E+00 |      1.91391947E-01 |
;;    |         Liveness |  :float64 |  3.0486250E-06 |  3.37454170E-02 | 1.00000000E+00 |      1.14862495E-01 |
;;    |          Valence |  :float64 |  0.0000000E+00 |  5.25841770E-01 | 9.93000000E-01 |      2.44911882E-01 |
;;    |            Tempo |  :float64 |  0.0000000E+00 |  1.20937819E+02 | 2.43372000E+02 |      2.96652253E+01 |
;;    |      Duration_ms |  :float64 |  3.0985000E+04 |  2.22830746E+05 | 4.58148300E+06 |      1.21165872E+05 |
;;    |            Views |  :float64 |  2.6000000E+01 |  8.79109559E+07 | 5.77379841E+09 |      2.52391810E+08 |
;;    |            Likes |  :float64 |  0.0000000E+00 |  6.48506362E+05 | 4.01476180E+07 |      1.72615677E+06 |
;;    |           Stream |  :float64 |  6.5740000E+03 |  1.29042648E+08 | 3.38652029E+09 |      2.41768149E+08 |
;;    |       Album_type |   :string |                |                 |                |                     |
;;    |         Licensed |   :string |                |                 |                |                     |
;;    |   official_video |   :string |                |                 |                |                     |
;;    |               id |    :int16 |  0.0000000E+00 |  8.58450000E+03 | 1.71690000E+04 |      4.95669640E+03 |
;;    |            Track |   :string |                |                 |                |                     |
;;    |            Album |   :string |                |                 |                |                     |
;;    |              Uri |   :string |                |                 |                |                     |
;;    |      Url_spotify |   :string |                |                 |                |                     |
;;    |      Url_youtube |   :string |                |                 |                |                     |
;;    |         Comments |  :float64 |  0.0000000E+00 |  2.81032121E+04 | 1.60831380E+07 |      2.14292202E+05 |
;;    |      Description |   :string |                |                 |                |                     |
;;    |            Title |   :string |                |                 |                |                     |
;;    |          Channel |   :string |                |                 |                |                     |
;;    |         Composer |   :string |                |                 |                |                     |
;;    |           Artist |   :string |                |                 |                |                     |

(def numeric-columns
  (into []
        (-> training
            (ds/descriptive-stats {:stat-names [:col-name
                                                :datatype
                                                :min
                                                :mean
                                                :max
                                                :standard-deviation]})
            (ds/filter-column :datatype #(= % :float64))
            (get :col-name))))
;; => ["Danceability"
;;     "Energy"
;;     "Key"
;;     "Loudness"
;;     "Speechiness"
;;     "Acousticness"
;;     "Instrumentalness"
;;     "Liveness"
;;     "Valence"
;;     "Tempo"
;;     "Duration_ms"
;;     "Views"
;;     "Likes"
;;     "Stream"
;;     "Comments"]

(defn
  column-average
  [column]
  (let [values (->> column
                   (filter some?))]
    (/ (->> values
            (reduce +))
       (-> values
           count))))
#_
(column-average [1
                 2
                 3
                 4])
#_
(column-average (->> training
                     ds/columns first))
#_
(mapv column-average
      (->> training
           cf/numeric
           ds/columns))

(defn
  add-index
  "Adds a INDEX column to keep track of the original row
  So that after a bunch of transformation and filtering
  you can return back to your original table and find the data"
  [dataset]
  (-> dataset
      (ds/add-column
        (->> dataset
             ds/row-count
             range
             (ds/new-column "INDEX")))))
#_
(-> training
    cf/numeric
    add-index)

(defn
  normalize-dataset
  "Make all the columns on the 0.0-1.0 range
  Column called INDEX is left untouched"
  [dataset]
  (let [col-mins (->> dataset
                      cf/numeric
                      ds/columns
                      (mapv (fn [column]
                              ;;       (println column)
                              (apply min
                                     (->> column                   
                                          (filter some?))))))
        col-maxs (->> dataset
                      cf/numeric
                      ds/columns
                      (mapv (fn [column]
                              ;;       (println column)
                              (apply max
                                     (->> column                   
                                          (filter some?))))))]
    (-> dataset
        cf/numeric
        (ds/row-map (fn [row]
                      (into {}
                            (mapv (fn [[elem-key
                                        elem-val]
                                       min-val
                                       max-val]
                                    (if (= elem-key
                                           "INDEX")
                                      [elem-key ;; don't normalize index :)
                                       elem-val]
                                      (if (nil? elem-val)
                                        [elem-key
                                         nil]
                                        [elem-key
                                         (/ (- elem-val
                                               min-val)
                                            max-val)])))
                                  row
                                  col-mins
                                    col-maxs)))))))
#_
(-> training
    add-index
    normalize-dataset)

(defn
  manhattan
  "Calculate the Manhattan distance (L1 norm)
  between two row values
  ..
  If either coordinate is `nil`
  then the coordinate is not considered
  ..
  Final result is normalized by the number of dimensionsd"
  [row1
   row2]
  (let [diff (->> (mapv (fn [[key1 val1]
                             [key2 val2]]
                          (if (or (nil? val1)
                                  (nil? val2)
                                  (= key1
                                     "INDEX")
                                  (= key2 ;; redundant check
                                     "INDEX"))
                            nil
                            (abs (- val1
                                    val2))))
                        row1
                        row2)
                  (filter some?))]
    (/ (->> diff
            (reduce +))
       (count diff))))
#_
(->> 10
     (range 1)
     (mapv #(let [data (-> training
                           cf/numeric
                           normalize-dataset
                           add-index
                           (ds/head 10))
                  row1 (-> data
                           (ds/row-at 0))
                  row2 (-> data
                           (ds/row-at %))]
              (manhattan row1
                         row2))))
         
(defn
  estimate-coord
  "Given a coordinate, specified by a `row-idx` and `col-name`
  Use an L1 Manhattan norm to equivalent value in
  the closest point/row which has a non-nil"
  [dataset
   normalized-data
   row-idx
   col-name]
  (let [#_#_normalized-data (-> dataset
                            cf/numeric
                            normalize-dataset
                            add-index)] ;; TODO: This is recomputed every time.. :S
    (let [reference-row  (-> normalized-data
                            (ds/row-at row-idx))
          candidate-data (-> normalized-data
                             add-index
                             (ds/drop-missing col-name))]
      ;; TODO: Throw some error if the candidate data is empty?
      (let [distances (->> candidate-data
                           count
                           range
                           (mapv #(let [candidate-row (-> candidate-data
                                                          (ds/row-at %))]
                                    (vector (get candidate-row
                                                 "INDEX")
                                            (manhattan reference-row
                                                       candidate-row)))))]
        (get (->> distances
                  (sort-by second)
                  first
                  first
                  (ds/row-at dataset))
             col-name)))))
(estimate-coord training
                (-> training
                            cf/numeric
                            normalize-dataset
                            add-index)
                1
                "Loudness")


(defn
  manhattan-nullfill
  "Fill in null values with value from the closest
  neighbouring point that has a value for that column.
  Distance to points is calculates using the L1 Manhattan norm
  divided by the number of common non-nil dimensions."
  [dataset]
  (let [clean-ds (-> dataset
                     cf/numeric
                     add-index)
        normalized-data (-> clean-ds
                            cf/numeric
                            normalize-dataset
                            add-index)]
  (-> clean-ds
      (ds/row-map (fn [row]
                    (->> row ;; run across each value in each row
                         (mapv (fn fix-if-nil
                                 [[key value]]
                                 (if (nil? value)
                                   [key
                                    (estimate-coord clean-ds ;; use original data
                                                    normalized-data
                                                    (get row
                                                         "INDEX")
                                                    key)]
                                   [key value])));; non-nil - no fix
                         (into {})))))))
(-> training
    cf/numeric
    add-index
    manhattan-nullfill
    (ds/write! "no-holes-numeric.csv"))

(let [hole-filled (-> training
                      cf/numeric
                      add-index
                      manhattan-nullfill)]
  (ds/write!
    (-> training
        (ds/add-or-update-column  (ds/column hole-filled   "Energy" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Key" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Loudness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Speechiness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Acousticness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Instrumentalness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Liveness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Valence" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Tempo" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Duration_ms" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Views" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Likes" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Stream" ))
        (ds/add-or-update-column  (ds/column hole-filled   "id" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Comments" )))
    #_(->> hole-filled
           (reduce (fn [fused-ds
                        column]
                     (ds/add-or-update-column fused-ds
                                              column))
                   training))
    "no-holes-all.csv"))

;;;;;; FOR FIXING #O$&#@&$@#&$)(#&$)(&#$*@#)*$
(let [hole-filled (-> training
                      cf/numeric
                      add-index
                      manhattan-nullfill
                      (ds/head 10))]
  ;; (println "LOUDNESS\n"
  ;;          (ds/column hole-filled "Loudness"))
  ;; (println "Hole filled..\n"
  ;;          hole-filled)
  ;; (println "Merged\n"
  (-> training
      (ds/add-or-update-column  (ds/column hole-filled   "Energy" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Key" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Loudness" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Speechiness" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Acousticness" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Instrumentalness" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Liveness" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Valence" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Tempo" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Duration_ms" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Views" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Likes" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Stream" ))
      (ds/add-or-update-column  (ds/column hole-filled   "id" ))
      (ds/add-or-update-column  (ds/column hole-filled   "Comments" )))
#_  (println "WHOLE THANG #$(&)(@&#)*$#@)"
           (->> hole-filled
                (reduce (fn [fused-ds
                             column]
                          (ds/add-or-update-column fused-ds
                                                   column))
                        (-> training
                            (ds/head 10))))))
;;;;;; FOR FIXING #O$&#@&$@#&$)(#&$)(&#$*@#)*$

  
(-> testing
    cf/numeric
    add-index
    manhattan-nullfill
    (ds/write! "no-holes-numeric-testing.csv"))

(let [hole-filled (-> testing
                      cf/numeric
                      add-index
                      manhattan-nullfill)]
  (ds/write!
    (-> testing
        (ds/add-or-update-column  (ds/column hole-filled   "Energy" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Key" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Loudness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Speechiness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Acousticness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Instrumentalness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Liveness" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Valence" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Tempo" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Duration_ms" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Views" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Likes" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Stream" ))
        (ds/add-or-update-column  (ds/column hole-filled   "id" ))
        (ds/add-or-update-column  (ds/column hole-filled   "Comments" )))
    #_
    (->> hole-filled
                  (reduce (fn [fused-ds
                               column]
                            (ds/add-or-update-column fused-ds
                                                     column))
                          testing))
             "no-holes-all-testing.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defn
   distance-squared
  [row1
   row2]
  (reduce
    +
    (mapv (fn [coord1
               coord2]
            (pow (- coord1
                    coord2)
                 2))
          row1
          row2)))
#_
(distance-squared [1 2 3]
                  [2 3 4])


(defn
  fill-nils-in-map-and-normalize
  "Run across and `input-map`
  and if the value is a nil
  then fill it with the value from `averages`
  ..
  Naturally `input-map` and `averages`
  should be the same length.."
  [averages
   input-map]
  (into {}
        (mapv
          (fn [[key
                value]
               average]
            (if (= key
                   "INDEX")
              [key
               value] ;; leave index untouched
              (if (nil? value)
                [key
                 1.0 #_average]
                [key
                 (/ value
                    average)])))
            input-map
            averages)))
#_
(fill-nils-in-map [999 666]
                  (map identity
                       {:hell "world"
                        :testing nil}))

(defn
  fill-nils-with-averages-and-normalize
  "Take a `numeric` dataset and fill the `nil`
  values with each column's average value"
  [dataset]
  (let [averages  (mapv column-average
                       (->> dataset
                            cf/numeric
                            ds/columns))]
    (ds/row-map dataset
                (partial fill-nils-in-map-and-normalize
                         (->> dataset
                              cf/numeric
                              ds/columns
                              (mapv column-average))))))
;; (-> training
;;     (ds/head 10)
;;     cf/numeric
;;     (add-column 
;;     fill-nils-with-averages)


(defn
  closest-val
  "Given a numeric dataset
  a `row-idx` and `col-name` of a `nil`
  Returns a the row that's closest (in Cartesian distance)"
  [dataset
   row-idx
   col-name]
  (let [row-to-fill (-> dataset
                        fill-nils-with-averages-and-normalize
                        (ds/rowvec-at row-idx))
        rest-of-data (-> dataset
                         fill-nils-with-averages-and-normalize
                         (ds/drop-rows [row-idx]))]
    (ds/row-map rest-of-data
                (fn [row]
                  {:distance (distance-squared (vals row)
                                               row-to-fill)}))))
(-> training
    (ds/head 10)
    (ds/drop-columns "Danceability")
    (cf/numeric)
    (closest-val 1
                 "Loudness"))




   
