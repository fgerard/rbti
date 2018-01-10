(ns rbti.core
  ;(:gen-class)
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]
            [clara.tools.tracing :as tracing]))

;(def types #{:Brix :UpH :SpH :Conductivity :CellDebris :NN :AN})

(def gender #{:Male :Female})

(def religion #{:protestant :catolic :hindu :budhist :muslim :none :other})

(def skin-color #{:black :light-brawn :brown :yellow :white })

(defrecord Measure [id timestamp Weight Brix UpH SpH SaltReading CellDebris NN AN])

(defrecord Client [id name gender birth height color occupation religion date-test1])

(defrecord Brix [id timestamp range ideal value inWRange])
(defrecord CorrectedBrix [id timestamp range ideal value inWRange])
(defrecord UpH [id timestamp range ideal value inWRange])
(defrecord SpH [id timestamp range ideal value inWRange])
(defrecord pHavg [id timestamp range ideal avg inWRange])
(defrecord Conductivity [id timestamp range ideal value inWRange])
(defrecord CellDebris [id timestamp range ideal value inWRange])
(defrecord NN [id timestamp range ideal value inWRange])
(defrecord AN [id timestamp range ideal value inWRange])
(defrecord Urea [id timestamp range ideal value inWRange])

(defrecord ReamsEq [id timestamp brix uph sph conductivity cell-debris nn an])

(def DebrisRanges [0.04 1 2 3 4 5 6 7])

(defn timestamp-factory [d]
  (let [sdf (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm")]
    (.parse sdf d)))

(defn date [d]
  (let [sdf (java.text.SimpleDateFormat. "yyyy-MM-dd")]
    (.parse sdf d)))

(println "VER 1")

(def initial-data [(->Client 1 "Minoja" :Female (date "1964-04-26") 1.61 :light-brown "home" :protestant #inst "2017-11-29T14:03:00.000-06:00")
                   (->Measure 1 #inst "2017-11-29T14:03:00.000-06:00" 60 5 7.5 6.5 0.89 4 7 8)
                   (->Measure 1 #inst "2017-12-01T10:00:00.000-06:00" 62 3.8 6.9 6.7 0.7 4 7 7)


                   (->Client 2 "PersonajeA" :Female (date "1964-04-26") 1.75 2 "home" :protestant #inst "2017-11-10T13:35:00.000-06:00")
                   (->Measure 2 #inst "2017-11-10T13:35:00.000-06:00" 80 4.2 5.7 7.2 0.7 7 7 8)

                   ;(->Measure 2 #inst "2017-11-17T13:35:00.000-06:00" 4.2 5.7 7.2 40.0 4 7 8)
                   ])

(defn range-selector [t m]
  t)

(defmulti create-range range-selector)

(defmethod create-range :default [_ _]
  nil)

(defmethod create-range :Brix [type {id :id timestamp :timestamp value :Brix}]
  (cond
    (and (>= value 1.2) (< value 2))
    (->Brix id timestamp :A (= value 1.5) value true)

    (and (>= value 2) (< value 8.5))
    (->Brix id timestamp :B false value false)

    (and (>= value 8.5) (<= value 13))
    (->Brix id timestamp :C false value false)

    (and (>= value 0.6) (< value 1.2))
    (->Brix id timestamp :D false value false)

    (and (>= value 0) (< value 0.6))
    (->Brix id timestamp :E false value false)

    :OTHERWISE
    (->Brix id timestamp :outOfRange false value false)))

(defmethod create-range :UpH [type {id :id timestamp :timestamp value :UpH}]
  (cond
    (and (>= value 6.30) (< value 6.50))
    (->UpH id timestamp :A (= value 6.40) value true)

    (and (>= value 6.50) (< value 7.21))
    (->UpH id timestamp :B false value false)

    (and (>= value 7.21) (<= value 8) false)
    (->UpH id timestamp :C false value false)

    (and (>= value 5.2) (< value 6.3))
    (->UpH id timestamp :D false value false)

    (and (>= value 4.8) (< value 5.2))
    (->UpH id timestamp :E false value false)

    :OTHERWISE
    (->UpH id timestamp :outOfRange false value false)))

(defmethod create-range :SpH [type {id :id timestamp :timestamp value :SpH}]
  (cond
    (and (>= value 6.30) (< value 6.50))
    (->SpH id timestamp :A (= value 6.40) value true)

    (and (>= value 6.50) (< value 7.21))
    (->SpH id timestamp :B false value false)

    (and (>= value 7.21) (<= value 8))
    (->SpH id timestamp :C false value false)

    (and (>= value 5.2) (< value 6.3))
    (->SpH id timestamp :D false value false)

    (and (>= value 4.8) (< value 5.2))
    (->SpH id timestamp :E false value false)

    :OTHERWISE
    (->SpH id timestamp :outOfRange false value false)))

(defmethod create-range :CellDebris [type {id :id timestamp :timestamp value :CellDebris}]
  (let [inWRange (and (>= value 4) (< value 7))]
    (cond
      (and (= value 0.04))
      (->CellDebris id timestamp :A (= value 0.04) value inWRange)

      (and (>= value 1) (< value 5))
      (->CellDebris id timestamp :B false value inWRange)

      (and (>= value 5) (< value 7))
      (->CellDebris id timestamp :C false value inWRange)

      (or (< value 0.04))
      (->CellDebris id timestamp :D false value inWRange)

      (or (>= value 7))
      (->CellDebris id timestamp :E false value inWRange)

      :OTHERWISE
      (->CellDebris id timestamp :outOfRange false value inWRange))))


(defmethod create-range :SaltReading [type {id :id timestamp :timestamp value :SaltReading}]
  (let [value (/ (* value 1000) (* 4 100))]
    (cond
      (and (>= value 6) (<= value 7))
      (->Conductivity id timestamp :A (= value 6.50) value true)

      (and (> value 7) (< value 35))
      (->Conductivity id timestamp :B false value true)

      (and (>= value 35) (<= value 80))
      (->Conductivity id timestamp :C false value true)

      (and (>= value 5) (< value 6))
      (->Conductivity id timestamp :D false value true)

      (and (>= value 0) (< value 5))
      (->Conductivity id timestamp :E false value true)

      :OTHERWISE
      (->Conductivity id timestamp :outOfRange false value true))))


(defn create-urea-fact [id height date-test1 {:keys [timestamp Weight SaltReading NN AN] :as measure}] ;id timestamp valueNN valueAN valueConductivity date-test1
  (let [Conductivity (* SaltReading 40)
        conductivityFactor (if (and (>= Conductivity 35) (= date-test1 timestamp)) 2 0)
        total (+ NN AN conductivityFactor)
        bmi (/ Weight (* height height))
        overWeight (>= bmi 28)
        inWRange (if overWeight
                   (and (>= total 15) (< total 20))
                   (and (>= total 12) (< total 20)))]
    (cond
      (and (>= total 6) (< total 7))
      [(->Urea id timestamp :A (and (= 3 NN) (= 3 AN)) total inWRange)
       (->NN id timestamp :A (and (= 3 NN) (= 3 AN)) NN inWRange)
       (->AN id timestamp :A (and (= 3 NN) (= 3 AN)) AN inWRange)]

      (and (>= total 7) (< total 19))
      [
       (->Urea id timestamp :B false total inWRange)
       (->NN id timestamp :B false NN inWRange)
       (->AN id timestamp :B false AN inWRange)]

      (and (>= total 19) (< total 30))
      [
       (->Urea id timestamp :C false total inWRange)
       (->NN id timestamp :C false NN inWRange)
       (->AN id timestamp :C false AN inWRange)]

      (and (>= total 5) (< total 6))
      [
       (->Urea id timestamp :D false total inWRange)
       (->NN id timestamp :D false NN inWRange)
       (->AN id timestamp :D false AN inWRange)]

      (and (>= total 0) (< total 5))
      [
       (->Urea id timestamp :E false total inWRange)
       (->NN id timestamp :E false NN inWRange)
       (->AN id timestamp :E false AN inWRange)]

      :OTHERWISE
      [
       (->Urea id timestamp :outOfRange false total inWRange)
       (->NN id timestamp :outOfRange false NN inWRange)
       (->AN id timestamp :outOfRange false AN inWRange)])))

(defn create-pHavg [{:keys [id timestamp SpH UpH] :as measure}]
  (let [avg (/ (+ UpH (* 2 SpH)) 3)]
    (cond
      (and (>= avg 6.30) (< avg 6.50))
      (->pHavg id timestamp :A (and (= SpH 6.40)
                                    (= UpH 6.40)) avg true)

      (and (>= avg 6.50) (< avg 7.21))
      (->pHavg id timestamp :B false avg false)

      (and (>= avg 7.21) (<= avg 8))
      (->pHavg id timestamp :C false avg false)

      (and (>= avg 5.2) (< avg 6.3))
      (->pHavg id timestamp :D false avg false)

      (and (>= avg 4.8) (< avg 5.2))
      (->pHavg id timestamp :E false avg false)

      :OTHERWISE
      (->pHavg id timestamp :outOfRange false avg false))))

(defn create-correctedBrix [{:keys [id timestamp Brix SaltReading] :as measure}]
  (let [Conductivity (* SaltReading 40)
        C (if (> Conductivity 6) (- Conductivity 6) 0)
        value (- Brix (* 0.054 C))]
    (cond
      (and (>= value 1.2) (< value 2))
      (->CorrectedBrix id timestamp :A (= value 1.5) value true)

      (and (>= value 2) (< value 8.5))
      (->CorrectedBrix id timestamp :B false value false)

      (and (>= value 8.5) (<= value 13))
      (->CorrectedBrix id timestamp :C false value false)

      (and (>= value 0.6) (< value 1.2))
      (->CorrectedBrix id timestamp :D false value false)

      (and (>= value 0) (< value 0.6))
      (->CorrectedBrix id timestamp :E false value false)

      :OTHERWISE
      (->CorrectedBrix id timestamp :outOfRange false value false))))

(defrule classify-simple
  [?measure <- Measure]
  =>
  (doseq [type #{:Brix :UpH :SpH :SaltReading :CellDebris}]
    (when-let [new-fact (create-range type ?measure)]
      (insert! new-fact))))

(defrule clasify-ureas
  [?m1 <- Measure (= ?id id)]
  [Client  (= ?id id) (= ?height height) (= ?date-test1 date-test1)]
  =>
  (when-let [new-facts (create-urea-fact ?id ?height ?date-test1 ?m1)]
    (doseq [fact new-facts]
      (insert! fact))))

(defrule clasify-pHavg
  [?measure <- Measure]
  =>
  (when-let [new-fact (create-pHavg ?measure)]
    (insert! new-fact)))

(defrule clasify-correctedBrix
  [?m1 <- Measure]
  =>
  (when-let [new-fact (create-correctedBrix ?m1)]
    (insert! new-fact)))

(defrule compute-ReamsEq
  [Brix (= ?id id) (= ?timestamp timestamp) (= ?brix value)]
  [UpH (= ?id id) (= ?timestamp timestamp) (= ?uph value)]
  [SpH (= ?id id) (= ?timestamp timestamp) (= ?sph value)]
  [Conductivity (= ?id id) (= ?timestamp timestamp) (= ?conductivity value)]
  [CellDebris (= ?id id) (= ?timestamp timestamp) (= ?cell-debris value)]
  [NN (= ?id id) (= ?timestamp timestamp) (= ?nn value)]
  [AN (= ?id id) (= ?timestamp timestamp) (= ?an value)]
  =>
  (insert!
   (->ReamsEq ?id ?timestamp ?brix ?uph ?sph ?conductivity ?cell-debris ?nn ?an)))

(defquery get-Brix []
  [?Brix <- Brix])
(defquery get-CorrectedBrix []
  [?CorrectedBrix <- CorrectedBrix])
(defquery get-UpH []
  [?UpH <- UpH])
(defquery get-SpH []
  [?SpH <- SpH])
(defquery get-pHavg []
  [?pHavg <- pHavg])
(defquery get-Conductivity []
  [?Conductivity <- Conductivity])
(defquery get-CellDebris []
  [?CellDebris <- CellDebris])
(defquery get-NN []
  [?NN <- NN])
(defquery get-AN []
  [?AN <- AN])
(defquery get-Urea []
  [?Urea <- Urea])
(defquery get-ReamsEq []
  [?ReamsEq <- ReamsEq])

(defquery client-ReamsEq [:?id]
  [?ReamsEq <- ReamsEq (= ?id id) (= ?timestamp timestamp)])

(comment

 (println "***** " (class get-Urea))
 (let [S (reduce
          (fn [session fact]
            (println (pr-str fact))
            (insert session fact))
          (mk-session)
          initial-data)
       Q (fire-rules S)]
   (dorun
     (map
      #(clojure.pprint/pprint (query Q %))
      [;get-Brix
       ;get-CorrectedBrix
       ;get-UpH
       ;get-SpH
       ;get-pHavg
       ;get-Conductivity
       ;get-NN
       ;get-AN
       ;get-Urea
       get-ReamsEq
       ]))))

(defn get-result [session & queries]
  (mapv
   #(query session %)
   queries))

(defn -main [& args]
  (clojure.pprint/pprint (as->
                          (mk-session 'rbti.core :cache true) $
                          (apply insert $ initial-data)
                          (fire-rules $)
                          (get-result $
                                      ;get-Brix
                                      ;get-CorrectedBrix
                                      ;get-UpH
                                      ;get-SpH
                                      ;get-pHavg
                                      ;get-Conductivity
                                      get-CellDebris
                                      ;get-NN
                                      ;get-AN
                                      ;get-Urea
                                      ;get-ReamsEq
                                      )))

  (clojure.pprint/pprint (as-> (mk-session 'rbti.core :cache true) $
                               (apply insert $ initial-data)
                               (fire-rules $)
                               (query $ client-ReamsEq :?id 2)
                               (map :?ReamsEq $)))


  (println "Ready"))
