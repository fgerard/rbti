(ns rbti.core
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]
            [clara.tools.tracing :as tracing]))

(def types #{:Brix :UpH :SpH :Conductivity :CellDebris :NN :AN})

(def gender #{:Male :Female})

(def religion #{:protestant :catolic :hindu :budhist :muslim :none :other})

(defrecord Measure [id timestamp type value])

(defrecord Client [id name gender birth weight color occupation religion date-test1])

(defrecord Brix [id timestamp range ideal])
(defrecord CorrectedBrix [id timestamp range ideal value])
(defrecord UpH [id timestamp range ideal])
(defrecord SpH [id timestamp range ideal])
(defrecord pHavg [id timestamp range ideal avg])
(defrecord Conductivity [id timestamp range ideal])
(defrecord CellDebris [id timestamp range ideal])
(defrecord Urea [id timestamp range ideal value])

(def DebrisRanges [".04M" "1M" "2M" "3M" "4M" "4M+" "4M++" "4M+++"])

(defn timestamp-factory [d]
  (let [sdf (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm")]
    (.parse sdf d)))

(defn date [d]
  (let [sdf (java.text.SimpleDateFormat. "yyyy-MM-dd")]
    (.parse sdf d)))

(println "VER 1")

(def initial-data [(->Client 1 "Minoja" :Female (date "1964-04-26") 132 2 "home" :protestant #inst "2017-11-10T13:35:00.000-00:00")
                   (->Measure 1 #inst "2017-11-10T13:35:00.000-00:00" :Brix 4.2)
                   (->Measure 1 #inst "2017-11-10T13:35:00.000-00:00" :UpH 5.7)
                   (->Measure 1 #inst "2017-11-10T13:35:00.000-00:00" :SpH 7.2)
                   (->Measure 1 #inst "2017-11-10T13:35:00.000-00:00" :Conductivity 26.8)
                   (->Measure 1 #inst "2017-11-10T13:35:00.000-00:00" :CellDebris "4M")
                   (->Measure 1 #inst "2017-11-10T13:35:00.000-00:00" :NN 7)
                   (->Measure 1 #inst "2017-11-10T13:35:00.000-00:00" :AN 8)

                   (->Client 2 "PersonajeA" :Female (date "1964-04-26") 132 2 "home" :protestant #inst "2017-11-10T13:35:00.000-00:00")
                   (->Measure 2 #inst "2017-11-10T13:35:00.000-00:00" :Brix 4.2)
                   (->Measure 2 #inst "2017-11-10T13:35:00.000-00:00" :UpH 5.7)
                   (->Measure 2 #inst "2017-11-10T13:35:00.000-00:00" :SpH 7.2)
                   (->Measure 2 #inst "2017-11-10T13:35:00.000-00:00" :Conductivity 36.8)
                   (->Measure 2 #inst "2017-11-10T13:35:00.000-00:00" :CellDebris "4M")
                   (->Measure 2 #inst "2017-11-10T13:35:00.000-00:00" :NN 7)
                   (->Measure 2 #inst "2017-11-10T13:35:00.000-00:00" :AN 8)

                   (->Measure 2 #inst "2017-11-17T13:35:00.000-00:00" :Brix 4.2)
                   (->Measure 2 #inst "2017-11-17T13:35:00.000-00:00" :UpH 5.7)
                   (->Measure 2 #inst "2017-11-17T13:35:00.000-00:00" :SpH 7.2)
                   (->Measure 2 #inst "2017-11-17T13:35:00.000-00:00" :Conductivity 36.8)
                   (->Measure 2 #inst "2017-11-17T13:35:00.000-00:00" :CellDebris "4M")
                   (->Measure 2 #inst "2017-11-17T13:35:00.000-00:00" :NN 7)
                   (->Measure 2 #inst "2017-11-17T13:35:00.000-00:00" :AN 8)
                   ])

(defmulti create-range :type :default :default)

(defmethod create-range :default [_]
  )

(defmethod create-range :Brix [{:keys [id timestamp value]}]
  (println "BRIX timestamp: " timestamp)
  (cond
    (and (>= value 1.2) (< value 2))
    (->Brix id timestamp :A (= value 1.5))

    (and (>= value 2) (< value 8.5))
    (->Brix id timestamp :B false)

    (and (>= value 8.5) (<= value 13))
    (->Brix id timestamp :C false)

    (and (>= value 0.6) (< value 1.2))
    (->Brix id timestamp :D false)

    (and (>= value 0) (< value 0.6))
    (->Brix id timestamp :E false)

    :OTHERWISE
    (->Brix id timestamp :outOfRange false)))

(defmethod create-range :UpH [{:keys [id timestamp value]}]
  (cond
    (and (>= value 6.30) (< value 6.50))
    (->UpH id timestamp :A (= value 6.40))

    (and (>= value 6.50) (< value 7.21))
    (->UpH id timestamp :B false)

    (and (>= value 7.21) (<= value 8))
    (->UpH id timestamp :C false)

    (and (>= value 5.2) (< value 6.3))
    (->UpH id timestamp :D false)

    (and (>= value 4.8) (< value 5.2))
    (->UpH id timestamp :E false)

    :OTHERWISE
    (->UpH id timestamp :outOfRange false)))

(defmethod create-range :SpH [{:keys [id timestamp value]}]
  (cond
    (and (>= value 6.30) (< value 6.50))
    (->SpH id timestamp :A (= value 6.40))

    (and (>= value 6.50) (< value 7.21))
    (->SpH id timestamp :B false)

    (and (>= value 7.21) (<= value 8))
    (->SpH id timestamp :C false)

    (and (>= value 5.2) (< value 6.3))
    (->SpH id timestamp :D false)

    (and (>= value 4.8) (< value 5.2))
    (->SpH id timestamp :E false)

    :OTHERWISE
    (->SpH id timestamp :outOfRange false)))

(defmethod create-range :Conductivity [{:keys [id timestamp value]}]
  (cond
    (and (>= value 6) (<= value 7))
    (->Conductivity id timestamp :A (= value 6.50))

    (and (> value 7) (< value 35))
    (->Conductivity id timestamp :B false)

    (and (>= value 35) (<= value 80))
    (->Conductivity id timestamp :C false)

    (and (>= value 5) (< value 6))
    (->Conductivity id timestamp :D false)

    (and (>= value 0) (< value 5))
    (->Conductivity id timestamp :E false)

    :OTHERWISE
    (->Conductivity id timestamp :outOfRange false)))

(defn create-urea-fact [id timestamp valueNN valueAN valueConductivity date-test1]
  (let [_ (println "iguales: " date-test1 timestamp (= date-test1 timestamp))
        conductivityFactor (if (and (>= valueConductivity 35) (= date-test1 timestamp)) 2 0)
        total (+ valueNN valueAN conductivityFactor)]
    (cond
      (and (>= total 6) (< total 7))
      (->Urea id timestamp :A (= 3 valueNN) (= 3 valueAN) total)

      (and (>= total 7) (< total 19))
      (->Urea id timestamp :B false total)

      (and (>= total 19) (< total 30))
      (->Urea id timestamp :C false total)

      (and (>= total 5) (< total 6))
      (->Urea id timestamp :D false total)

      (and (>= total 0) (< total 5))
      (->Urea id timestamp :E false total)

      :OTHERWISE
      (->Urea id timestamp :outOfRange false total))))

(defn create-pHavg [id timestamp valueSpH valueUpH]
  (let [avg (/ (+ valueUpH (* 2 valueSpH)) 3)]
    (cond
      (and (>= avg 6.30) (< avg 6.50))
      (->pHavg id timestamp :A (and (= valueSpH 6.40)
                                    (= valueUpH 6.40)) avg)

      (and (>= avg 6.50) (< avg 7.21))
      (->pHavg id timestamp :B false avg)

      (and (>= avg 7.21) (<= avg 8))
      (->pHavg id timestamp :C false avg)

      (and (>= avg 5.2) (< avg 6.3))
      (->pHavg id timestamp :D false avg)

      (and (>= avg 4.8) (< avg 5.2))
      (->pHavg id timestamp :E false avg)

      :OTHERWISE
      (->pHavg id timestamp :outOfRange false avg))))

(defn create-correctedBrix [id timestamp valueBrix valueConductivity]
  (let [C (if (> valueConductivity 6) (- valueConductivity 6) 0)
        value (- valueBrix (* 0.054 C))]
    (cond
      (and (>= value 1.2) (< value 2))
      (->CorrectedBrix id timestamp :A (= value 1.5) value)

      (and (>= value 2) (< value 8.5))
      (->CorrectedBrix id timestamp :B false value)

      (and (>= value 8.5) (<= value 13))
      (->CorrectedBrix id timestamp :C false value)

      (and (>= value 0.6) (< value 1.2))
      (->CorrectedBrix id timestamp :D false value)

      (and (>= value 0) (< value 0.6))
      (->CorrectedBrix id timestamp :E false value)

      :OTHERWISE
      (->CorrectedBrix id timestamp :outOfRange false value)))
  )

(defrule classify-simple
  [?measure <- Measure (not (nil? type))]
  =>
  (if-let [new-fact (create-range ?measure)]
    (insert! new-fact)))

(defrule clasify-ureas
  [?m1 <- Measure (= type :NN) (= ?id id) (= ?timestamp timestamp) (= ?valueNN value)]
  [Measure (= type :AN) (= ?id id) (= ?timestamp timestamp) (= ?valueAN value)]
  [Measure (= type :Conductivity) (= ?id id) (= ?timestamp timestamp) (= ?valueConductivity value)]
  [Client  (= ?id id) (= ?date-test1 date-test1)]
  ;[:test (= ?id1 ?id2 ?id3 ?id-client) (= ?timestamp1 ?timestamp2 ?timestamp3)]
  =>
  (println "1.ureas  " (:timestamp ?m1) " " (class ?timestamp) (= ?timestamp (:timestamp ?m1)) " " ?valueNN ?id)
  ;(println "2. " ?timestamp2 (:timestamp ?m2))
  ;(println "3. " ?timestamp3 (:timestamp  ?m3))
  ;(println "4. " ?date-test1)
  (when-let [new-fact (create-urea-fact ?id ?timestamp ?valueNN ?valueAN ?valueConductivity ?date-test1)]
    (println "Dispareó con: " [?id])
    (insert! new-fact)))

(defrule clasify-pHavg
  [?measureSpH <- Measure (= type :SpH) (= ?id id) (= ?timestamp timestamp) (= ?valueSpH value)]
  [?measureUpH <- Measure (= type :UpH) (= ?id id) (= ?timestamp timestamp) (= ?valueUpH value)]
  ;[:test (= ?id1 ?id2) (= ?timestamp1 ?timestamp2)]
  =>
  (if-let [new-fact (create-pHavg ?id ?timestamp ?valueSpH ?valueUpH)]
    (insert! new-fact)))

(defrule clasify-correctedBrix
  [Measure (= type :Brix) (= ?id id) (= ?timestamp timestamp) (= ?valueBrix value)]
  [Measure (= type :Conductivity) (= ?id id) (= ?timestamp timestamp) (= ?valueConductivity value)]
  ;[:test (= ?id1 ?id2) (= ?timestamp1 ?timestamp2)]
  =>
  (if-let [new-fact (create-correctedBrix ?id ?timestamp ?valueBrix ?valueConductivity)]
    (insert! new-fact)))

;(def contador (acc/count))

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
(defquery get-Urea []
  [?Urea <- Urea])


;(def S (apply insert (mk-session) amigos))

(def S (reduce
        (fn [session fact]
          (println (pr-str fact))
          (insert session fact))
        (mk-session)
        initial-data))

(def Q (fire-rules S))

(println "Listo!")

(dorun
  (map
   #(clojure.pprint/pprint (query Q %))
   [;get-Brix
    ;get-CorrectedBrix
    ;get-UpH
    ;get-SpH
    ;get-pHavg
    ;get-Conductivity
    get-Urea
    ]))
