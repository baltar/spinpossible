(ns net.blankenburg.spinpossible.spinpossible3
  (:use net.blankenburg.spinpossible.spinpossible2)
  (:use clojure.contrib.math))

(set! *warn-on-reflection* true)

; Model

(def original-positions
  (memoize
    (fn [width height]
      (range (* width height)))))

(defn map-xy [f positions width height]
  (map #(position-from-xy % width)
       (map f
            (map #(xy-from-position % width height) positions))))
  
(defn mirror-positions-x [positions width height]
  (let [max-y (int (dec height))]
    (map-xy (fn [[x y]] [x (- max-y y)]) positions width height)))
 
(defn mirror-positions-y [positions width height]
  (let [max-x (int (dec width))]
    (map-xy (fn [[x y]] [(- max-x x) y]) positions width height)))
 
(defn mirror-diagonal [positions width height]
  (map-xy (fn [[x y]] [y x]) positions width height))

(def equivalence-positions
  (memoize
    (fn [width height]
      (let [originals (original-positions width height)
            mirrored-x (mirror-positions-x originals width height)
            mirrored-y (mirror-positions-y originals width height)
            mirrored-xy (mirror-positions-y mirrored-x width height)
            mirrored-d (mirror-diagonal originals width height)
            mirrored-xd (mirror-diagonal mirrored-x width height)
            mirrored-yd (mirror-diagonal mirrored-y width height)
            mirrored-xyd (mirror-diagonal mirrored-xy width height)]
        [(long-array originals) (long-array mirrored-x) (long-array mirrored-y) (long-array mirrored-xy)
         (long-array mirrored-d) (long-array mirrored-xd) (long-array mirrored-yd) (long-array mirrored-xyd)]))))

(defn equivalence-fields [field width height]
  (map #(map (fn [i] (nth field i)) %) (equivalence-positions width height)))

(defn long-pow ^long [^long x ^long y]
  (loop [r x i (dec y)]
    (if (pos? i) 
      (recur (* r x) (dec i))
      r)))

(defn long-encode ^long [^longs field ^long field-length]
  (loop [pos (long 0) r (long 0)]
      (if (< pos field-length)
        (recur 
          (inc pos)
          (+ r (* (long-pow (inc (* 2 field-length)) pos) (+ (long (aget field pos)) field-length))))
        r)))

(defn pow [x y]
  (loop [r x i (dec y)]
    (if (pos? i) 
      (recur (* r x) (dec i))
      r)))

(defn encode [field field-length]
  (loop [pos (long 0) r (num 0)]
      (if (< pos field-length)
        (recur 
          (inc pos)
          (+ r (* (pow (inc (* 2 field-length)) pos) (+ (long (nth field pos)) field-length))))
        r)))

(defn equivalence-codes-set [^longs field ^long width ^long height]
  (hash-set (map #(encode % (* width height)) (equivalence-fields field width height))))

; Solver

(defn get-spins-fn [width height]
  "Returns a function [ field depth last-move -> moves-sequence ]" 
  (let [all-possible-moves (all-possible-moves width height)]
    (fn [field last-move]
      (filter (make-candidate-filter field width last-move) all-possible-moves))))

(defn get-children [field width height spins-fn encoded-seen-fields-set previous-moves]
  (let [moves (spins-fn field (last previous-moves))
        candidates (map #(vector (move field width height %) %) moves)]
    (loop [rest-candidates candidates new-encoded-seen-fields-set encoded-seen-fields-set moves previous-moves]
      (if (seq rest-candidates)
        (let [candidate (first rest-candidates)
              equivalance-codes (equivalence-codes-set (first candidate) width height)]
          (if (not-any? encoded-seen-fields-set equivalance-codes)          
            (recur (rest rest-candidates) (conj new-encoded-seen-fields-set (first equivalance-codes)) (conj moves (second candidate)))
            (recur (rest rest-candidates) new-encoded-seen-fields-set moves)))
        [new-encoded-seen-fields-set moves]))))
          

; Annahme: alles außerhalb der b-box aller Vertauschten + die direkt anliegenden tiles können ignoriert werden (Reduzierung der Feldgröße)
; solche Moves bevorzugen! 
        
        