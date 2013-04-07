(ns bio.core
  (:require [clojure.string :as string]
            [loom.graph :as graph]))

(defrecord Rule [reduction])
(defrecord Node [id state rule])
(defrecord Edge [nodes]) ;; reference to node ids

(defn grid-shape
  [n m create-node]
  (let [grid (graph/graph)
        nodes {}]
    (reduce
     (fn [[grid nodes] x]
       (reduce
        (fn [[grid nodes] y]
          (let [id [x y]
                node (create-node id)
                nodes (assoc nodes id node)
                decx (mod (dec x) n)
                incx (mod (inc x) n)
                decy (mod (dec y) m)
                incy (mod (inc y) m)
                grid (graph/add-edges
                      grid
                      [id [decx decy]]
                      [id [x decy]]
                      [id [incx decy]]
                      [id [decx y]]
                      [id [incx y]]
                      [id [decx incy]]
                      [id [x incy]]
                      [id [incx incy]])]
            [grid nodes]))
        [grid nodes] (range m)))
     [grid nodes] (range n))))

(defn total-rule
  [pred]
  (fn [states]
    (let [total (reduce + 0 states)]
      (if (pred total)
        1
        0))))

(def conway
  {0 (total-rule
      (fn [total]
        (= total 3)))

   1 (total-rule
      (fn [total]
        (or
         (= total 2)
         (= total 3))))})

(defn gear
  [nodes grid rules]
  (iterate
   (fn [[nodes grid rules]]
     (let [new-nodes
           (reduce
            (fn [new-nodes [id node]]
              (let [state (:state node)
                    rule (get rules state)
                    neighbor-ids (graph/neighbors grid id)
                    neighbor-states (map
                                     (fn [id]
                                       (get-in nodes [id :state]))
                                     neighbor-ids)
                    next-state (rule neighbor-states)]
                (assoc-in new-nodes [id :state] next-state)))
            nodes nodes)]
       [new-nodes grid rules]))
   [nodes grid rules]))

(defn xy-offset
  [x y n m]
  (fn [a b]
    [(mod (+ a x) n)
     (mod (+ b y) m)]))

(defn imprint
  [nodes pattern]
  (reduce
   (fn [nodes vertex]
     (update-in
      nodes [vertex :state]
      (fn [state] 1)))
   nodes pattern))

(def glider
  [[0 0]
   [1 0]
   [2 0]
   [0 1]
   [1 2]])

(def state-char
  {0 \.
   1 \o})

(defn print-nodes
  [nodes]
  (let [rows (group-by first (keys nodes))
        lines (sort-by (comp first first) (map sort (vals rows)))
        states (map
                (fn [line]
                  (string/join 
                   " "
                   (map
                    (fn [id]
                      (let [state (get-in nodes [id :state])]
                        (get state-char state)))
                    line)))
                lines)]
    (string/join "\n" states)))

(defn imprint-glider
  [])