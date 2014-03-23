(ns poker.core)

(def deck
  (for [suit [:clubs :hearts :spades :diamonds]
        pip (range 2 15)]
    {:suit suit
     :pip pip}))

(frequencies (map :suit deck))

(shuffle deck)
(first deck)
(first (shuffle deck))

(defn deal [deck]
  (take 5 (shuffle deck)))

(defn n-same-pips
  "Returns the pip of any n same pips"
  [n hand]
  (map #(get % 0)
       (filter #(= n (val %))
               (frequencies (map :pip hand)))))

(defn count-pairs [hand]
  (count (n-same-pips 2 hand)))

(defn pair? [hand]
  (= 1 (count-pairs hand)))

(defn two-pair? [hand]
  (= 2 (count-pairs hand)))

(defn triple? [hand]
  (= 1 (count (n-same-pips 3 hand))))

(defn four-of-a-kind? [hand]
  (= 1 (count (n-same-pips 4 hand))))

(defn full-house? [hand]
  (and (pair? hand) (triple? hand)))

(defn flush? [hand]
  (let [suit (:suit (first hand))]
    (every? #(= suit (:suit %)) (rest hand))))

(defn straight? [hand]
  (let [pips (sort (map :pip hand))]
    (= (count hand)
       (count (loop [left (rest pips)
                     right [(first pips)]]
                (if (= (first left) (+ 1 (last right)))
                  (recur (rest left)
                         (conj right (first left)))
                  right))))))

(straight? '({:suit :clubs :pip 6}
         {:suit :clubs :pip 8}
         {:suit :clubs :pip 9}
         {:suit :clubs :pip 7}
         {:suit :clubs :pip 5}))

(defn play []
  (let [hand (deal deck)]
    (println (str "Got a pair? " (if (pair? hand) "yes" "no")))
    (println (seq hand))))

(play )
