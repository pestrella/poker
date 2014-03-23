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

(defn n-same-suits
  "Returns the suit of any n same suits"
  [n hand]
  (map #(get % 0)
       (filter #(= n (val %))
               (frequencies (map :suit hand)))))

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
  (let [n (count hand)]
    (= 1 (count (n-same-suits n hand)))))

(flush? '({:suit :clubs :pip 6}
         {:suit :clubs :pip 8}
         {:suit :clubs :pip 9}
         {:suit :clubs :pip 2}
         {:suit :hearts :pip 11}))

(defn play []
  (let [hand (deal deck)]
    (println (str "Got a pair? " (if (pair? hand) "yes" "no")))
    (println (seq hand))))

(play )
