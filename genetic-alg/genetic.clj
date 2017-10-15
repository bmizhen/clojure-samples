(ns genetic)

(defn random-elements [vector-collection]
 (let [size (count vector-collection)]
   (repeatedly #(vector-collection (rand-int size)))))

(defn tournament-selection
 "Probabilistic tournament selection
  https://en.wikipedia.org/wiki/Tournament_selection

  Larger tournament-size and larger selection-probability tends to favor
  more fit individuals.

  According to http://www.fernandolobo.info/p/thesis.pdf
  tournament-size: 5; selection-probability: 0.5 are good values.

  Degrades to random selection when tournament-size is 0 or
  all fitness values are 0."
 [tournament-size selection-probability scored-population]
 (let [tournament (sort-by #(- (first %))
                          (take tournament-size
                                (random-elements scored-population)))]
   (if-let [winner (some #(and (< (rand) selection-probability) %) tournament)]
     (second winner)
     (recur tournament-size selection-probability scored-population))))

(defn max-fitness
 "Returns the highest score in the scored-population."
 [scored-population]
 (apply max (map first scored-population)))

(defn cross-breed
 "Generate a young generation using crossbreeding.

 cross-breed-fn - takes two members, produces an offspring.
 scored-population - vector of pairs of [score individual].
 max-score - value of max score in the scored population."

 [cross-breed-fn scored-population]
 (let [size (count scored-population)]
   (repeatedly
    size
    #(cross-breed-fn
      ;; see tournament-selection comment about values 5 and 0.5
      (tournament-selection 5 0.5 scored-population)
      (tournament-selection 5 0.5 scored-population)))))

(defn mutate
 "With given probability, applies mutation operator to members of population."
 [mutation-fn mutation-probability population]
 (map #(if (< (rand) mutation-probability) (mutation-fn %) %) population))

(defn score
 "Returns a vector of [score individual] pairs, where score is
 calculated by applying fitness-fn to each member of population."
 [fitness-fn population]
 (into [] (pmap #(vector (fitness-fn %) %) population)))

(defn next-gen
 "Produces a young generation using fitness proportionate selection,
  cross-breeding and mutation."
 [cross-breed-fn
  mutation-fn
  mutation-probability
  scored-old-gen]
 (mutate mutation-fn
         mutation-probability
         (cross-breed cross-breed-fn scored-old-gen)))

(defn evolve
 "Runs a genetic algorithm, using supplied fitness, crossbreed and mutation
 operators.

 Returns the best performing individual.

 fitness-fn - function that can score an individual in the population.
              Higher score means better fitness.
 cross-breed-fn - takes two individual and creates an offspring.
 mutation-fn - produces a new individual by making a random change to
               the argument.
 mutation-probability - probability of mutation.
 good-enough-fitness - stop the algorithm and return the individual that
                       scored this much or more.
 max-iterations - stop the algorithm if this many iterations have passed,
                  return the best scoring individual.
 first-generation - initial population.

 Example usage:

 (time (evolve score-long
               cross-bits-long
               mutate-long
               0.01
               80
               1000
	       (random-longs 2000)))

 TODO: detect plateauing of the max-fitness and stop at this point."
 [fitness-fn
  cross-breed-fn
  mutation-fn
  mutation-probability
  good-enough-fitness
  max-iterations
  first-generation]
 (letfn
  [(evolve-rec
    [iteration old-generation]
    (let [scored-gen (score fitness-fn old-generation)
          max-fitness (max-fitness scored-gen)
          result (fn []
                   [(* (inc iteration) (count old-generation))
                    (some #(and (= max-fitness (first %)) %) scored-gen)])]
      (println "Iteration: " iteration)
      (println "Results: " (result))
      (if (or (= max-iterations (inc iteration))
              (<= good-enough-fitness max-fitness))
        (result)
        (recur (inc iteration)
               (next-gen cross-breed-fn
                         mutation-fn
                         mutation-probability
                         scored-gen)))))]
   (evolve-rec 0 first-generation)))

;;; bit cross-breed and mutation operations on longs.
(defn cross-bits-long
 "Single point crossover function on longs."
 [a b]
 (let [cross-point (rand-int 63)
       right-mask (- (bit-shift-left 1 cross-point) 1)
       left-mask (bit-not right-mask)]
   (bit-or (bit-and a left-mask) (bit-and b right-mask))))

(defn mutate-long
 "Flip a random bit of a long."
 [n] (bit-flip n (rand-int 63)))

(defn score-long
 "Fitness function for population of longs, where every quartet of bits is
 scored like this:0000 - 5, otherwise count 1s."
 [n]
 (letfn [(four-bit-seq
          [n] (map #(bit-and 15 (bit-shift-right n %)) (range 0 63 4)))
         (count-4-bits
          [n] ({0 0 1 1 2 1 3 2 4 1 5 2 6 2 7 3
                8 1 9 2 10 2 11 3 12 2 13 3 14 3 15 4} n))
         (score-4-bits
          [n] ({0 5 1 1 2 2 3 3 4 4} (count-4-bits n)))]
   (apply + (map score-4-bits (four-bit-seq n)))))

(defn random-longs [n]
  (into [] (take n (repeatedly #(long (* (rand) Long/MAX_VALUE))))))

(defn -main
 "this is how you run it"
 []
  (println
   (time (evolve score-long
                 cross-bits-long
                 mutate-long
                 0.01
                 80
                 1000
		 (random-longs 2000))))
  (println "done"))

