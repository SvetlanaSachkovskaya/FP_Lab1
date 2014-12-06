(ns lab1.core
  (:gen-class))
  (use 'clojure.java.io)

(def r-a 3)
(def r-b (* 1.5 r-a))
(def alpha (/ 4(* r-a r-a)))
(def beta (/ 4(* r-b r-b))) 
(def min-eps 0.15)
(def max-eps 0.5) 


;data parsing
(defn parse-number [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))
 
(defn parse-string-to-features [line]
	(->> (clojure.string/split line #",") 
		 (map clojure.string/trim)
		 (drop-last)
		 (map parse-number)))
		 
(defn read-data-from-file [filename]
	(with-open [rdr (reader filename)]
    (reduce 
		(fn [features line]
			(let [features-vector (parse-string-to-features line )]
				(if (not(nil? features-vector))
					(conj features features-vector))))
         [] (line-seq rdr))))

		 
;distance calculation		 
(defn euclidian-distance [obj1 obj2]
	(->> (map - obj1 obj2)
		 (map #(* % %))
		 (reduce +)))

(defn hamming-distance [obj1 obj2]
	(count (filter false? (map = obj1 obj2))))


;potentials calculation	
(defn potential [x y k distance-calculator]
	(Math/exp (- (* k (distance-calculator x y)))))
	
(defn calculate-potential [features-vector features-vectors distance-calculator]
	(reduce + 
		(map (fn [x] 
			(potential features-vector x alpha distance-calculator)) 
		features-vectors)))

(defn initialize-objects [features-vectors distance-calculator]
	(map (fn [x] 
			(hash-map :vect x :potential (calculate-potential x features-vectors distance-calculator))) 
		features-vectors))
		
(defn calculate-revised-potential [object center distance-calculator]
	(- (:potential object) (* (:potential center) (potential (:vect object) (:vect center) beta distance-calculator))))
		
(defn revise-object-potentials [objects center distance-calculator]
	(map (fn [x] 
			(assoc x :potential (calculate-revised-potential x center distance-calculator))) 
		objects))


(defn sort-by-potential [objects]
	(sort-by :potential objects))
			
(defn calculate-min-distance [object objects distance-calculator]
	(apply min (
		map (fn [x] 
			(distance-calculator (:vect x) (:vect object))) 
		objects)))

		
(defn find-centers [features-vectors distance-calculator]
	( let [initial-objects (initialize-objects features-vectors distance-calculator)
			sorted-initial-objects (sort-by-potential initial-objects)		
			first-center (last sorted-initial-objects)]
		 (loop [objects (drop-last sorted-initial-objects) centers [first-center]]
			(let [revised-objects (revise-object-potentials objects (last centers) distance-calculator)
					sorted-revised-objects (sort-by-potential revised-objects)
					new-center (last sorted-revised-objects)]
					(cond 
						(> (:potential new-center) (* (:potential first-center) max-eps)) 
							(recur (drop-last sorted-revised-objects) (conj centers new-center))
						(< (:potential new-center) (* (:potential first-center) min-eps)) centers
						(>= (+ (/ (calculate-min-distance new-center centers distance-calculator) r-a) (/ (:potential new-center) (:potential first-center))) 1)
							(recur (drop-last sorted-revised-objects) (conj centers new-center))
						:else (recur (conj (drop-last sorted-revised-objects) (assoc new-center :potential 0)) centers))))))

(defn get-distance-calculator [distance-name]
	(if (= distance-name "hamming") hamming-distance euclidian-distance))
						
(defn -main [& args]
	(if (>= (count args) 2)
		(let [distance (get-distance-calculator (last args))
			  features-vectors (read-data-from-file (apply str (first args)))]
			println (find-centers features-vectors distance))
		(println "Few input data")))
		
		
		
		
		
