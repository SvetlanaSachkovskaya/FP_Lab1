(ns lab1.core-test
  (:use clojure.test
        lab1.core))

(deftest hammingMinDistanceTest
  (testing "hamming calculate-min-distance function doesn't work"
    (is (=  2 (lab1.core/calculate-min-distance {:vect `( 2 1 3 8)} 
	[{:vect `( 2 5 6 5)} {:vect `( 2 3 3 5)} {:vect `( 4 4 6 5)}] hamming-distance )))))

(deftest getDistanceCalculatorTest
  (testing "get-distance-calculator function doesn't work"
    (is (=  euclidian-distance (lab1.core/get-distance-calculator "eucl")))
	(is (=  hamming-distance (lab1.core/get-distance-calculator "hamming")))
	(is (=  euclidian-distance (lab1.core/get-distance-calculator "euclidian")))))
	
(deftest sortByPotentialTest
  (testing "sort-by-potential function doesn't work"
    (is (= [{:potential 1} {:potential 2} {:potential 3} {:potential 6}] (lab1.core/sort-by-potential 
	[{:potential 2} {:potential 6} {:potential 3} {:potential 1}])))))
