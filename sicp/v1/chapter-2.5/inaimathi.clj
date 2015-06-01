(ns sicp
  "Chapter 2.5.3 of SICP implemented in Clojure (with some stuff pulled out of 3.3.3)"
  (:require [clojure.pprint :as pp]
            [clojure.contrib.math :as math]
            [clojure.contrib.generic.math-functions :as math]))

;;;;;;;;;; Basics from 3.3.3
(defn make-table [] (atom {}))

(defn insert! [k1 k2 v table]
  (swap! table 
         (fn [it]
           (if-let [sub (it k1)]
             (assoc it k1 (assoc sub k2 v))
             (assoc it k1 {k2 v})))))

(defn lookup [k1 k2 table]
  (let [subtable (@table k1)]
    (if subtable (subtable k2))))

(def *method-table* (make-table))

(defn put [name tags function]
  (insert! name tags function *method-table*))

(defn get [name tags] 
  (lookup name tags *method-table*))

;;;;;;;;;; Basics from earlier chapters
(defn attach-tag [tag value] [tag value])
(defn type-tag [[tag _]] tag)
(defn contents [[_ val]] val)

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)]
    (if-let [proc (get op type-tags)]
      (apply proc (map contents args))
      (throw (Exception. (pp/cl-format nil "No method '~a' for types [~{~a~^, ~}]" op type-tags))))))

;;;;;;;;;; The givens
(defn add-terms [l1 l2]
  (cond (empty-termlist? l1) l2
        (empty-termlist? l2) l1
        :else
        (let [t1 (first-term l1)
              t2 (first-term l2)]
          (cond (> (order t1) (order t2))
                (adjoin-term t1 (add-terms (rest-terms l1) l2))
                (< (order t1) (order t2))
                (adjoin-term t2 (add-terms l1 (rest-terms l2)))
                :else
                (adjoin-term
                 (make-term (order t1) (add (coeff t1) (coeff t2)))
                 (add-terms (rest-terms l1) (rest-terms l2)))))))

(defn install-polynomial-package []
  (let [make-poly (fn [variable term-list] [variable term-list])
        variable (fn [[var _]] var)
        term-list (fn [[_ terms]] terms)
        variable? symbol?
        same-variable? (fn [a b] (and (variable? a) (variable? b) (= a b)))
        tag (fn [p] (attach-tag 'polynomial p))

        add-poly (fn [p1 p2] 
                   (if (same-variable? (variable p1) (variable p2))
                     (make-poly (variable p1)
                                (add-terms (term-list p1) (term-list p2)))
                     (throw (Exception. "Polys not in same var: ADD-POLY"))))
        mul-poly (fn [p1 p2] 
                   (if (same-variable? (variable p1) (variable p2))
                     (make-poly (variable p1)
                                (mul-terms (term-list p1) (term-list p2)))
                     (throw (Exception. "Polys not in same var: MUL-POLY"))))]
    (put 'add '(polynomial polynomial)
         (fn [p1 p2] (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
         (fn [p1 p2] (tag (mul-poly p1 p2))))
    (put 'make 'polynomial
         (fn [var terms] (tag (make-poly var terms))))))

;;;;;;;;;; Now then...
