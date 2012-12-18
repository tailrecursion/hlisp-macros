(ns hlisp.macros
  (:use
    [clojure.walk   :only [postwalk]]
    [clojure.string :only [blank? split]]) 
  (:require
    [clojure.core.strint :as strint]))

(defn- do-def-values
  [bindings values]
  (->>
    (macroexpand `(let [~bindings ~values]))
    (second)
    (partition 2)
    (map #(cons 'def %))
    (list* 'do)))

(defmacro def-values
  "Destructuring def, similar to scheme's define-values."
  ([bindings values] 
   (do-def-values bindings values))
  ([docstring bindings values]
   (do-def-values bindings values)))

(defn i
  [template]
  (let [parts (remove #(= "" %) (#'strint/interpolate template))]
    (if (every? string? parts) (apply str parts) `(str ~@parts))))

(defn interpolate
  [& forms]
  (postwalk #(if (string? %) (i %) %) forms))

(defmacro interpolating
  [& body]
  `(do ~@(apply interpolate body)))

(defmacro tpl
  [& forms]
  (let [params  (butlast forms)
        body    (last forms)]
    `(fn [~@params] ~body)))

(defmacro deftpl
  [nm & forms]
  `(def ~nm (tpl ~@forms)))

(comment
  
  (def a ["asdf ~{x}" "qwer"])
  (def b ["asdf ~{y}" "qwer"])

  (postwalk identity [1 2 3])

  (postwalk
    #(if (and (vector? %) (= :script (first %)) (= "text/hlisp" (:type (second %))))
       (assoc-in % [2] (str "~(do " (nth % 2) ")")) 
       %)
    [:body [:script {:type "text/hlisp"} "foo"]] 
    ) 

  (interpolate a b)
  (every? string? '("asdf"))

  (macroexpand '(interpolating

    (deftpl
      mytpl 
      foo 
      {:hey "~(if (= \"a\" foo) \"is a\" \"not a\")---~{foo}"})

    (mytpl "a") 

    (mytpl "b")

    )) 

  (clojure.pprint/pprint (macroexpand '(def-values "asdf" [x y z] [1 2 3]))) 
  ;=> (do
  ;     (def vec__803 [1 2 3])
  ;     (def x (clojure.core/nth vec__803 0 nil))
  ;     (def y (clojure.core/nth vec__803 1 nil))
  ;     (def z (clojure.core/nth vec__803 2 nil)))

  )
