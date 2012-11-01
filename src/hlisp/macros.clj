(ns hlisp.macros
  (:use [clojure.walk :only [postwalk]])
  (:use [clojure.string :only [blank? split]]))

(defmacro def-values
  "Destructuring def, similar to scheme's define-values."
  [bindings values]
  (->>
    (macroexpand `(let [~bindings ~values]))
    (second)
    (partition 2)
    (map #(cons 'def %))
    (list* 'do)))

(defn i [template]
  (let [re    #"#\{([^}]+)}"
        text  (split template re) 
        refs  (mapv (comp read-string second) (re-seq re template))
        ndif  (max 0 (- (count refs) (count text))) 
        pads  (repeat ndif "")
        both  (remove #(= "" %) (interleave (concat text pads) (conj refs "")))
        parts (if (seq text) both refs)]
    (if (seq refs) `(str ~@parts) template)))

(defmacro interpolating
  [& body]
  `(do ~@(postwalk #(if (string? %) (i %) %) body)))

(defmacro tpl
  "Create a template function. Does string interpolation."
  [& forms]
  (let [params  (butlast forms)
        body    (last forms)]
    `(fn [~@params] (interpolating ~body))))

(defmacro deftpl
  "Create and bind a template function."
  [nm & forms]
  (let [params  (butlast forms)
        body    (last forms)]
    `(defn ~nm [~@params] (interpolating ~body))))

(comment
  
  (deftpl
    mytemplate2 
    foo 
    {:hey "#{foo}---#{foo}"})

  (mytemplate2 "a")

  ;=> (macroexpand '(def-values [x y z] [1 2 3]))
  (do
    (def vec__803 [1 2 3])
    (def x (clojure.core/nth vec__803 0 nil))
    (def y (clojure.core/nth vec__803 1 nil))
    (def z (clojure.core/nth vec__803 2 nil)))

  )
