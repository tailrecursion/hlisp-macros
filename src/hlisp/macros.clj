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
        refs  (mapv (comp read-string second) (re-seq re template))]
    (cond
      (= 0 (count refs)) template
      :else
      `(str ~@(if (= 0 (count text)) 
                refs 
                (remove #(= "" %) (interleave text (conj refs ""))))))))

(defmacro interpolating
  [& body]
  `(do ~@(postwalk #(if (string? %) (i %) %) body)))

(defmacro deftemplate
  "Define a template. Templates are functions."
  [_ [nm _] & forms]
  `(defn ~nm [_# ~@(map first (butlast forms))]
     (interpolating ~(last forms))))

(comment
  
  (macroexpand
    '(interpolating
       (defn doit [foo bar baz]
         {:one "#{foo}asdf#{foo}"}
         "#{baz}"
         "FOO"
         ))) 
  
  (interpolating
    (defn doit [foo bar baz]
      "#{baz} asdf #{foo} #{bar} asdf")) 
  
  (doit "one" "two" "three") 

  (in-ns 'user)

  (deftemplate {}
    (mytemplate {}) 
    (foo {}) 
    (bar {}) 
    (baz {}) 
    {:one "--#{foo}--" :two bar}) 

  (deftemplate {}
    (mytemplate2 {}) 
    (foo {}) 
    {:hey "!!#{foo}!!"})

  (mytemplate2 {} "a")

  (macroexpand (interpolating (let [x "foo"] "--#{x}--")))

  ;=> (macroexpand '(def-values [x y z] [1 2 3]))
  (do
    (def vec__803 [1 2 3])
    (def x (clojure.core/nth vec__803 0 nil))
    (def y (clojure.core/nth vec__803 1 nil))
    (def z (clojure.core/nth vec__803 2 nil)))

  )
