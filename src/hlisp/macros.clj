(ns hlisp.macros
  (:use
    [clojure.walk   :only [postwalk]]
    [clojure.string :only [blank? split]]) 
  (:require
    [clojure.core.strint :as strint]))

(defmacro <-
  [last-arg f & args]
  `(~f ~@args ~last-arg))
 
(defmacro <<-
  [f & args]
  `(~f ~(last args) ~@(butlast args)))

(defn- add-docstring
  [docstring pair]
  (if (string? docstring)
    (list (first pair) docstring (last pair))
    pair))

(defn- do-def-values
  [docstring bindings values]
  (->>
    (macroexpand `(let [~bindings ~values]))
    (second)
    (partition 2)
    (map (partial add-docstring docstring)) 
    (map #(cons 'def %))
    (list* 'do)))

(defmacro def-values
  "Destructuring def, similar to scheme's define-values."
  ([bindings values] 
   (do-def-values nil bindings values))
  ([docstring bindings values]
   (do-def-values docstring bindings values)))

(defmacro def-elem
  [param body]
  `(do (def ~param ~body) nil))

(defmacro def-elems
  [params body]
  `(do (def-values [~@(rest params)] ~body) nil))

(defmacro tpl [params body]
  `(fn [~@(rest params)] ~body))

(defmacro deftpl
  [nm params body]
  `(do (def ~nm (tpl ~params ~body)) nil))

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

(comment
  
  (def a ["asdf ~{x}" "qwer"])
  (def b ["asdf ~{y}" "qwer"])

  (interpolate a b)

  (use 'clojure.walk)

  (clojure.pprint/pprint
    (macroexpand-all '(def-elems
                        (arglist foo bar)
                        (doit form input)))) 

  (macroexpand '(hlisp.macros/def-values [foo bar] (doit form input))) 

  (macroexpand '(interpolating

    (deftpl
      mytpl 
      foo 
      {:hey "~(if (= \"a\" foo) \"is a\" \"not a\")---~{foo}"})

    (mytpl "a") 

    (mytpl "b")

    )) 
  
  (clojure.pprint/pprint (macroexpand '(def-values [x y z] [1 2 3]))) 
  ;=> (do
  ;     (def vec__803 [1 2 3])
  ;     (def x (clojure.core/nth vec__803 0 nil))
  ;     (def y (clojure.core/nth vec__803 1 nil))
  ;     (def z (clojure.core/nth vec__803 2 nil)))

  )
