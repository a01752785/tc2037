(ns process-file)

(as-> (slurp "/examples/suma.von") here
      (clojure.string/replace here
                              #";.*"
                              "")
      (clojure.string/split here
                            #"\s+")
      (remove #(= % "") here)
      (map clojure.edn/read-string here))