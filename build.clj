(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'lhrb/words)
(def version "1.0.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file "target/woertle.jar")

(defn clean [_]
  (b/delete {:path "target"}))

;; build with clj -T:build uber
(defn uber [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :manifest {"Main-Class" "lhrb.words.core"}}))
