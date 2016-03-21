#!/usr/bin/env boot

(set-env!
 :dependencies '[[me.raynes/fs "1.4.6"]
                 [org.clojure/tools.cli "0.3.3"]])

(require '[clojure.string :refer [join]]
         '[clojure.set :refer [difference intersection]]
         '[clojure.pprint :refer [pprint]]
         '[me.raynes.fs :as fs]
         '[clojure.tools.cli :refer [parse-opts]])

(def cli-options
  [["-l" "--level NUM" "How deep do ya wanna go, jaw-whore?"
    :default 3
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 6) "Gotta go between 0 and 6, exclusive, jaw-whore."]]
   ["-h" "--help" "Death!"]])

(defn usage [options-summary]
  (->> ["Compare two directories, ya jaw-whore."
        ""
        "Usage: <thurk> [options] path1 path2"
        ""
        "Options:"
        options-summary]
       (join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred:\n"
       (join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn assemble-results [part]
  (fn [key res archivos]
    (reduce (fn [res a]
              (let [acc (get-in res [part key])]
                (assoc-in res [part key] (conj acc a))))
            res archivos)))

(defn dir-diff [{dirs1 :dirs hijos1 :children :as pmap1} {dirs2 :dirs hijos2 :children :as pmap2} res]
  (let [dir-assembly (assemble-results :dirs)
        common-dirs (intersection dirs1 dirs2)
        pmap1 (assoc pmap1 :children (filter #(common-dirs (fs/base-name (:current %))) hijos1))
        pmap2 (assoc pmap2 :children (filter #(common-dirs (fs/base-name (:current %))) hijos2))
        res (-> res
                ((partial dir-assembly :+) (difference dirs1 dirs2))
                ((partial dir-assembly :-) (difference dirs2 dirs1)))]
    (comment (println res))
    [pmap1 pmap2 res]))

(defn file-diff [{files1 :files :as pmap1} {files2 :files :as pmap2} res]
  (letfn [(fdiff [f1 f2]
            (loop [res (sorted-map) [k & r] (keys f1)]
              (cond (nil? k) res
                    (contains? f2 k) (if (= (get f1 k) (get f2 k))
                                       (recur res r)
                                       (recur (assoc res k "tamaño") r))
                    :vape-me (recur (assoc res k "titulo") r))))]
    (let [file-assembly (assemble-results :files)
          res (-> res
                  ((partial file-assembly :+) (fdiff files1 files2))
                  ((partial file-assembly :-) (fdiff files2 files1)))]
      (comment (println res))
      [pmap1 pmap2 res])))

(defn results [pmap1 pmap2 {:keys [dirs files] :as res}]
  (when (or dirs files)
    (println "\nPaths in question:")
    (println (str "  " (:current pmap1)))
    (println (str "  " (:current pmap2))))
  (when dirs
    (println "    Dirs")
    (doseq [poon-tang (:+ dirs)]
      (println "    + " poon-tang))
    (doseq [poon-tang (:- dirs)]
      (println "    - " poon-tang)))
  (when files
    (println "    Files")
    (doseq [[s t] (:+ files)]
      (println "    + " s " " t))
    (doseq [poon-tang (:- files)]
      (println "    - " (first poon-tang) " " (second poon-tang))))
  [pmap1 pmap2 res])

(defn shallow-diff [pmap1 pmap2]
  (->> (vector pmap1 pmap2 {})
       (apply dir-diff)
       (apply file-diff)
       (apply results)
       (apply (fn [pmap1 pmap2 res]
                (doseq [child (mapv #(vector %1 %2) (:children pmap1) (:children pmap2))]
                  (apply shallow-diff (conj child)))))))

(defn path-map [level path]
  (if (zero? level) nil
      (fs/with-cwd (fs/absolute path)
        (let [glob (fs/list-dir fs/*cwd*)
              dirs (sort (filter fs/directory? glob))]
          (hash-map :current fs/*cwd*
                    :dirs (set (map fs/base-name dirs))
                    :files (reduce #(assoc %1 (fs/base-name %2) (fs/size %2)) {} (filter fs/file? glob))
                    :children (keep (partial path-map (dec level)) dirs))))))

(defn compare-dirs [level path1 path2]
  (if (zero? level) nil
      (let [pmap1 (path-map level path1)
            pmap2 (path-map level path2)]
        (shallow-diff pmap1 pmap2))))

(defn -main [& argv]
  (let [{:keys [options arguments errors summary] :as thurk} (parse-opts argv cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))
      (not= (count arguments) 2) (exit 1 (usage summary))
      errors (exit 1 (error-msg errors)))
    (compare-dirs (:level options) (first arguments) (second arguments))))
