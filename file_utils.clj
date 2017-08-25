(ns file_utils)
(require '[globals           :as gbl])
(require '[clojure.string    :as str])
(require '[clojure.java.io   :as io])
(import '(java.io File))
(import '(java.io FileNotFoundException))


(defn largest_filename_in_dl_dir []  ;get last downloaded id, as per largest filename in gbl/fpath_dl_dir, 0 if no files
  (loop [files (.listFiles (File. gbl/fpath_dl_dir)), maxfnamenum 0]     
    (if-let [file (first files)]
      (let [fnamenum (try
                       (Integer/parseInt (apply str (drop-last 5 (.getName file))))  ;5 is ".html"'s length
                       (catch Exception e))]
        (recur (rest files)
               (if fnamenum
                 (if (> fnamenum maxfnamenum) fnamenum maxfnamenum)
                 maxfnamenum)))
      maxfnamenum)))

(defn largest_filename_in_dl_dir_svg []  ;get last downloaded id, as per largest filename in gbl/fpath_dl_dir, 0 if no files
  (loop [files (.listFiles (File. gbl/fpath_dl_dir_svg)), maxfnamenum 0]     
    (if-let [file (first files)]
      (let [fnamenum (try
                       (Integer/parseInt (apply str (drop-last 4 (.getName file))))  ;4 is ".svg"'s length
                       (catch Exception e))]
        (recur (rest files)
               (if fnamenum
                 (if (> fnamenum maxfnamenum) fnamenum maxfnamenum)
                 maxfnamenum)))
      maxfnamenum)))



(defn append_to_runningdev_id_log__fnfe [title]
  (spit (str gbl/fpath_dl_dir_base gbl/runningdev_id ".log")
        (str "FileNotFoundException" "@@-_-@@"  (:id title) "@@-_-@@" (:ns title) "@@-_-@@" (:title title) "\n")
        :append true))

(defn append_to_runningdev_id_log__fnfe_svg [svg]
  (spit (str gbl/fpath_dl_dir_base_svg gbl/runningdev_id ".log")
        (str "FileNotFoundException" "@@-_-@@"  (:id svg) "@@-_-@@" (:name svg) "@@-_-@@" (:url svg) "\n")
        :append true))

(defn copy-file [source-path dest-path]
  (io/copy (io/file source-path) (io/file dest-path)))

(defn merge_dl_dirs [from_dir to_dir dl_id1 dl_id2 log_path stop_path]
  (let [prevrun_last_copied (Integer/parseInt (str/trim (str/trim-newline (slurp log_path))))
        files               (.listFiles (File. from_dir))
        files               (filter #(let [fnamenum (try (Integer/parseInt (apply str (drop-last 5 (.getName %1)))) (catch Exception e nil))]
                                       (and fnamenum
                                            (> fnamenum prevrun_last_copied)
                                            (or (= dl_id1 (mod fnamenum 3)) (= dl_id2 (mod fnamenum 3)))))
                                    files)
        fnamenums           (sort (map #(Integer/parseInt (apply str (drop-last 5 (.getName %1)))) files))
        fnamenums_cnt       (count fnamenums)]
    (loop [fnamenums fnamenums, prev_fnamenum prevrun_last_copied, i 0]
      (if-let [fnamenum (first fnamenums)]
        (do
          (io/copy (io/file (str from_dir fnamenum ".html")) (io/file (str to_dir fnamenum ".html")))
          (if (= 0 (mod i 1000))
            (do
              (println (str i "/" fnamenums_cnt))
              (if (.exists (io/as-file stop_path))
                (do (println "stop file detected, stopping") (spit log_path fnamenum))
                (recur (rest fnamenums), fnamenum, (inc i))))
            (recur (rest fnamenums), fnamenum, (inc i))))
        (do
          (println (str i "/" fnamenums_cnt))
          (println (str "**done**"))
          (spit log_path prev_fnamenum))))))
