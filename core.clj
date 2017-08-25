(ns wikiindexer.core
  (:import (org.apache.commons.compress.compressors.bzip2 BZip2CompressorInputStream))
  (:gen-class))

(require '[globals           :as gbl])
(require '[db                :as db])
(require '[file_utils        :as fil])
(require '[html_utils        :as htm])
(require '[ring.util.codec   :as ringutil])
(require '[clojure.string    :as str])
(require '[clojure.java.io   :as io])
(require '[clojure.java.jdbc :as jdbc])
;(require '[clojure.data.json :as json])
(require '[clojure.math.numeric-tower :as math])
(import '(java.io File))
(import '(java.io FileNotFoundException))
(import '(java.net.UnknownHostException))
(import '(java.net.ConnectException))
(import '(javax.net.ssl.SSLException))

;abbreviations: cont/content, opn/open, cls/close, idx/index (generally of a character within its string)


;wiki api, can get metadata on pages, of particular interest are file sizes, to see if can include svgs:

;;;forward declarations: START
(declare parse_page)
(declare parse_page_lite)
(declare make_reader_automaton)
(declare parse_enclosures)
;;;forward declarations: CLOSE

;;;variables we will use throughout: START
(def all_templates  (atom {}))   ;;;{"name" {:occ_count, :id}, ...} intented to be an in-memory copy of the db template table (intention: avoid querying the db too much). It is filled at start of runproc from the db, and updated by every insert/update to the template table (all of this done manually in code ofc), so it's always available and updated. 
(def all_categories (atom {}))   ;;;{"name" {:occ_count, :id}, ...} intented to be an in-memory copy of the db template table (intention: avoid querying the db too much). It is filled at start of runproc from the db, and updated by every insert/update to the template table (all of this done manually in code ofc), so it's always available and updated. 
(def categories_regex (re-pattern "\\[\\[Category:(.*?)\\]\\]"))  ;todo:multilang, more than 1 Category word may appear, depending on lang so adjust regex accordingly
;;;variables we will use throughout: CLOSE

(defn notzero? [n] (not (zero? n)))

(defn re_find_first_match [regexes text]
  (loop [regexes regexes]
    (if-let [regex (first regexes)]
      (if-let [rslt (second (re-find regex text))] ;the second is to get only the "captured string", see the regexes in gbl, the part with parens and brackets is a "capturing group", see doc for more info
        rslt
        (recur (rest regexes))))))






(defn parse_wikimarkup_redir_link [redirect_str]              ;sufficient parsing for redirect links, note though that code below doesn't fully cover the full wikimarkup link spec at all
  (let [redirect_prefix  (if (not= -1 (. redirect_str indexOf ":")) (first (. redirect_str split ":")))
        redirect_section (if (not= -1 (. redirect_str indexOf "#")) (second (. redirect_str split ":")))
        redirect_title   (let [parts     (. redirect_str split ":|#")  ;split by multiple delimiters (here ':' and '#'), see: http://stackoverflow.com/questions/5993779/java-use-split-with-multiple-delimiters
                              parts_cnt (count parts)]
                           (cond (= 1 parts_cnt) (first parts)
                                 (= 2 parts_cnt) (if redirect_prefix (second parts) (first parts))
                                 (= 3 parts_cnt) (second parts)))]
    {:title redirect_title :section redirect_section :redirect_prefix redirect_prefix}))

(defn make_reader_automaton [s]                       ;makes a closure which can be fed chars indefinitely and returns truish (see below) whenever the chars form the target string
  (let [idxmark (atom 0), lastidx (dec (count s))]    ;ret value: length of s - 1, so that the caller knows how many chars to go back to mark that idx as the start of the string's occurrence
    (fn [c]
      (if (= (nth s @idxmark) c)
          (if (= @idxmark lastidx)
              (do (swap! idxmark (constantly 0)) lastidx)
              (do (swap! idxmark inc)            nil))
          (do (swap! idxmark (constantly 0))     nil)))))



(defn procflag? [proc_kw flag_kw]   ;'kw' stands for keyword
  (.exists (io/as-file (str gbl/fpath_base
                            "communication_with_running_procs/"
                            (cond (= proc_kw :articletitles_only) "articletitles_only_proc/"
                                  (= proc_kw :html_download)      "html_download_proc/"
                                  (= proc_kw :svg_make_db)        "svg_make_db_proc/")
                            (cond (= flag_kw :pause)              "PAUSE"
                                  (= flag_kw :stop)               "STOP")))))

(defn runproc_articletitles_only []
  (with-open [rdr (io/reader gbl/fpath_dump :encoding "UTF-8" :buffer-size (* 100 1000 1024))]
    (db/ensure_db_exists)
    (db/ensure_tbl_exists__process_info)
    (let [pages_to_proc_per_loop     1000
          chars_processed_prev_run   (db/select__process_info__chars_processed)]
      (println (str "skipping to char: " chars_processed_prev_run))
      (. rdr skip chars_processed_prev_run)
      (loop [i 0, chars_advanced_so_far chars_processed_prev_run]
        (if (zero? (mod i 100)) (println (str "iter: " i ", chars advanced so far: " chars_advanced_so_far)))
        (if (procflag? :articletitles_only :stop)
          (println "stop file detected, ending here")
          (do
            (loop [procflag_pause (procflag? :articletitles_only :pause)]
              (if procflag_pause
                  (do (println "pause file detected, sleeping for 1 minute...")
                      (Thread/sleep (* 60 1000))
                      (recur (procflag? :articletitles_only :pause)))))
            (if-let [[page_data chars_advanced] (parse_page_lite rdr)]
              (let [chars_advanced_so_far_updated (+ chars_advanced_so_far chars_advanced)]
                (if (= 0 (:is_redirect page_data))
                  (db/insert_title_only    page_data chars_advanced_so_far_updated)
                  (db/insert_redirect_only page_data chars_advanced_so_far_updated))
                (recur (inc i), chars_advanced_so_far_updated))
	      (println "***done***"))))))))


(defn readlines [rdr n]
  (let [lines (take n (line-seq rdr))]
    (if (not (empty? lines))
      (apply str lines)
      nil)))

(defn split_on_first_occ [s c] (if-let [c_start_idx (str/index-of s c)] [(.substring s 0 c_start_idx) (.substring s (inc c_start_idx))])) ;;;nil if c is not found in s
  
(defn split_tag_into_params_and_content [tag]
  (let [[tag_open cont] (split_on_first_occ tag ">")]
    (if-let [rslt (split_on_first_occ tag_open " ")]
      [(second rslt) cont]
      [nil cont])))

(defn get_tag_nn [s tag_name]          ;;nn stands for non-nested, this is a simple parser, don't use for tags that might contain other tags within
  (let [tag_start       (str "<" tag_name)
        tag_start_len   (count tag_start)
        tag_close       (str "</" tag_name ">")
        page_start_idx  (str/index-of s tag_start)
        page_close_idx (if page_start_idx (str/index-of s tag_close page_start_idx))
       ]
    (if page_start_idx
      (if page_close_idx
        (split_tag_into_params_and_content (.substring s page_start_idx page_close_idx))
        (if-let [page_close_idx (if page_start_idx (str/index-of s "/>" page_start_idx))]
          [(.substring s (+ page_start_idx tag_start_len) page_close_idx) nil])))))



   ;(let [max_html_id (Integer/parseInt (str/replace (fil/largest_filename_in_dl_dir) ".html" ""))]




(defn svg_makedb_proc []
  (jdbc/with-db-connection [db_con db/fdb_spec]
    (db/ensure_fdb_exists db_con)
    (let [log_path     (str gbl/fpath_dl_dir_base_svg "log.txt")
          _            (if-not (.exists (io/as-file log_path)) (spit log_path "0"))
          prevrun_last (Integer/parseInt (str/trim (str/trim-newline (slurp log_path))))
          max_html_id  (Integer/parseInt (str/replace (fil/largest_filename_in_dl_dir) ".html" ""))
         ;max_html_id  500
         ]
      (loop [html_id prevrun_last, prev_html_id prevrun_last, i 0]
        (if (and (not= 0 i) (zero? (mod i 1000))) (do (spit log_path prev_html_id) (println (str "iter: " i ", html_id: " prev_html_id))))
        (if (procflag? :svg_make_db :stop)
          (println "stop file detected, ending here")
          (do
            (loop [procflag_pause (procflag? :svg_make_db :pause)]
              (if procflag_pause
                  (do (println "pause file detected, sleeping for 1 minute...")
                      (Thread/sleep (* 60 1000))
                      (recur (procflag? :svg_make_db :pause)))))
            (if (<= html_id max_html_id)
              (let [html (try (slurp (str fpath_dl_dir html_id ".html")) (catch FileNotFoundException e nil))]
                (if html
                  (let [svg_fnames (distinct (map second (re-seq #"<a href=.+/wiki/File:([^\"]*).[sS][vV][gG]\"" (slurp (str fpath_dl_dir html_id ".html")))))]
                   ;(println (str "html_id: " html_id))
                    (loop [svg_fnames svg_fnames]
                      (if-let [svg_fname (first svg_fnames)]
                        (let [svg_fname_dec (htm/decode_html_entities (htm/decode_url_percent svg_fname))
                              svg_fname_enc (htm/encode_url_also_encode_plus_sign svg_fname_dec)
                             ;url_full      (re-find (re-pattern (str "//upload\\.wikimedia\\.org/.+?(?=\\/" svg_fname_enc ".svg" ")")) html)
                              url_full      (re-find (re-pattern (str "//upload\\.wikimedia\\.org/.{1,50}?(?=\\/" svg_fname_enc "\\.svg" ")")) html)   ;note: the {1,50} is to avoid taking more than one consecutive such url, it can happen, e.g. in 133.html, see http://stackoverflow.com/questions/7838240/regex-to-match-only-5-letter-words
                              url_full      (if (not (empty? url_full)) url_full (re-find (re-pattern (str "//upload\\.wikimedia\\.org/.{1,50}?(?=\\/" svg_fname ")")) html))
                             ;_             (println (str "svg_fname    : " svg_fname))
                             ;_             (println (str "svg_fname_dec: " svg_fname_dec))
                             ;_             (println (str "svg_fname_enc: " svg_fname_enc))
                             ;_             (println (str "url_full     : " url_full))
                             ]
                          (let [url  (if (not (empty? url_full))
                                       (str/replace-first
                                         (str/replace-first url_full "//upload.wikimedia.org/wikipedia/commons/thumb/" "")
                                         "//upload.wikimedia.org/wikipedia/en/thumb/" ""))]
                            (if (empty? url) (println (str "did not find url match for svg, html id:" html_id ", svg_fname:" svg_fname ", svg_fname_dec:" svg_fname_dec", svg_fname_enc:" svg_fname_enc)))
                            (db/ensure_fdb_svg_exists db_con svg_fname_dec url html_id))
                          (recur (rest svg_fnames)))))))
                (recur (inc html_id), html_id, (inc i)))
             (do (spit log_path prev_html_id) (println (str "iter: " i ", html_id: " prev_html_id)) (println "***done***")) 
)))))))


(defn fff []
  (jdbc/with-db-connection [db_con db/fdb_spec]
  (let [svg           {:id 1 :name "Hecataeus_world_map-en" :url nil}
        dl_file_path  (str gbl/fpath_dl_dir_svg (:id svg)  ".svg")
        svg           (if (not (empty? (:url svg)))
                        svg
                        (if-let [url_part (htm/get_svg_url_part (:name svg))]
                          (do
                            (db/update_fdb_svg_url db_con (:id svg) url_part)
                            (assoc svg :url url_part))))]
    svg)))

(defn ensuresvgdb []
  (jdbc/with-db-connection [db_con db/fdb_spec]
    (db/ensure_fdb_exists db_con)))


(defn svg_download_proc []
  (jdbc/with-db-connection [db_con db/fdb_spec]
    (loop [svgs (db/select__title__downloadtargetsonly____dlquick_svg db_con)] ;grab some titles to process
      (if-let [svg (first svgs)]
        (if (= gbl/runningdev_id (mod (:id svg) gbl/runningdevs_cnt))
          (do
            (loop [procflag_pause (procflag? :html_download :pause)]
              (if procflag_pause
                  (do (println "pause file detected, sleeping for 1 minute...")
                      (Thread/sleep (* 60 1000))
                      (recur (procflag? :html_download :pause)))))
            (if-not (procflag? :html_download :stop)
              (let [dl_file_path (str gbl/fpath_dl_dir_svg (:id svg)  ".svg")
                    svg          (if (not (empty? (:url svg)))
                                   svg
                                   (if-let [url_part (htm/get_svg_url_part (:name svg))] (do (db/update_fdb_svg_url db_con (:id svg) url_part) (assoc svg :url url_part))))
                    xml          (loop [full_urls (htm/urls_for_svg svg)]
                                   (if-let [full_url (first full_urls)]
                                     (let [xml (try (slurp full_url)
                                                 (catch FileNotFoundException         e  (do
                                                                                           (println (str "FileNotFoundException for svg id, does not exist in server anymore, skipping:" (:id svg)))
                                                                                           (fil/append_to_runningdev_id_log__fnfe_svg svg)
                                                                                           :skip))
                                                 (catch java.net.UnknownHostException e  (do (println (str "network exception:" (type e) ", for id:" (:id svg) ", will retry.")) :retry))
                                                 (catch java.net.ConnectException     e  (do (println (str "network exception:" (type e) ", for id:" (:id svg) ", will retry.")) :retry))
                                                 (catch javax.net.ssl.SSLException    e  (do (println (str "network exception:" (type e) ", for id:" (:id svg) ", will retry.")) :retry))
                                                 (catch java.io.IOException           e  (do (println (str "network exception:" (type e) ", for id:" (:id svg) ", will retry.")) :retry)))]
                                       (cond
                                         (= :retry xml) (do (Thread/sleep gbl/dl_network_exception_sleep) (recur full_urls))
                                         (= :skip xml)  (recur (rest full_urls))
                                         :else          xml))))]
                (if (instance? String xml)
                  (if-let [xml_cont (htm/get_content_svg xml)]
                    (do
                      (spit dl_file_path xml_cont)
                      (db/update_fdb_svg_dlinfo db_con (:id svg) :now (.length (File. dl_file_path)))
                      (Thread/sleep gbl/dl_sleep)
                      (recur (rest svgs)))
                    (println "'<svg' not found within xml! stopping"))
                  (do
                    (Thread/sleep gbl/dl_network_exception_sleep)
                    (case xml
                      :retry (recur svgs)
                      :skip  (do (db/update_fdb_svg_dlinfo db_con (:id svg) "fail" 0) (recur (rest svgs)))))))
              (println  "stop file detected, ending here")))
          (recur (rest svgs)))
        (let [svgs (db/select__title__downloadtargetsonly____dlquick_svg db_con)] ;refill
          (if (first svgs)
            (recur svgs)
            (println "*** done ***")))))))





(defn ggg []
  (with-open [rdr (io/reader gbl/fpath_dump :encoding "UTF-8" :buffer-size (* 100 1024))]
    (loop [s (readlines rdr 2)]
      (if s
        (let [leftover (loop [s s]                                     ;leftover: unprocessed remainder of s (because it doesn't contain a full page tag)
                         (let [page_start_idx (str/index-of s "<page")
                               page_start_idx (if page_start_idx (+ page_start_idx (count "<page")))
                               page_close_idx (if page_start_idx (str/index-of s "</page>" page_start_idx))
                              ]
                           (if (and page_start_idx page_close_idx)
                             (do
                               (let [page              (.substring s page_start_idx page_close_idx)
                                     [page_paras page] (split_on_first_occ page ">")
                                     [page text]       (split_on_first_occ page "<text")
                                     text              (split_tag_into_params_and_content text)
                                     title             (second (get_tag_nn page "title"))
                                     redirect_tag      (get_tag_nn page "redirect")
                                     ns_               (second (get_tag_nn page "ns"))
                                     tstamp            (second (get_tag_nn page "timestamp"))
                                    ]
                                ;(println (str "page: " page))
                                 (println (str "title: " title))
                                 (println (str "redirect: " redirect_tag))
                                 (println (str "timestamp: " tstamp))
                                 (println (str "text: " text))
                                 (println "--------")
                               )
                               ;(println (.substring s page_start_idx page_close_idx))
                               (recur (.substring s page_close_idx)))
                             s
                           )
                         )
                       )]
          (if-let [s (readlines rdr 2)] (recur (str leftover s)))
        )
        (println "done")))))

;(recur (readlines rdr 6))


;(def        page_start_dt   (make_reader_automaton "<page"))       ;note: relying on these tags being non-nested, single appearance and with names whose beginnings don't overlap (e.g. "<ab" would overlap with "<abc"). Would have problems with tags that don't comply with such simplicity, such as <id (see comment right below)
;(def        page_close_dt   (make_reader_automaton "</page>"))
;(def        title_start_dt  (make_reader_automaton "<title"))
;(def        title_close_dt  (make_reader_automaton "</title>"))
;(def        ns_start_dt     (make_reader_automaton "<ns"))
;(def        ns_close_dt     (make_reader_automaton "</ns>"))
;(def        pt_start_dt     (make_reader_automaton "<page")) 
;(def        pt_close_dt     (make_reader_automaton "<text"))
;(def        tstamp_start_dt (make_reader_automaton "<timestamp"))
;(def        tstamp_close_dt (make_reader_automaton "</timestamp>"))
;(def        text_start_dt   (make_reader_automaton "<text"))
;(def        text_close_dt   (make_reader_automaton "</text>"))


;(defn ggg []
;  (with-open [rdr (io/reader gbl/fpath_dump :encoding "UTF-8" :buffer-size (* 10 1000 1024))]
;    (loop [cc 0, file_counter 0, acc "", line (. rdr readLine)]
;      (if (and line (< file_counter 4))
;          (if (and (> cc 1000000) (= "</page>" (str/trim line)))
;              (do 
;                (spit (str gbl/fpath_base "dumps/split-en-w/en-w-" (inc file_counter) ".xml")
;                      (str acc line \newline))
;                (recur 0 (inc file_counter) "" (. rdr readLine)))
;              (recur (+ cc 1 (count line)) file_counter (str acc line \newline) (. rdr readLine)))
;          (spit (str gbl/fpath_base "dumps/split-en-w/en-w-" (inc file_counter) ".xml") acc)))))




(defn runproc_download_html_quick [] 
  (loop [prev_loop_id (fil/largest_filename_in_dl_dir)                               ;id to restart from, last id downloaded by previous run
         titles       (db/select__title__downloadtargetsonly____dlquick prev_loop_id)] ;grab some titles to process, starting after last downloaded before left off
    (if-let [title (first titles)]
      (if (= gbl/runningdev_id (mod (:id title) gbl/runningdevs_cnt))
        (do
          (loop [procflag_pause (procflag? :html_download :pause)]
            (if procflag_pause
                (do (println "pause file detected, sleeping for 1 minute...")
                    (Thread/sleep (* 60 1000))
                    (recur (procflag? :html_download :pause)))))
          (if-not (procflag? :html_download :stop)
            (let [dl_file_path (str gbl/fpath_dl_dir (:id title)  ".html")
                  html         (try (slurp (htm/url_for_title title))
                                 (catch FileNotFoundException         e  (do
                                                                           (println (str "FileNotFoundException for title id, does not exist in server anymore, skipping:" (:id title)))
                                                                           (fil/append_to_runningdev_id_log__fnfe title)
                                                                           :skip))
                                 (catch java.net.UnknownHostException e  (do (println (str "network exception:" (type e) ", for id:" (:id title) ", will retry.")) :retry))
                                 (catch java.net.ConnectException     e  (do (println (str "network exception:" (type e) ", for id:" (:id title) ", will retry.")) :retry))
                                 (catch javax.net.ssl.SSLException    e  (do (println (str "network exception:" (type e) ", for id:" (:id title) ", will retry.")) :retry))
                                 (catch java.io.IOException           e  (do (println (str "network exception:" (type e) ", for id:" (:id title) ", will retry.")) :retry)))]
              (if (instance? String html)
                (if-let [html_cont (htm/get_content html)]
                  (do
                    (spit dl_file_path html_cont)
                    (Thread/sleep gbl/dl_sleep)
                    (recur (:id title) (rest titles)))
                  (println "cont_start_str not found in html! stopping"))
                (do
                  (Thread/sleep gbl/dl_network_exception_sleep)
                  (case html
                    :retry (recur prev_loop_id titles)
                    :skip  (recur (:id title) (rest titles))))))
            (println  "stop file detected, ending here")))
        (recur (:id title) (rest titles)))
      (let [titles (db/select__title__downloadtargetsonly____dlquick prev_loop_id)] ;refill
        (if (first titles)
          (recur prev_loop_id titles)
          (println "*** done ***"))))))


(defn bz2-reader [filename]
  (-> filename
      io/file
      io/input-stream
      (BZip2CompressorInputStream. true)
      io/reader))

(defn print-bz2-file [filename]
  (with-open [rdr (bz2-reader filename)]
    (doseq [line (line-seq rdr)]
      (println line)))) 

(defn bztst []
  (let [path_bz2  "/Volumes/MP2T/wikiindexer/dumps/q/tst/tst.bz2"]
    (print-bz2-file path_bz2)))

;(defn bztst []
;  (let [path_bz2  "/Volumes/MP2T/wikiindexer/dumps/q/enwikiquote-20160720-pages-articles-multistream.xml.bz2"
;        path_out  "/Volumes/MP2T/wikiindexer/dumps/q/tst/gentst.bz2"
;        byte_stt  567
;        byte_end  1198893
;        byte_len  (- byte_end byte_stt)
;        byte_arr  (byte-array (* byte_len 2))]
;    (with-open [rdr (io/input-stream  (java.io.FileInputStream.  path_bz2))
;                wtr (io/output-stream (java.io.FileOutputStream. path_out))]
;      (loop [rtn (. rdr read byte_arr byte_stt (* byte_len 2)), acc rtn]
;        (if (> rtn -1)
;            (do (. wtr write byte_arr 0 ))


(defn splitdump []
  (with-open [rdr (io/reader gbl/fpath_dump :encoding "UTF-8" :buffer-size 1160000)]
    (loop [cc 0, file_counter 0, acc "", line (. rdr readLine)]
      (if (and line (< file_counter 4))
          (if (and (> cc 1000000) (= "</page>" (str/trim line)))
              (do 
                (spit (str gbl/fpath_base "dumps/split-en-w/en-w-" (inc file_counter) ".xml")
                      (str acc line \newline))
                (recur 0 (inc file_counter) "" (. rdr readLine)))
              (recur (+ cc 1 (count line)) file_counter (str acc line \newline) (. rdr readLine)))
          (spit (str gbl/fpath_base "dumps/split-en-w/en-w-" (inc file_counter) ".xml") acc)))))
          



;stuff for parse_page, we could have these within the function, but defining them here to see if performance increases
;todo: wrap these in a closure, def a function with this closure, to keep these off the global ns
(def        page_start_dt   (make_reader_automaton "<page"))       ;note: relying on these tags being non-nested, single appearance and with names whose beginnings don't overlap (e.g. "<ab" would overlap with "<abc"). Would have problems with tags that don't comply with such simplicity, such as <id (see comment right below)
(def        page_close_dt   (make_reader_automaton "</page>"))
(def        title_start_dt  (make_reader_automaton "<title"))
(def        title_close_dt  (make_reader_automaton "</title>"))
(def        ns_start_dt     (make_reader_automaton "<ns"))
(def        ns_close_dt     (make_reader_automaton "</ns>"))
(def        pt_start_dt     (make_reader_automaton "<page")) 
(def        pt_close_dt     (make_reader_automaton "<text"))
(def        tstamp_start_dt (make_reader_automaton "<timestamp"))
(def        tstamp_close_dt (make_reader_automaton "</timestamp>"))
(def        text_start_dt   (make_reader_automaton "<text"))
(def        text_close_dt   (make_reader_automaton "</text>"))
(defn reset_dts [] (do ;this isn't really necessary, but just in case 
                     (page_start_dt   \space)
                     (page_close_dt   \space)
                     (title_start_dt  \space)
                     (title_close_dt  \space)
                     (ns_start_dt     \space)
                     (ns_close_dt     \space)
                     (pt_start_dt     \space)
                     (tstamp_start_dt \space)
                     (tstamp_close_dt \space)
                     (text_start_dt   \space)
                     (text_close_dt   \space)))
(defn get_new_start_idx [start_dt_retval close_dt_retval start_idx cc]
  (if close_dt_retval
      nil
      (if start_dt_retval
          (- cc start_dt_retval)
          start_idx)))
(defn get_new_acc [close_dt_retval start_idx acc c cc]
  (cond close_dt_retval  '()
        start_idx        (conj acc c)
        :else            '()))
(defn get_new_tag_content [curr_tag_content close_dt_retval acc clos_tag_str cc] ;need extra arg indicating if we want tag parameters too, in which case return a vector, not just a string
  (if close_dt_retval
      (let [clos_tag_len (dec (count clos_tag_str))
            raw      (apply str (reverse (drop clos_tag_len acc)))
            rawsplit (. raw split ">" 2)
      ]                                   ;first val here is tag parameters, second is tag value
        (second rawsplit))
      curr_tag_content))




(defn parse_page_lite [rdr] ;takes a reader, advances it char by char until it's gotten a page belonging to a ns of interest, returns it in a vector, along with the number of chars advanced to get that data. Returns nil if it reaches EOF
  (loop [;cc means char count, we process 1 char per loop           ;pt means 'page tags', text between <page> and <text>, can get all other tags within this text
         cc 0              ,  c (. rdr read)     ,  pages_so_far 0  ,
         page_start_idx nil,  title_start_idx nil,  ns_start_idx nil,  pt_start_idx nil,  tstamp_start_idx nil,  text_start_idx nil  
         page_acc '()      ,  title_acc '()      ,  ns_acc '()      ,  pt_acc '()      ,  tstamp_acc '()      ,  text_acc '() 
         page nil          ,  title nil          ,  ns_ nil         ,  pt nil          ,  tstamp nil          ,  text nil          ]
    (if (and c (> c 0))
        ;****** if c, i.e. not at EOF yet:START ******
        (let [c                      (char            c)
              page_start_dt_retval   (page_start_dt   c)    page_close_dt_retval   (page_close_dt   c)
              pt_start_dt_retval     (pt_start_dt     c)    pt_close_dt_retval     (pt_close_dt     c)
              title_start_dt_retval  (title_start_dt  c)    title_close_dt_retval  (title_close_dt  c)
              ns_start_dt_retval     (ns_start_dt     c)    ns_close_dt_retval     (ns_close_dt     c)
;             tstamp_start_dt_retval (tstamp_start_dt c)    tstamp_close_dt_retval (tstamp_close_dt c)
              text_start_dt_retval   (text_start_dt   c)    text_close_dt_retval   (text_close_dt   c)

              new_page_start_idx    (get_new_start_idx page_start_dt_retval   page_close_dt_retval   page_start_idx   cc)
              new_pt_start_idx      (get_new_start_idx pt_start_dt_retval     pt_close_dt_retval     pt_start_idx     cc)
              new_title_start_idx   (get_new_start_idx title_start_dt_retval  title_close_dt_retval  title_start_idx  cc)
              new_ns_start_idx      (get_new_start_idx ns_start_dt_retval     ns_close_dt_retval     ns_start_idx     cc)
;             new_tstamp_start_idx  (get_new_start_idx tstamp_start_dt_retval tstamp_close_dt_retval tstamp_start_idx cc)
              new_text_start_idx    (get_new_start_idx text_start_dt_retval   text_close_dt_retval   text_start_idx   cc)

              new_page_acc          (get_new_acc page_close_dt_retval   page_start_idx   page_acc   c cc)
              new_pt_acc            (get_new_acc pt_close_dt_retval     pt_start_idx     pt_acc     c cc)
              new_title_acc         (get_new_acc title_close_dt_retval  title_start_idx  title_acc  c cc)
              new_ns_acc            (get_new_acc ns_close_dt_retval     ns_start_idx     ns_acc     c cc)
;             new_tstamp_acc        (get_new_acc tstamp_close_dt_retval tstamp_start_idx tstamp_acc c cc)
              new_text_acc          (get_new_acc text_close_dt_retval   text_start_idx   text_acc   c cc)

              title                 (get_new_tag_content title  title_close_dt_retval  title_acc  "</title>"     cc)
              pt                    (get_new_tag_content pt     pt_close_dt_retval     pt_acc     "<text"        cc)
              ns_                   (get_new_tag_content ns_    ns_close_dt_retval     ns_acc     "</ns>"        cc)
;             tstamp                (get_new_tag_content tstamp tstamp_close_dt_retval tstamp_acc "</timestamp>" cc)
              text                  (get_new_tag_content text   text_close_dt_retval   text_acc   "</text>"      cc)
              page                  (get_new_tag_content page   page_close_dt_retval   page_acc   "</page>"      cc)
              ns_prefix             (gbl/relevant_namespaces ns_)]
          (if (and page_close_dt_retval ns_prefix)   ;process article contents here, at this point the contents of an individual page (belonging to a collectable namespace) have been collected
;(do (spit (str "/Users/rflores/gearapps/wikiindexer/temp/" (gbl/make_titlens_key title ns_) "--pg.txt") page)
;    (spit (str "/Users/rflores/gearapps/wikiindexer/temp/" (gbl/make_titlens_key title ns_) "--pt.txt") pt)
;    (spit (str "/Users/rflores/gearapps/wikiindexer/temp/" (gbl/make_titlens_key title ns_) "--tx.txt") text)
            (let [redirect_title                  (second (re-find #"<redirect.*title=\"([^\"]*)\".*" pt))
                  page_data {
                    :title                        (if (= ns_prefix "") title (second (. title split ns_prefix))) ;get rid of namespace prefix on the title, if any
                    :ns                           ns_
                    :redirect_title               redirect_title
                    :is_redirect                  (if (nil? redirect_title) 0 1)
                  }
                  redir_data (if (page_data :redirect_title)
                                 (let [redirect_str (re_find_first_match gbl/redirect_strs_regexes text)] {
                    :to_title                     redirect_title
                    :to_section                   (if redirect_str (second (clojure.string/split redirect_str #"#"))) ;the if is there because the #REDIRECT line may not be well-formed, e.g. article 'Stem (skiing)', notice the extraneous colon, in this case the split would throw a "NullPointerException   java.util.regex.Matcher.getTextLength (Matcher.java:1283)": #REDIRECT: [[stem Christie]]
                    :redirect_str                 redirect_str
                    :redir_from_camelcase         (not (nil? (str/index-of text "CamelCase")))
                    :redir_from_uppercase         (not (nil? (str/index-of text "from uppercase")))
                    :redir_from_lowercase         (not (nil? (str/index-of text "from lowercase")))
                    :redir_from_mixedcase         (not (nil? (str/index-of text "from mixed case")))
                    :redir_from_alt_cap           (not (nil? (str/index-of text "from alternative capitali")))
                    :redir_from_oth_cap           (not (nil? (str/index-of text "from other capitali")))
                    :redir_from_mis_cap           (not (nil? (str/index-of text "from miscapitali")))
                    :redir_from_all_cap           (not (nil? (str/index-of text "from all caps")))}))]
              (do (reset_dts)
                  [(conj page_data redir_data) cc]))
;)
            (let [pages_so_far (if (and page_close_dt_retval ns_prefix) (inc pages_so_far) pages_so_far)]
              (recur (inc cc)           (. rdr read)        pages_so_far
                     new_page_start_idx new_title_start_idx new_ns_start_idx new_pt_start_idx 0                    new_text_start_idx
                     new_page_acc       new_title_acc       new_ns_acc       new_pt_acc       []                   new_text_acc
                     page               title               ns_              pt               tstamp               text))
          )
        )
        ;****** if c, i.e. not at EOF yet:CLOSE   ******
        ;****** else (not c, i.e. reached EOF):START ******
        ; just return nil, telling the caller this way that we've reached EOF and there aren't any more pages
        ;****** else (not c, i.e. reached EOF):CLOSE   ******
)))



;                                 (let [redirect_str (second (re-find #"#[Rr][Ee][Dd][Ii][Rr][Ee][Cc][Tt]\s*\[\[(.+)\]\]" text))] {
