(ns wikiindexer.core
  (:gen-class))

(require '[clojure.java.jdbc :as jdbc])
(require '[clojure.java.io   :as io])


;table  for every usable article title in the xml (skip useless namespaces, for example)
;       indicates chars to skip and take (using BufferedInputReader, reader)
;       to get a <page> within xml
;columns
;  num of chars to skip to get to first char of <page>
;  num of chars to skip to get to last char of  </page>
;  title
;  namespace
;  if there is a <redirect tag within, this title is empty and redirects to tag property title, which is the actual article with content

;have a db table with the names of templates we can process and which ones we can ignore
;find in article's wikimarkup what templates are used
;  if there are any that we don't process, mark article for html download
;  we can also keep track of template presence thru separate tables
;    need this to determine which we could gain most by implementing and also learn which to ignore


;BuffereInputReader.read     returns -1  when at EOF
;BuffereInputReader.readLine returns nil when at EOF


(def relevant_namespaces {"0" "" "108" "Book:" "3" "Portal:"})

(def opntags {:page "<page>" :title "<title>"})
(def clstags {"<page>" "</page>" "<title>" "</title>"})


(defn mkreaderautomaton [s]                             ;makes a closure which can be fed chars indefinitely and returns truish (see below) whenever the chars form the target string
  (let [idxmark (atom 0) lastidx (dec (count s))]       ;ret value: length of string - 1, so that the caller knows how many chars to go back to mark that idx as the start of the string's occurrence
    (fn [c]
      (if (= (nth s @idxmark) c)
          (if (= @idxmark lastidx)
              (do (swap! idxmark (constantly 0)) lastidx)
              (do (swap! idxmark inc)            nil))
          (do (swap! idxmark (constantly 0))     nil)))))

(defn readnchars [n]
  (with-open [rdr (io/reader "/Users/ghost/.lein/wikiindexer/.git/resources/dumps/tst.xml")]
    (let [
          page_start_dt   (mkreaderautomaton "<page")
          page_close_dt   (mkreaderautomaton "</page>")
          title_start_dt  (mkreaderautomaton "<title")
          title_close_dt  (mkreaderautomaton "</title>")
          ns_start_dt     (mkreaderautomaton "<ns")
          ns_close_dt     (mkreaderautomaton "</ns>")
          id_start_dt     (mkreaderautomaton "<id")
          id_close_dt     (mkreaderautomaton "</id>")
          tstamp_start_dt (mkreaderautomaton "<timestamp")
          tstamp_close_dt (mkreaderautomaton "</timestamp>")
          text_start_dt   (mkreaderautomaton "<text")
          text_close_dt   (mkreaderautomaton "</text>")

         ]
      (loop [
             m 0 c (. rdr read)
             page_start_idx nil  title_start_idx nil  ns_start_idx nil  id_start_idx nil  tstamp_start_idx nil  text_start_idx nil  
             page_acc '()        title_acc '()        ns_acc '()        id_acc '()        tstamp_acc '()        text_acc '() 
                                 title nil            ns_ nil           id nil            tstamp nil            text nil 
            ]
        (if (and c (> c 0) (< m n))
            (let [
                  c                       (char c)
                  page_start_dt_retval    (page_start_dt  c)
                  page_close_dt_retval    (page_close_dt  c)
                  title_start_dt_retval   (title_start_dt c)
                  title_close_dt_retval   (title_close_dt c)
                  ns_start_dt_retval      (ns_start_dt c)
                  ns_close_dt_retval      (ns_close_dt c)
                  id_start_dt_retval      (id_start_dt c)
                  id_close_dt_retval      (id_close_dt c)
                  tstamp_start_dt_retval  (tstamp_start_dt c)
                  tstamp_close_dt_retval  (tstamp_close_dt c)
                  text_start_dt_retval    (text_start_dt c)
                  text_close_dt_retval    (text_close_dt c)

                  get_new_start_idx       (fn [close_dt_retval start_dt_retval start_idx]
                                            (if close_dt_retval
                                                nil
                                                (if start_dt_retval
                                                    (- m start_dt_retval)
                                                    start_idx)))

                  new_page_start_idx    (get_new_start_idx page_close_dt_retval   page_start_dt_retval   page_start_idx)
                  new_title_start_idx   (get_new_start_idx title_close_dt_retval  title_start_dt_retval  title_start_idx)
                  new_ns_start_idx      (get_new_start_idx ns_close_dt_retval     ns_start_dt_retval     ns_start_idx)
                  new_id_start_idx      (get_new_start_idx id_close_dt_retval     id_start_dt_retval     id_start_idx)
                  new_tstamp_start_idx  (get_new_start_idx tstamp_close_dt_retval tstamp_start_dt_retval tstamp_start_idx)
                  new_text_start_idx    (get_new_start_idx text_close_dt_retval   text_start_dt_retval   text_start_idx)
 
                  get_new_acc           (fn [close_dt_retval start_idx acc]
                                          (cond close_dt_retval  '()
                                                start_idx        (conj acc c)
                                                :else            '()))

                  new_page_acc          (get_new_acc page_close_dt_retval   page_start_idx   page_acc)
                  new_title_acc         (get_new_acc title_close_dt_retval  title_start_idx  title_acc)
                  new_ns_acc            (get_new_acc ns_close_dt_retval     ns_start_idx     ns_acc)
                  new_id_acc            (get_new_acc id_close_dt_retval     id_start_idx     id_acc)
                  new_tstamp_acc        (get_new_acc tstamp_close_dt_retval tstamp_start_idx tstamp_acc)
                  new_text_acc          (get_new_acc text_close_dt_retval   text_start_idx   text_acc)
          
                  get_new_tag_content   (fn [curr_tag_content close_dt_retval acc clos_tag_str] ;need extra arg indicating if we want tag parameters too, in which case return a vector, not just a string
                                          (if close_dt_retval
                                            (let [clos_tag_len (dec (count clos_tag_str))
                                                  raw      (apply str (reverse (drop clos_tag_len acc)))
                                                  rawsplit (. raw split ">" 2)] ;first val here is tag parameters, second is tag value
                                              (second rawsplit))
                                            curr_tag_content))

                  title                 (get_new_tag_content title  title_close_dt_retval  title_acc  "</title>")
                  ns_                   (get_new_tag_content ns_    ns_close_dt_retval     ns_acc     "</ns>")
                  id                    (get_new_tag_content id     id_close_dt_retval     id_acc     "</id>")
                  tstamp                (get_new_tag_content tstamp tstamp_close_dt_retval tstamp_acc "</timestamp>")
                  text                  (get_new_tag_content text   text_close_dt_retval   text_acc   "</text>")
                 ]
              (if page_close_dt_retval      ;process article contents here, at this point the contents of an individual page have been collected
                (do 
                    ;(println (str "title: " m ", " title_start_idx ", " (- m title_close_dt_retval)))
                    (println (str "title: " title))
                    (println (str "ns   : " ns_))
                    (println (str "id   : " id))
                    (println (str "tstmp: " tstamp))
                    (println (str "title: " text))
                    (println (str "------------------------------------"))
                    
;                    (if (= ns_ 2) 1 1)
                ))

              (recur (inc m) (. rdr read)
                     new_page_start_idx new_title_start_idx new_ns_start_idx new_id_start_idx new_tstamp_start_idx new_text_start_idx
                     new_page_acc       new_title_acc       new_ns_acc       new_id_acc       new_tstamp_acc       new_text_acc
                                        title               ns_              id               tstamp               text
)))))))

;(defn readnlines [n]
;  (with-open [rdr (io/reader "/Users/rflores/bin/wikiindexer/resources/dumps/tst.xml")]
;    (loop [l (. rdr readLine) acc [] m 0]
;      (if (and l (< m n))
;          (recur (. rdr readLine) acc (inc m))))))

;(defn readnlines [n]
;  (with-open [rdr (io/reader "/Users/rflores/bin/wikiindexer/resources/dumps/tst.xml")]
;    (loop [c (. rdr read) acc [] m 0]
;      (if (and c (> c 0) (< m n))
;          (let [c (char c)]
;            (println (. rdr readLine))
;            (recur (. rdr read) acc (inc m)))))))

;(defn readnlines [n]
;  (with-open [rdr (io/reader "/Users/rflores/bin/wikiindexer/resources/dumps/jawiki-20160501-pages-articles.xml")]
;    (. rdr skip 200000000)
;    (dotimes [i n]
;      (println (. rdr read)))))

;(defn readnlines [n]
;  (with-open [rdr (io/reader "/Users/rflores/bin/wikiindexer/resources/dumps/jawiki-20160501-pages-articles.xml")]
;    (dotimes [i n]
;      (println (line-seq rdr)))))

;    (doseq [line (line-seq rdr)]
;      (println line))))






(def db-spec {:classname   "org.sqlite.JDBC"
              :subprotocol "sqlite"
              :subname     "resources/dbs/test.db"})

;(sql/query db "select 3*5 as result")

;(sql/db-do-commands db
;  (sql/create-table-ddl :fruit [[:name "varchar(32)"]
;                                [:appearance "varchar(32)"]
;                                [:cost :int]
;                                [:grade :real]]))

;(jdbc/with-db-connection [db-con db-spec]
;  (let [rows (jdbc/query db-con ["SELECT * FROM fruit"])]
;    (prn rows)))
;(jdbc/insert! db-con :table (dissoc (first rows) :id))))


