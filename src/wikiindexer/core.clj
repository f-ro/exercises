(ns wikiindexer.core
  (:gen-class))

(require '[clojure.java.jdbc :as jdbc])
(require '[clojure.java.io   :as io])

;notes on math markup
;  this is only visible in the dumps wiki markup, the html will only have an image file instead of the math markup
;    thus, if using downloaded html, will have to insert the math markup into the html. Not entirely trivial, but doable
;  already have lib and code to render this in obj-c, make use of it
;  https://en.wikipedia.org/wiki/Help:Displaying_a_formula

;notes on wiki markup
;  https://en.wikipedia.org/wiki/Help:Wiki_markup
;  there are some special tags and details that need to be handled separately, check the "See also" section in the above url
;    for example, rendering math formulas, etc., see below for notes on that and add as they are discovered

;notes on chem markup
;  dunno about this, check later
;  see doc url for math markup above, "Chemistry" section

;notes on files
;  size can be found at commons.wikimedia, eg: https://commons.wikimedia.org/wiki/File:Pythagorean.svg
;  might want to get svg images, they could fit

;notes on templates
;  sometimes, templates from other language wikipedias are used, for example ja's パリ uses número
;  but if you take a look at the documentation, it's empty in ja, you need to see fr's
;    https://ja.wikipedia.org/wiki/Template:Num%C3%A9ro
;    https://fr.wikipedia.org/wiki/Mod%C3%A8le:Num%C3%A9ro
;  some {{ }} are not templates, they are substitution, see: https://en.wikipedia.org/wiki/Help:Substitution
;  some {{ }} are not templates, but "magic words", see below

;notes on "magic words"
;  some have the same text format as templates, e.g. {{PAGENAME}}
;    https://www.mediawiki.org/wiki/Help:Magic_words#Behavior_switches
;  https://en.wikipedia.org/wiki/Help:Magic_words
;  some {{ }} are also but "magic words" of the parser function type, the "template" name will have a colon, followed by parameters

;notes on database
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

;notes on java api
;  BuffereInputReader.read     returns -1  when at EOF
;  BuffereInputReader.readLine returns nil when at EOF


; (def fpath_dump "/Users/rflores/gearapps/resources/dumps/jawiki-20160501-pages-articles.xml")
;;(def fpath_dump "/Users/rflores/gearapps/resources/dumps/tst.xml")
; (def fpath_db   "/Users/rflores/gearapps/resources/dbs/test.db")



(def relevant_namespaces {"0" "" "14" "Category:" "100" "Portal:" "108" "Book:"})  ;note: deal with languages other than English

;(def opntags {:page "<page>" :title "<title>"})
;(def clstags {"<page>" "</page>" "<title>" "</title>"})


(defn get_template_names_within [text]                                               ;returns vec with 2 elems: first is a set of all template names within text, second is also such a set, but of magic word parser functions (see doc urls in comment below)
  (let [templates (re-seq #"\{\{.*\}\}" text)]
    (loop [templates templates template_names_acc #{} magicwords_parserfunc_names_acc #{}]
      (if-let [template (first templates)]
        (let [template       (apply str (drop 2 (apply str (drop-last 2 template)))) ;get rid of enclosing chars "{{" and "}}"
              template_parts (. template split "\\|")                                ;note we had to escape the pipe character, i.e. | -> \\|
              template_name_colonsplit (. (first template_parts) split ":")          ;see https://en.wikipedia.org/wiki/Help:Magic_words    and    https://www.mediawiki.org/wiki/Help:Extension:ParserFunctions
              is_magicword_parserfunc (> (count template_name_colonsplit) 1)]
          (recur (rest templates)
                 (if is_magicword_parserfunc
                     template_names_acc 
                     (conj template_names_acc              (first template_name_colonsplit)))
                 (if is_magicword_parserfunc
                     (conj magicwords_parserfunc_names_acc (first template_name_colonsplit))
                     magicwords_parserfunc_names_acc)))
        [template_names_acc magicwords_parserfunc_names_acc]))))


(defn make_reader_automaton [s]                             ;makes a closure which can be fed chars indefinitely and returns truish (see below) whenever the chars form the target string
  (let [idxmark (atom 0) lastidx (dec (count s))]       ;ret value: length of string - 1, so that the caller knows how many chars to go back to mark that idx as the start of the string's occurrence
    (fn [c]
      (if (= (nth s @idxmark) c)
          (if (= @idxmark lastidx)
              (do (swap! idxmark (constantly 0)) lastidx)
              (do (swap! idxmark inc)            nil))
          (do (swap! idxmark (constantly 0))     nil)))))

(defn parsewikidump [pages_limit]
  (with-open [rdr (io/reader fpath_dump)]
    (let [
          page_start_dt   (make_reader_automaton "<page")       ;note: relying on these tags being non-nested, single appearance and with names whose beginnings don't overlap (e.g. "<ab" would overlap with "<abc"). Would have problems with tags that don't comply with such simplicity, such as <id (see comment right below)
          page_close_dt   (make_reader_automaton "</page>")
          title_start_dt  (make_reader_automaton "<title")
          title_close_dt  (make_reader_automaton "</title>")
          ns_start_dt     (make_reader_automaton "<ns")
          ns_close_dt     (make_reader_automaton "</ns>")
          id_start_dt     (make_reader_automaton "<id")         ;note: unusable right now, multiple id tags, e.g. contributor's
          id_close_dt     (make_reader_automaton "</id>")
          tstamp_start_dt (make_reader_automaton "<timestamp")
          tstamp_close_dt (make_reader_automaton "</timestamp>")
          text_start_dt   (make_reader_automaton "<text")
          text_close_dt   (make_reader_automaton "</text>")
         ]
      (loop [
             m 0                 c (. rdr read)       pages_count 0
             page_start_idx nil  title_start_idx nil  ns_start_idx nil  id_start_idx nil  tstamp_start_idx nil  text_start_idx nil  
             page_acc '()        title_acc '()        ns_acc '()        id_acc '()        tstamp_acc '()        text_acc '() 
                                 title nil            ns_ nil           id nil            tstamp nil            text nil 
            ]
        (if (and c (> c 0) (< pages_count pages_limit))
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
                (if-let [ns_prefix (relevant_namespaces ns_)]
                  (do
                    (let [
                          title                        (if (= ns_prefix "") title (second (. title split ns_prefix))) ;get rid of namespace prefix on the title, if any
                          [template_names
                          magicword_parserfunc_names]  (get_template_names_within text)
                          math_markup_present          (not= (. text indexOf "</math>") -1)
                          chem_markup_present          (or (not= (. text indexOf "</math chem>") -1) (not= (. text indexOf "</ce>") -1))
                          score_markup_present         (not= (. text indexOf "</score>") -1)
                          svg_files_present            (boolean (re-seq #"\[\[.*\.svg\]\]" text))
                         ]
                      
                      (println (str "title: " title))
                      (println (str "ns   : " ns_))
                      (println "***templates***")
                      (doseq [template_name template_names] (print (str template_name ",")))
                      (println "")
                      (println "***magic word parser functions***")
                      (doseq [mword_pf_name magicword_parserfunc_names] (print (str mword_pf_name ",")))
                      (println "")
                      ;(println (str "id   : " id))
                      ;(println (str "tstmp: " tstamp))
                      ;(println (str "text : " (apply str (take 52 text)) " ... " (apply str (take-last 52 text))))
                      (println (str "------------------------------------"))
              ))))

            (let [pages_count (if (and page_close_dt_retval (relevant_namespaces ns_)) (inc pages_count) pages_count)]
              (recur (inc m)            (. rdr read)        pages_count 
                     new_page_start_idx new_title_start_idx new_ns_start_idx new_id_start_idx new_tstamp_start_idx new_text_start_idx
                     new_page_acc       new_title_acc       new_ns_acc       new_id_acc       new_tstamp_acc       new_text_acc
                                        title               ns_              id               tstamp               text
))))))))




(def db-spec {:classname   "org.sqlite.JDBC"
              :subprotocol "sqlite"
              :subname     fpath_db})



;http://clojure-doc.org/articles/ecosystem/java_jdbc/home.html

(jdbc/with-db-connection [db-con db-spec]  ;note that running this creates the db file, so no need to create it beforehand
  (let [table_names (set (map :name (jdbc/query db-con ["SELECT name FROM sqlite_master WHERE type='table'"])))]
    (prn table_names)
    (if (not (table_names "article"))
      (do (println "'article' table not found, will make it")
          (jdbc/db-do-commands db-spec
                               (jdbc/create-table-ddl :article [
                                                                [:title    :text    "primary key"]
                                                                [:rel_rwid :integer "not null"]
                                                               ])
      )))
))

;(jdbc/with-db-connection [db-con db-spec]
;  (let [rows (jdbc/query db-con ["SELECT * FROM fruit"])]
;    (prn rows)))
;(jdbc/insert! db-con :table (dissoc (first rows) :id))))


