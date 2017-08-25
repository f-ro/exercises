(ns db)

(require '[globals           :as gbl])
(require '[clojure.java.jdbc :as jdbc])
(require '[clojure.string    :as str])
(import '(java.sql.SQLException))

(def fpath_db           (str gbl/fpath_base "dbs/" gbl/projkey ".db"))
(def fpath_db__dlquick  (str gbl/fpath_base "dbs/" gbl/projkey " --dl_quick.db"))
(def fpath_fdb          (str gbl/fpath_base "dbs/" "files" ".db"))
(def db_spec            {:classname "org.sqlite.JDBC" :subprotocol "sqlite" :subname fpath_db})
(def db_spec__dlquick   {:classname "org.sqlite.JDBC" :subprotocol "sqlite" :subname fpath_db__dlquick})
(def fdb_spec           {:classname "org.sqlite.JDBC" :subprotocol "sqlite" :subname fpath_fdb})

(declare tst)
(declare insert_article_page_datas)
(declare ensure_db_exists)

(declare ensure_fdb_exists)
(declare ensure_fdb_svg_exists)

(declare ensure_tbl_exists__process_info)
(declare select__process_info__chars_processed)
(declare update__process_info__chars_processed)
(declare select__title__downloadtargetsonly)
(declare select__title__downloadtargetsonly____dlquick)
(declare select__title__nonredirects__title_ns)
(declare select__title__all__title_ns)
(declare select__title__id)
(declare select__template__all)
(declare select__template__all__id_name_occcount)
(declare insert__template)
;(declare insert__process_info__chars_processed)
(declare insert_multi!) ;;;wrapper around jdbc/insert-multi! that returns list of rowids for the newly inserted rows (same order as rows)


(defn ensure_fdb_exists [db_con]
  ;(jdbc/with-db-connection [db_con fdb_spec]
    (let [table_names (set (map :name (jdbc/query db_con ["SELECT name FROM sqlite_master WHERE type='table'"])))]
      (if (empty? table_names)
        (let [tbls_static (hash-map (keyword "svg")                                      ;table name
                                    [[:id               :integer   :primary :key]
                                     [:name             :text      :not :null]
                                     [:url              :text           :null] ;if null, the html had no url for it, just a link to a File:, so get the url later
                                     [:html_id          :text           :null] ;html file where it was first found and recorded into the db (corresponds to a title id)
                                     [:size             :integer        :null]
                                     [:size_compressed  :integer        :null]
                                     [:dl_tstamp        :text           :null]
                                     ["UNIQUE " "(name)"                    ]]
                          )
             ]
          (doseq [sttmnt tbls_static]
            (jdbc/db-do-commands fdb_spec (jdbc/create-table-ddl (first sttmnt) (second sttmnt))))))
      db_con
))
;)

(defn ensure_fdb_svg_exists [db_con name url html_id]
  (let [rslt (jdbc/query db_con ["SELECT * FROM svg WHERE name=?" name])]
    (if (empty? rslt)
      (jdbc/insert! db_con "svg" {:name name :url (if (not (empty? url)) url) :html_id html_id})
      (first rslt))))
    
(defn update_fdb_svg_url [db_con rowid url] (jdbc/update! db_con "svg" {:url url} ["id=?" rowid]))
(defn update_fdb_svg_dlinfo [db_con rowid tstamp fsize]
  (jdbc/update! db_con "svg"
                {:dl_tstamp (if (= :now tstamp) (.format (java.text.SimpleDateFormat. "yyMMdd") (new java.util.Date)) tstamp)
                 :size fsize} ["id=?" rowid]))


(defn ensure_tbl_exists__process_info []
  (jdbc/with-db-connection [db_con db_spec proj lang]  ;note that running this creates the db file, so no need to create it beforehand
    (let [rows (jdbc/query db_con ["SELECT * FROM process_info"])]
      (if-let [row (first rows)]
        row
        (jdbc/insert! db_con "process_info"
                      {:proj            gbl/proj
                       :lang            gbl/lang
                       :dump_date       (str gbl/dump_year "-" gbl/dump_month "-" gbl/dump_day)
                       :progress_status "created empty db"
                       :chars_processed 0})))))

(defn select__title__id [db_con title ns_]
  (jdbc/query db_con [(str "SELECT id FROM title WHERE title=? AND ns=" ns_) title]))
  

(defn select__title__all__title_ns []
  (jdbc/with-db-connection [db_con db_spec] (jdbc/query db_con ["SELECT title,ns FROM title"])))

(defn select__title__nonredirects__title_ns []
  (jdbc/with-db-connection [db_con db_spec] (jdbc/query db_con ["SELECT title,ns FROM title WHERE is_redirect='0'"])))
  
(defn select__title__downloadtargetsonly____dlquick [from_id]
  (let [qry (str "SELECT id,title,ns FROM title WHERE "
                      "id > " from_id " AND is_redirect='0' AND ns in ("
                      (loop [i 0, nss (keys gbl/dl_namespaces), acc ""]
                        (if (empty? nss)
                          acc
                          (recur (inc i) (rest nss) (str acc (if (> i 0) "," "") "'" (first nss) "'"))))
                      ") LIMIT 100")]
    (jdbc/with-db-connection [db_con db_spec]
      (jdbc/query db_con [qry]))))

;(defn select__title__downloadtargetsonly____dlquick_svg [db_con from_id]
;  (jdbc/query db_con [(str "SELECT id,name,url FROM svg WHERE id > " from_id " LIMIT 100")]))
(defn select__title__downloadtargetsonly____dlquick_svg [db_con]
  (jdbc/query db_con [(str "SELECT id,name,url FROM svg WHERE dl_tstamp is null LIMIT 2")]))

(defn select__title__downloadtargetsonly []
  (jdbc/with-db-connection [db_con db_spec]
    (jdbc/query db_con
                ["SELECT id,title,ns FROM title WHERE is_redirect='0' AND ns in ('0','100','108')"])))

(defn select__template__all__id_name_occcount []
  (jdbc/with-db-connection [db_con db_spec]
    (let [rslts (jdbc/query db_con ["SELECT name, id, occ_count FROM template"])]
      (loop [acc {}, rslts rslts]
        (if-let [rslt (first rslts)]
          (recur (conj acc {(rslt :name) {:occ_count (rslt :occ_count) :id (rslt :id)}}), (rest rslts))
          acc)))))

(defn ensure__template [template_names db____con]
  (jdbc/with-db-connection [db_con db_spec]
  (let [sql_base  "SELECT name, occ_count FROM template where name in (?"  ;todo assuming at least one template, add checking code later
        sql_qmrks (apply str (repeat (dec (count template_names)) ",?"))
        sql       (str sql_base sql_qmrks ")")]
    (jdbc/query db_con (apply vector sql template_names)))))

    ;(jdbc/db-do-prepared db_con sql template_names))))

(defn select__process_info__chars_processed []
  (jdbc/with-db-connection [db_con db_spec]
    ((first (jdbc/query db_con ["SELECT chars_processed FROM process_info"])) :chars_processed)))

;(defn update__process_info__chars_processed [n]
;  (jdbc/with-db-connection [db_con db_spec]
;    (let [rows (jdbc/query db_con ["SELECT * FROM process_info"])]
;      (jdbc/update! db_con :process_info {:chars_processed n} []))))


;http://clojure-doc.org/articles/ecosystem/java_jdbc/home.html




(defn ensure_db_exists []
  (jdbc/with-db-connection [db_con db_spec proj lang]  ;note that running this creates the db file, so no need to create it beforehand
    (let [table_names (set (map :name (jdbc/query db_con ["SELECT name FROM sqlite_master WHERE type='table'"])))]
      (if (empty? table_names)
          (let [tbls_static (hash-map (keyword "process_info")                 ;table name
                                      [[:proj            :text    :not :null]  ;table columns
                                       [:lang            :text    :not :null]
                                       [:dump_date       :text    :not :null]
                                       [:progress_status :text    :not :null]
                                       [:chars_processed :integer :not :null]] ;chars to skip when starting run, because they have been parsed as pages and processed. -1 if done

                                      (keyword "category")
                                      [[:id          :integer :primary :key]
                                       [:title       :text    :not :null   ]
                                       ["UNIQUE " "(title)"]]

                                      (keyword "title")
                                      [[:id          :integer :primary :key] ;note: this col becomes an alias to rowid, but stops rowid from being potentially updated by VACUUM, so we really need it. See: http://stackoverflow.com/questions/35876171/sqlite-rowid-after-deleting-rows
                                       [:title       :text    :not :null   ]
                                       [:ns          :text    :not :null   ]
                                       [:is_redirect :integer :not :null   ]
                                       ["UNIQUE " "(title, ns)"]]
  
                                      (keyword "redirect_info")
                                      [[:id                        :integer :primary :key]
                                       [:to_title                  :text    :not :null   ]
                                       [:to_section                :text    :null        ]
                                       [:title_id                  :integer :not :null   ] ;parent title_id
                                       [:to_title_id               :integer :null        ] ;note: may be null because the redirect page may appear in the dump before the actual article page, thus may not have a db record for it at that point
                                       [:redir_from_camelcase      :integer :null        ]
                                       [:redir_from_uppercase      :integer :null        ]
                                       [:redir_from_lowercase      :integer :null        ]
                                       [:redir_from_mixedcase      :integer :null        ]
                                       [:redir_from_alt_cap        :integer :null        ]
                                       [:redir_from_oth_cap        :integer :null        ]
                                       [:redir_from_mis_cap        :integer :null        ]
                                       [:redir_from_all_cap        :integer :null        ]
                                       ["UNIQUE " "(title_id)"
                                        "FOREIGN KEY" "(title_id)" " REFERENCES title(id)"]] ;note: by default this constraint is not enabled on an sqlite3 db, need to turn it on by issuing a pragma command to it, see: http://stackoverflow.com/questions/9433250/foreign-key-constraint-doesnt-work
  
                                      (keyword "template")
                                      [[:id               :integer :primary :key]
                                       [:name             :text    :not :null   ]
                                       [:occ_count        :integer :not :null   ]
                                       [:coded            :integer :null        ]
                                       [:ignorable        :integer :null        ]
                                       [:is_navbox        :integer :null        ]
                                       ["UNIQUE " "(name)"]]
                                     
                                      (keyword "wikimarkup")
                                      [[:id                      :integer :primary :key]
                                       [:title_id                :integer              ]
                                       [:wikimarkup              :text                 ]
                                       [:contains_markup_math    :integer              ]
                                       [:contains_markup_chem    :integer              ]
                                       [:contains_markup_score   :integer              ]
                                       [:contains_files_svg      :integer              ]
                                       ["UNIQUE " "(title_id)"
                                        "FOREIGN KEY" "(title_id)" " REFERENCES title(id)"]] ;note: by default this constraint is not enabled on an sqlite3 db, need to turn it on by issuing a pragma command to it, see: http://stackoverflow.com/questions/9433250/foreign-key-constraint-doesnt-work
  
                                      (keyword "html")
                                      [[:id                       :integer :primary :key]
                                       [:title_id                 :integer              ]
                                       [:html                     :text                 ]
                                       [:html_first_chars         :text                 ]    ;250
                                       [:length                   :integer :not :null   ]
                                       [:flag_found_title_in_bold :integer :not :null   ]
                                       [:flag_html_tag_open_close :integer :not :null   ]
                                       [:flag_no_such_article     :integer :not :null   ]
                                       ["FOREIGN KEY" "(title_id)" " REFERENCES title(id)"]]
  
                                   ;;; junctions ;;;
                                      (keyword "wikimarkup_template")
                                      [[:wikimarkup_id           :integer              ]
                                       [:template_id             :integer              ]
                                       ["UNIQUE " "(wikimarkup_id, template_id) "
                                        "FOREIGN KEY" "(wikimarkup_id)" " REFERENCES wikimarkup(id) "
                                        "FOREIGN KEY" "(template_id)"   " REFERENCES template(id)"]]

                                      (keyword "category_category")
                                      [[:child_id    :integer :not :null   ]
                                       [:paren_id    :integer :not :null   ]
                                       ["UNIQUE " "(child_id, paren_id)"]]
  
                                      (keyword "title_category")
                                      [[:title_id    :integer :not :null   ]
                                       [:category_id :integer :not :null   ]
                                       ["UNIQUE " "(title_id, category_id)"]]
                            )
               ]
           (doseq [sttmnt tbls_static]
;            (println (first sttmnt))
             (jdbc/db-do-commands db_spec (jdbc/create-table-ddl (first sttmnt) (second sttmnt)))))))))


            
;(jdbc/with-db-connection [db_con db_spec]
;  (let [rows (jdbc/query db_con ["SELECT * FROM fruit"])]
;    (prn rows)))
;(jdbc/insert! db_con :table (dissoc (first rows) :id))))

;(defn ensure__templates [db_con page_datas all_template_names]
;  (
;(defn fill__all_template_names [holding_atom]
;  (swap! holding_atom (hash-map "t1" 1 "t2" 2)))

(defn insert_multi! [db_con table_name rows] (map first (map vals (jdbc/insert-multi! db_con table_name rows))))

;(defn insert_titles_only [page_datas]
;  (if (not (empty? page_datas))
;    (jdbc/with-db-transaction [db_con db_spec]
;      (doall                                                                ;without doall'ing we get a sql lock exception
;        (map #(try [(first (vals (first (jdbc/insert! db_con :title %)))) true]
;               (catch java.sql.SQLException sqle
;                 (if (str/includes? (.getMessage sqle) "UNIQUE constraint failed: title.title, title.ns")
;                   (do
;                     (println (str "dupe:" (gbl/make_titlens_key (:title %) (:ns %))))
;                     [(:id (first (select__title__id db_con (:title %) (:ns %))))) false]
;                   (do
;                     (println "unknown sqlexception occurred, aborting process")
;                     (println sqle)
;                     (throw sqle))))
;               (catch Exception e
;                 (do
;                   (println (str "some exception occurred for:" (gbl/make_titlens_key (:title %) (:ns %))))
;                   (println e)
;                   (throw e))))
;             (map #(select-keys % [:title :ns :is_redirect]) page_datas))))))


(defn insert_title_only [page_data chars_processed]
  (jdbc/with-db-transaction [db_con db_spec]
    (try
      (let [title_id (first (vals (first (jdbc/insert! db_con :title (select-keys page_data [:title :ns :is_redirect])))))]
        (jdbc/update! db_con :process_info {:chars_processed chars_processed} [])
        title_id)
      (catch java.sql.SQLException sqle
        (if (str/includes? (.getMessage sqle) "UNIQUE constraint failed: title.title, title.ns")
          (do
            (println (str "dupe:" (gbl/make_titlens_key (:title page_data) (:ns page_data))))
            nil)  ;return nil if already in db ;old: return id: (:id (first (select__title__id db_con (:title %) (:ns %))))
          (do
            (println "unknown sqlexception occurred, aborting process")
            (println sqle)
            (throw sqle))))
      (catch Exception e
        (do
          (println (str "some exception occurred for:" (gbl/make_titlens_key (:title page_data) (:ns page_data))))
          (println e)
          (throw e))))))

(defn insert_redirect_only [page_data chars_processed]
  (jdbc/with-db-transaction [db_con db_spec]
    (if-let [title_id (insert_title_only (select-keys page_data [:title :ns :is_redirect]) chars_processed)]
      (let [redirect_info (conj (select-keys page_data [:to_title
                                                        :to_section 
                                                        :redir_from_camelcase
                                                        :redir_from_uppercase
                                                        :redir_from_lowercase
                                                        :redir_from_mixedcase
                                                        :redir_from_alt_cap
                                                        :redir_from_oth_cap
                                                        :redir_from_mis_cap
                                                        :redir_from_all_cap])
                                [:title_id title_id])]
        [title_id (first (vals (first (jdbc/insert! db_con :redirect_info redirect_info))))]))))

;(defn insert_redirects_only [page_datas]
;  (if (not (empty? page_datas))
;    (jdbc/with-db-transaction [db_con db_spec]
;      (let [title_ids       (insert_multi! db_con :title
;                                           (map #(select-keys % [:title :ns :is_redirect]) page_datas))
;            redirect_infos  (map #(conj %2 [:title_id %1])
;                                 title_ids
;                                 (map #(select-keys % [:to_title :to_section 
;                                                       :redir_from_camelcase
;                                                       :redir_from_uppercase
;                                                       :redir_from_lowercase
;                                                       :redir_from_mixedcase
;                                                       :redir_from_alt_cap
;                                                       :redir_from_oth_cap
;                                                       :redir_from_mis_cap
;                                                       :redir_from_all_cap])
;                                      page_datas))
;            redir_ids       (insert_multi! db_con :redirect_info redirect_infos)]
;))))

(defn insert_article_page_datas [page_datas all_templates]
  (if (not (empty? page_datas))
  (jdbc/with-db-transaction [db_con db_spec]
    (let [;title_ids       (insert_multi! db_con :title
          ;                               (map #(select-keys % [:title :ns :is_redirect]) page_datas))
          ;wikimarkup_ids  (insert_multi! db_con :wikimarkup
          ;                               (map #(conj %1 [:title_id %2])
          ;                                    (map #(select-keys % [:wikimarkup :contains_markup_math :contains_markup_chem :contains_markup_score :contains_files_svg]) page_datas)
          ;                                    title_ids))
          
          pd_tnames       (set (apply concat (map :template_names page_datas)))  ;#{template-names-in-page_datas}
          
          pd_tnames_occs  (loop [acc {}, pd_tnames pd_tnames]   ;{"template name" total-occurrences-in-all-page_datas, ...}
                            (if-let [pd_tname (first pd_tnames)]
                              (let [pd_tname_occs (loop [acc 0, page_datas page_datas]
                                                    (if-let [page_data (first page_datas)]
                                                      (recur (if ((page_data :template_names) pd_tname) (inc acc) acc)
                                                             (rest page_datas))
                                                      acc))]
                                (recur (conj acc [pd_tname pd_tname_occs]), (rest pd_tnames)))
                              acc))
          
          pd_tnames_indb          (map first (filter #(@all_templates (first %)) pd_tnames_occs))

          pd_tnames_occs_indb     (select-keys  pd_tnames_occs pd_tnames_indb)
          pd_tnames_occs_notindb  (apply dissoc pd_tnames_occs pd_tnames_indb)
         ]
      (let [template_ids_inserted (insert_multi! db_con :template
                                                 (map #(hash-map :name (first %) :occ_count (second %))
                                                      pd_tnames_occs_notindb))
           ]
        (do
          (if (> (count template_ids_inserted) 0)
            (swap! all_templates conj (reduce conj
                                              (map #(hash-map (first %2) (hash-map :occ_count (second %2) :id %1))
                                                   template_ids_inserted
                                                   pd_tnames_occs_notindb))))
          
          (loop [pd_tnames_occs_indb pd_tnames_occs_indb]
            (if-let [[pd_tname pd_toccs] (first pd_tnames_occs_indb)]
              (do
                (jdbc/update! db_con :template
                                     {:occ_count (+ ((@all_templates pd_tname) :occ_count) pd_toccs)}
                                     ["id = ?" ((@all_templates pd_tname) :id)])
                (swap! all_templates update-in [pd_tname :occ_count] #(+ % pd_toccs))
                (recur (rest pd_tnames_occs_indb)))))
        )
      )
 

     (println "pd_tnames:")
     (println pd_tnames)
     (println "pd_tnames_occs:")
     (println pd_tnames_occs)
     (println "pd_tnames_indb:")
     (println pd_tnames_indb)
     (println "***pd_tnames_occs_indb***:")
     (println pd_tnames_occs_indb)
     (println "***pd_tnames_occs_notindb***:")
     (println pd_tnames_occs_notindb)
     (println "***all_templates***:")
     (println @all_templates)
))))

(defn tst []
  (jdbc/with-db-transaction [db_con db_spec]
    (let [page_datas [{:title "t1" :wikimarkup "txt1"} {:title "t2" :wikimarkup "txt2"}]
          title_ids  (insert_multi! db_con :title
                                           [{:title "t1" :ns "0" :is_redirect "0"}
                                            {:title "t2" :ns "1" :is_redirect "1"}])
          wmrkps     (map #(conj %1 [:title_id %2])
                          (map #(select-keys % [:wikimarkup]) page_datas)
                          title_ids)
          wmrkps_ids (insert_multi! db_con :wikimarkup wmrkps)
         ]
      wmrkps
)))
