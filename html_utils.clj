(ns html_utils)
(require '[globals           :as gbl])
(require '[clojure.string    :as str])
(require '[clojure.data.json :as json])
(require '[ring.util.codec   :as ringutil])
(require '[net.cgrand.enlive-html :as enl])

(declare decode_html_entities)
(declare decode_html_percent_chars)
(declare encode_html_percent_chars)
(declare urls_for_svg)


(defn decode_url_percent [s] (ringutil/percent-decode s))
;(defn encode_url_percent [s] (ringutil/percent-encode s)) ;encodes EVERY character with url percent encoding, including regular characters such as a b c, etc
(defn encode_url_also_encode_plus_sign [s] (str/replace (ringutil/url-encode s) "+" "%2B")) ;need to manually encode the '+' character, since this can be either a regular (special) character in a url. If it's part of a string, it must be url encoded


(defn url_for_title [title]
  (str "https://"
       gbl/lang
       "."
       (case gbl/proj "w" "wikipedia" "d" "wiktionary" "q" "wikiquote")
       ".org/wiki/"
       (gbl/dl_namespaces (:ns title))
       (ringutil/url-encode (decode_html_entities (:title title)))))

(defn get_svg_url_part [name]
  (let [fileinfo_url (str "https://commons.wikimedia.org/w/api.php?action=query&titles=file:"
                          (encode_url_also_encode_plus_sign name) ".svg&format=json&prop=imageinfo&iiprop=url")
        jsonstr     (slurp fileinfo_url)
        jsonmap     (json/read-str jsonstr)
        jsonmap     (get (get jsonmap "query") "pages")
        jsonmap     (first (get (get jsonmap (first (keys jsonmap))) "imageinfo"))
        url         (get jsonmap "url")]
    (if url
      (let [url               (str/replace-first url "https://upload.wikimedia.org/wikipedia/commons/" "")
            idx_first_slash   (str/index-of url "/")
            idx_second_slash  (if idx_first_slash (str/index-of url "/" (inc idx_first_slash)))]
        (if idx_second_slash (subs url 0 idx_second_slash))))))
   

(defn urls_for_svg [svg]
  (let [base_url  "https://upload.wikimedia.org/wikipedia/commons/" 
        name_enc  (ringutil/url-encode (decode_html_entities (:name svg)))]
    [(str base_url "commons/" (:url svg) "/" name_enc ".svg")
     (str base_url "en/"      (:url svg) "/" name_enc ".svg")
     (str base_url "commons/" (:url svg) "/" name_enc ".SVG")
     (str base_url "en/"      (:url svg) "/" name_enc ".SVG")]))

(defn get_content [html] ;html parameter: full html as downloaded from net
  (let [div_opn        "<div"       div_opn_len (count div_opn)
        div_cls        "</div>"     div_cls_len (count div_cls)
        cont_start_str "<div id=\"mw-content-text\""
        cont_start_idx (str/index-of html cont_start_str)
        cont_start_idx (inc (str/index-of html ">" cont_start_idx))     ;note: we're starting right after  the opening div tag to save some bytes, re-add it when reading
        cont_close_idx (loop [opn_cnt 1, cls_cnt 0, from_idx cont_start_idx]
                         (let [nextdiv_start_idx (str/index-of html div_opn from_idx)
                               nextdiv_close_idx (str/index-of html div_cls from_idx)
                               nexttag_is_opn?   (< nextdiv_start_idx nextdiv_close_idx)
                               opn_cnt           (if nexttag_is_opn? (inc opn_cnt) opn_cnt)
                               cls_cnt           (if nexttag_is_opn? cls_cnt       (inc cls_cnt))]
                           (if (= opn_cnt cls_cnt)
                               (dec nextdiv_close_idx)   ;note: we're stopping right before the closing div tag to save some bytes, re-add it when reading
                               (recur opn_cnt,
                                      cls_cnt,
                                      (if nexttag_is_opn? 
                                          (+ nextdiv_start_idx div_opn_len)
                                          (+ nextdiv_close_idx div_cls_len))))))]
    (if cont_start_idx (subs html cont_start_idx (inc cont_close_idx)))))
  
(defn get_content_svg [xml] ;xml parameter: full xml as downloaded from net
  (if-let [svg_opn_idx (str/index-of xml "<svg")]
    (subs xml svg_opn_idx)))

;(let [rslt (try (throw (FileNotFoundException. "ddd")) (catch Exception e e))] (if (= java.io.FileNotFoundException (type rslt)) 1 2))

(defn decode_html_entities [s] ; &amp; -> &  ;http://www.ascii.cl/htmlcodes.htm
  (if-not (str/index-of s "&") 
    s
    (let [s (-> s
              (str/replace "&amp;" "&")
              (str/replace "&quot;" "\"")
              (str/replace "&lt;" "<")
              (str/replace "&gt;" ">")
              (str/replace "&nbsp;" " "))]
      (if-not (str/index-of s "&")
        s
        (-> s
          (str/replace "&iexcl;" "¡")
          (str/replace "&cent;" "¢")
          (str/replace "&pound;" "£")
          (str/replace "&curren;" "¤")
          (str/replace "&yen;" "¥")
          (str/replace "&brvbar;" "¦")
          (str/replace "&sect;" "§")
          (str/replace "&uml;" "¨")
          (str/replace "&copy;" "©")
          (str/replace "&ordf;" "ª")
          (str/replace "&laquo;" "«")
          (str/replace "&not;" "¬")
          (str/replace "&shy;" "­")
          (str/replace "&reg;" "®")
          (str/replace "&macr;" "¯")
          (str/replace "&deg;" "°")
          (str/replace "&plusmn;" "±")
          (str/replace "&sup2;" "²")
          (str/replace "&sup3;" "³")
          (str/replace "&acute;" "´")
          (str/replace "&micro;" "µ")
          (str/replace "&para;" "¶")
          (str/replace "&middot;" "·")
          (str/replace "&cedil;" "¸")
          (str/replace "&sup1;" "¹")
          (str/replace "&ordm;" "º")
          (str/replace "&raquo;" "»")
          (str/replace "&frac14;" "¼")
          (str/replace "&frac12;" "½")
          (str/replace "&frac34;" "¾")
          (str/replace "&iquest;" "¿")
          (str/replace "&Agrave;" "À")
          (str/replace "&Aacute;" "Á")
          (str/replace "&Acirc;" "Â")
          (str/replace "&Atilde;" "Ã")
          (str/replace "&Auml;" "Ä")
          (str/replace "&Aring;" "Å")
          (str/replace "&AElig;" "Æ")
          (str/replace "&Ccedil;" "Ç")
          (str/replace "&Egrave;" "È")
          (str/replace "&Eacute;" "É")
          (str/replace "&Ecirc;" "Ê")
          (str/replace "&Euml;" "Ë")
          (str/replace "&Igrave;" "Ì")
          (str/replace "&Iacute;" "Í")
          (str/replace "&Icirc;" "Î")
          (str/replace "&Iuml;" "Ï")
          (str/replace "&ETH;" "Ð")
          (str/replace "&Ntilde;" "Ñ")
          (str/replace "&Ograve;" "Ò")
          (str/replace "&Oacute;" "Ó")
          (str/replace "&Ocirc;" "Ô")
          (str/replace "&Otilde;" "Õ")
          (str/replace "&Ouml;" "Ö")
          (str/replace "&times;" "×")
          (str/replace "&Oslash;" "Ø")
          (str/replace "&Ugrave;" "Ù")
          (str/replace "&Uacute;" "Ú")
          (str/replace "&Ucirc;" "Û")
          (str/replace "&Uuml;" "Ü")
          (str/replace "&Yacute;" "Ý")
          (str/replace "&THORN;" "Þ")
          (str/replace "&szlig;" "ß")
          (str/replace "&agrave;" "à")
          (str/replace "&aacute;" "á")
          (str/replace "&acirc;" "â")
          (str/replace "&atilde;" "ã")
          (str/replace "&auml;" "ä")
          (str/replace "&aring;" "å")
          (str/replace "&aelig;" "æ")
          (str/replace "&ccedil;" "ç")
          (str/replace "&egrave;" "è")
          (str/replace "&eacute;" "é")
          (str/replace "&ecirc;" "ê")
          (str/replace "&euml;" "ë")
          (str/replace "&igrave;" "ì")
          (str/replace "&iacute;" "í")
          (str/replace "&icirc;" "î")
          (str/replace "&iuml;" "ï")
          (str/replace "&eth;" "ð")
          (str/replace "&ntilde;" "ñ")
          (str/replace "&ograve;" "ò")
          (str/replace "&oacute;" "ó")
          (str/replace "&ocirc;" "ô")
          (str/replace "&otilde;" "õ")
          (str/replace "&ouml;" "ö")
          (str/replace "&divide;" "÷")
          (str/replace "&oslash;" "ø")
          (str/replace "&ugrave;" "ù")
          (str/replace "&uacute;" "ú")
          (str/replace "&ucirc;" "û")
          (str/replace "&uuml;" "ü")
          (str/replace "&yacute;" "ý")
          (str/replace "&thorn;" "þ")
          (str/replace "&yuml;" "ÿ")
          (str/replace "&euro;" "€"))))))
































;(declare parse_tag_nattrs)
;(declare parse_enclosures)
;(declare next_tag_bracket_idxs)
;
;
;
;
;(defn hparse [txt]
;  (let [txt_len (count txt)]
;    (loop [[topn_idx tcls_idx] (next_tag_bracket_idxs txt 0 txt_len), acc []]
;      (if topn_idx
;        (let [tag (parse_tag_nattrs (subs txt (inc topn_idx) tcls_idx))]
;          (if (:self_closed tag)
;            111
;            222
;          )
;          (recur (next_tag_bracket_idxs txt (inc tcls_idx) txt_len) (conj acc tag)))
;        acc))))
;
;(defn next_tag_bracket_idxs [txt from_idx to_idx]
;  (loop [curr_idx from_idx, opn_idx nil]
;    (if (< curr_idx to_idx)
;      (let [c (. txt charAt curr_idx)]
;        (if (and opn_idx (= \> c))
;          [opn_idx curr_idx]
;          (recur (inc curr_idx)
;                 (if opn_idx
;                     opn_idx
;                     (if (= \< c) curr_idx))))))))
;          
;
;
;
;(defn parse_tag_nattrs [txt]
;  (let [len             (count txt)
;        is_self_closed  (= \\ (. txt charAt (dec len)))
;        txt             (if is_self_closed (subs txt 0 (dec len)) txt)
;        len             (if is_self_closed (dec len) len)
;        add_attn        (fn [h s] (assoc (assoc h :prev_added_attn s) s nil))
;        set_attv        (fn [h s prev_attn] (dissoc (assoc h prev_attn s) :prev_added_attn))]
;    (loop [i 0
;           st_eq nil
;           st_qt nil
;           acc_str ""
;           acc_parts {}
;          ]
;      (if (< i len)
;        (let [c (. txt charAt i),
;              d (if (< i (dec len)) (. txt charAt (inc i)))
;              st_eq (cond (and (not st_eq) (not  st_qt) (= \=     c))       \=
;                          (and      st_eq  (= :u st_qt) (= \space c))       nil
;                          (and      st_eq  (= \" st_qt) (= \"     c))       nil
;                          :else                                             st_eq)
;             ]
;         ;(println (str c ", " acc_str ", " st_eq ", " st_qt ",    parts: " acc_parts)) (println (str "--------------------------------"))
;          (recur (inc i)
;            ;;; st_eq   ;;;
;            st_eq
;            ;;; st_qt ;;;
;            (cond (and st_eq (not  st_qt) (=    \"     c))                  \"
;                  (and st_eq (not  st_qt) (not= \space d)  (not= \" d))     :u
;                  (and st_eq (= \" st_qt) (not= \"     c))                  st_qt
;                  (and st_eq (= :u st_qt) (not= \space c))                  st_qt
;                  (and st_eq (= \" st_qt) (=    \"     d))                  nil
;                  (and st_eq (= :u st_qt) (=    \space d))                  nil
;            )
;            ;;; acc_str ;;;
;            (cond (= \space c)
;                    (cond (= \" st_qt)                                      (str acc_str c) 
;                          :else                                             ""
;                    )
;                  (= \" c)
;                                                                            ""
;                  (= \= c)
;                    (cond (= \" st_qt)                                      (str acc_str c) 
;                          :else                                             ""
;                    )
;                  :else                                                     (str acc_str c)
;            )
;            ;;; acc_parts ;;;
;            (cond 
;                  (= \" c)
;                    (cond (= \" st_qt)                                      (set_attv acc_parts acc_str (:prev_added_attn acc_parts))
;                          :else                                             acc_parts
;                    )
;                  (= \space c)
;                    (cond (and (not (seq acc_parts)) (seq acc_str))         (assoc acc_parts :name acc_str)
;                          (= :u st_qt)                                      (if-let [prev_attn (:prev_added_attn acc_parts)]
;                                                                              (set_attv acc_parts acc_str prev_attn)
;                                                                              (add_attn acc_parts acc_str))
;                          (and (not st_eq) (seq acc_parts) (seq acc_str))   (add_attn acc_parts acc_str)
;                          :else                                             acc_parts
;                    )
;                  (= \= c)
;                    (cond (= \" st_qt)                                      acc_parts
;                          (and (seq acc_parts) (seq acc_str))               (add_attn acc_parts acc_str)
;                          :else                                             acc_parts
;                    )
;                  :else                                                     
;                                                                            acc_parts
;            )
;          )
;        )
;        (let [acc_parts (cond (seq acc_str)
;                                (cond (empty? acc_parts)             (assoc acc_parts :name acc_str)
;                                      st_qt                          (set_attv acc_parts acc_str (:prev_added_attn acc_parts))
;                                      :else                          (add_attn acc_parts acc_str)
;                                )
;                              :else
;                                acc_parts)]
;          (assoc (dissoc acc_parts :prev_added_attn) :self_closed is_self_closed))
;      )
;    )
;  )
;)
;
;
;
;;(defn parse_enclosures
;;  ([txt opn cls] (parse_enclosures txt opn cls 0 (dec (count txt))))
;;  ([txt opn cls from_idx to_idx]
;;    (let [cls_len (count cls), opn_dt (make_reader_automaton opn), cls_dt (make_reader_automaton cls)]
;;      (loop [acc [], opn_anc nil, opn_cnt 0, cls_cnt 0, char_idx from_idx] ;anc stands for anchor
;;        (if (<= char_idx to_idx)
;;          (let [c              (. txt charAt char_idx)
;;                opn_dt_retval  (opn_dt c)
;;                cls_dt_retval  (cls_dt c)
;;                opn_cnt        (if opn_dt_retval (inc opn_cnt) opn_cnt)
;;                cls_cnt        (if cls_dt_retval (inc cls_cnt) cls_cnt)
;;               ]
;;            (cond     ;opn_anc  opn_cnt  cls_cnt  char_idx           <- recur params below arranged as columns (except acc)
;;              (and (notzero? opn_cnt) (= opn_cnt cls_cnt))           ;step 3:   found matching (outermost) cls, add to acc, continue
;;                (recur (let [start_idx (inc opn_anc), close_idx (- char_idx cls_len)]
;;                         (conj acc                                     ;note that for acc's nodes, call recursively
;;                               {:start  start_idx
;;                                :close  close_idx
;;                                :name   (str/trim (first (str/split (subs txt start_idx (inc close_idx)) #"\|"))) ;note: this is specific for wikimarkup templates, when we start parsing other stuff generalize this, maybe receive functions that add key/vals to the map and which are called after generating the base map (i.e. generate the map in 3 steps, first generate the base map with :start and :close, then call the passed functions to conj to the map, then conj :nodes)
;;                                :nodes  (parse_enclosures txt opn cls start_idx close_idx)}))
;;                       nil       0        0        (inc char_idx))
;;              (and opn_dt_retval (= 1 opn_cnt) (zero? cls_cnt))      ;step 1:   found first opn, save char_idx to opn_anc, continue
;;                (recur acc
;;                       char_idx  opn_cnt  cls_cnt  (inc char_idx))
;;              :else                                                  ;step 0,2: nothing happened at this char, no changes, continue
;;                (recur acc
;;                       opn_anc   opn_cnt  cls_cnt  (inc char_idx))))
;;          acc)))))
;
;
;
;;(defn parse_tag_nattrs [txt]
;;  (let [len (count txt)]
;;    (loop [i 0
;;           acc_str ""
;;           acc_parts {}
;;           state_part :name
;;           state_into :waiting
;;           state_eq   nil
;;           state_qtyp nil
;;          ]
;;      (if (< i len)
;;        (let [c (. txt charAt i)]
;;          (println (str c ", " acc_str ", " state_part ", " state_into ", " state_eq ", " state_qtyp ",    parts: " acc_parts))
;;          (println (str "--------------------------------"))
;;          (recur (inc i)
;;            ;;; acc_str ;;;
;;            (cond (= \space c)
;;                    (cond (or state_qtyp (= :waiting state_into))             (str acc_str c)
;;                          (and (= :attv state_part) (= :started state_into))  (if (= :unquoted state_qtyp) "" acc_str)
;;                          :else                                               ""
;;                    )
;;                  (= \" c)
;;                                                                              ""
;;                  (= \= c)
;;                    (cond (and (= :attv state_part) (= :started state_into))  (str acc_str c)
;;                          :else                                               ""
;;                    )
;;                  :else                                                       (str acc_str c)
;;            )
;;            ;;; acc_parts ;;;
;;            (cond (= \space c)
;;                    (cond (or state_qtyp (= :waiting state_into))             acc_parts
;;                          (and (= :name state_part) (= :started state_into))  (conj acc_parts [:name acc_str])
;;                          (and (= :attn state_part) (= :started state_into))  (assoc (assoc acc_parts acc_str nil) :latest_attrn acc_str)
;;                          (and (= :attv state_part) (= :started state_into))  (if (= :unquoted state_qtyp) (assoc acc_parts (:latest_attrn acc_parts) acc_str) acc_parts)
;;                          :else                                               acc_parts
;;                    )
;;                  (= \" c)
;;                    (cond (and (= :attv state_part) (= :started state_into))  (assoc acc_parts (:latest_attrn acc_parts) acc_str)
;;                          :else                                               acc_parts
;;                    )
;;                  (= \= c)
;;                    (cond (and (= :attn state_part) (= :started state_into))  (assoc (assoc acc_parts acc_str nil) :latest_attrn acc_str)
;;                          :else                                               acc_parts
;;                    )
;;                  :else
;;                                                                              acc_parts
;;            )
;;            ;;; state_part ;;;
;;            (cond (= \space c)
;;                    (cond (or state_qtyp (= :waiting state_into))             state_part
;;                          (= :attv state_part)                                :attn
;;                          (= :attn state_part)                                :attv
;;                          (= :name state_part)                                :attn
;;                          :else                                               state_part
;;                    )
;;                  (= \= c)
;;                    (cond (and (= :attv  state_part) (= :started state_into)) state_part
;;                          :else                                               :attv
;;                    )
;;                  (= \" c)
;;                    (cond (and (= :attv  state_part) (= :waiting state_into)) state_part
;;                          (and (= :attv  state_part) (= :started state_into)) :attn
;;                          :else                                               state_part
;;                    )
;;                  :else
;;                                                                              state_part
;;            )
;;            ;;; state_into ;;;
;;            (cond (= \space c)
;;                    (cond (or state_qtyp (= :waiting state_into))             state_into
;;                          (and (= :name state_part) (= :started state_into))  :waiting
;;                          (and (= :attv state_part) (= :started state_into))  (if (= :unquoted state_qtyp) :waiting state_into)
;;                          :else                                               :waiting
;;                    )
;;                  (= \= c)
;;                    (cond (and (= :attv state_part) (= :started state_into))  state_into
;;                          :else                                               :waiting
;;                    )
;;                  (= \" c)
;;                    (cond (and (= :attv state_part) (= :started state_into))  :waiting
;;                          (and (= :attv state_part) (= :waiting state_into))  :started
;;                          :else                                               state_into
;;                    )
;;                  :else
;;                    (cond                           (= :waiting state_into)   :started
;;                          :else                                               state_into
;;                    )
;;            )
;;            ;;; state_eq   ;;;
;;            (cond (and (not         state_qtyp)  (not state_eq) (= \=     c))           c
;;                  (and (= :unquoted state_qtyp)       state_eq  (= \space c))           nil
;;                  (and (= \"        state_qtyp)       state_eq  (= \"     c))           nil
;;                  :else                                                                 state_eq
;;            )
;;            ;;; state_qtyp ;;;
;;            (cond (and state_eq (not         state_qtyp) (not= \space c)  (not= \" c))         :unquoted
;;                  (and state_eq (not         state_qtyp) (=    \"     c))                      c
;;                  (and state_eq (= \"        state_qtyp) (not= \"     c))                      state_qtyp
;;                  (and state_eq (= :unquoted state_qtyp) (not= \space c))                      state_qtyp
;;                  :else                                                                 nil
;;            )
;;          )
;;        )
;;        (let [acc_parts_finished
;;          (cond (seq acc_str)
;;                  (cond (not (:name acc_parts))     (assoc acc_parts :name acc_str)
;;                        (:latest_attrn acc_parts)   (assoc acc_parts (:latest_attrn acc_parts) acc_str)
;;                        :else                       (assoc acc_parts acc_str nil)
;;                  )
;;                :else
;;                  acc_parts
;;          )]
;;          (dissoc acc_parts_finished :latest_attrn))
;;      )
;;    )
;;  )
;;)
