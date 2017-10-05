(ns globals)
(require '[clojure.string    :as str])
(declare wikilang_info)

;to start a run: edit first proj, lang, dump_fnames, and fpath_base
(def proj       "w")      ;w,d
(def lang       "en")     ;en,ja
(def dump_year  "2016")
(def dump_month "07")
(def dump_day   "20")
(def fpath_base "/Volumes/2TBMP/wikiindexer/")
;(def fpath_base "/Users/ghost/wikiindexer/")
(def dump_fnames {
                   "w-en" "enwiki-20160720-pages-articles.xml"       ;;; "enwiki-20160720-pages-articles--short.xml"
                   "w-ja" "jawiki-20161101-pages-articles.xml"
                   "d-en" "enwiktionary-20160901-pages-articles.xml"
                   "d-pl" "plwiktionary-20161120-pages-articles.xml"
                 }
)
(def projkey       (str proj "-" lang))
(def fpath_dump    (str fpath_base "dumps/" (dump_fnames projkey)))
(def runningdevs_cnt 1)     ;3 runningdevs
(def runningdev_id   0)     ;0:iMac, 1:macbook air, 2:home
(def dl_sleep        50)  ;milliseconds to sleep between each download
(def dl_network_exception_sleep   (* 22 1 1000))  ;milliseconds to sleep before retry in case of network exception (11 minutes)
(def fpath_dl_dir_base      (str fpath_base "dl/" projkey "/html_cont/"))
(def fpath_dl_dir           (str fpath_dl_dir_base runningdev_id "/"))
(def fpath_dl_dir_base_svg  (str fpath_base "dl/" "w-en" "/files/svg/"))
(def fpath_dl_dir_svg       (str fpath_dl_dir_base_svg runningdev_id "/"))


;to get data for the below:
;https://meta.wikimedia.org/wiki/List_of_Wikipedias
;https://en.wikipedia.org/wiki/Wikipedia:Namespace
;https://en.wikipedia.org/wiki/Wikipedia:Redirect
;and open any article in the target wiki project, check in the bottom section for format help, you'll likely find the redirect, category, file, etc. words in there

(def relevant_namespaces_complete   ;namespaces (key:number, val:prefix word) to be processed during the dump indexing phase, currently match downloads, since we're not doing categories or templates
  {
    "w-en" {"0" "", "100" "Portal:", "108" "Book:"}
    "w-de" {"0" "", "100" "Portal:"}
    ;"w-nl" {"0" ""}
    ;"w-sv" {"0" ""}
    "w-fr" {"0" "", "100" "Portail:"}
    "w-es" {"0" "", "100" "Portal:", "104" "Anexo:"} ;note: maybe won't need Anexo, see later if it has good content
    ;"w-pt" {"0" ""}
    ;"w-it" {"0" ""}
    "w-ja" {"0" "", "100" "Portal:"}
    "w-zh" {"0" "", "100" "Portal:"}
    "w-ko" {"0" "", "100" "포털:"}
    ;"w-vi" {"0" ""}
    ;"w-th" {"0" ""}
    "w-ru" {"0" "", "100" "Портал:"}
    "w-pl" {"0" "", "100" "Portal:"}
    "d-en" {"0" "", "100" "Appendix:", "110" "Wikisaurus:"}
    ;"d-de" {"0" ""}
    ;"d-nl" {"0" ""}
    ;"d-sv" {"0" ""}
    ;"d-fr" {"0" ""}
    ;"d-es" {"0" ""}
    ;"d-pt" {"0" ""}
    ;"d-it" {"0" ""}
    ;"d-ja" {"0" ""}
    ;"d-zh" {"0" ""}
    ;"d-ko" {"0" ""}
    ;"d-vi" {"0" ""}
    ;"d-th" {"0" ""}
    ;"d-ru" {"0" ""}
    "d-pl" {"0" "", "100" "Aneks:", "104" "Portal:"}
  }
)
(def dl_namespaces_complete       ;namespaces that are to be downloaded
  {
    "w-en" {"0" "", "100" "Portal:", "108" "Book:"}
    "w-de" {"0" "", "100" "Portal:"}
    ;"w-nl" {"0" ""}
    ;"w-sv" {"0" ""}
    "w-fr" {"0" "", "100" "Portail:"}
    "w-es" {"0" "", "100" "Portal:", "104" "Anexo:"} ;note: maybe won't need Anexo, see later if it has good content
    ;"w-pt" {"0" ""}
    ;"w-it" {"0" ""}
    "w-ja" {"0" "", "100" "Portal:"}
    "w-zh" {"0" "", "100" "Portal:"}
    "w-ko" {"0" "", "100" "포털:"}
    ;"w-vi" {"0" ""}
    ;"w-th" {"0" ""}
    "w-ru" {"0" "", "100" "Портал:"}
    "w-pl" {"0" "", "100" "Portal:"}
    "d-en" {"0" "", "100" "Appendix:", "110" "Wikisaurus:"}
    ;"d-de" {"0" ""}
    ;"d-nl" {"0" ""}
    ;"d-sv" {"0" ""}
    ;"d-fr" {"0" ""}
    ;"d-es" {"0" ""}
    ;"d-pt" {"0" ""}
    ;"d-it" {"0" ""}
    ;"d-ja" {"0" ""}
    ;"d-zh" {"0" ""}
    ;"d-ko" {"0" ""}
    ;"d-vi" {"0" ""}
    ;"d-th" {"0" ""}
    ;"d-ru" {"0" ""}
    "d-pl" {"0" "", "100" "Aneks:", "104" "Portal:"}
  }
)
(def relevant_namespaces (relevant_namespaces_complete projkey))
(def dl_namespaces (dl_namespaces_complete projkey))
(def redirect_strs (reverse (flatten (conj (if (not= "en" lang) [(:redirect_words (get wikilang_info lang))]) (:redirect_words (get wikilang_info "en"))))))

(defn make_redirectstr_regex [srch_str]
  (loop [acc_regexp "#" srch_str srch_str]
    (if-let [c (first srch_str)]
       (let [lower_case_c (first (str/lower-case c))
             upper_case_c (first (str/upper-case c))]
         (if (= lower_case_c upper_case_c)
           (recur (str acc_regexp c)                      (rest srch_str))
           (recur (str acc_regexp "[" upper_case_c lower_case_c "]") (rest srch_str))))
       (re-pattern (str acc_regexp "\\s*\\[\\[(.+)\\]\\]"))))) ;note: escaping the \ char through \\, this is because it's a normal string thus we don't want to say "escaped s" \s, we want to say "\s" \\s. Note that if this were a regex string, there would be no need, we could just write #"#[Rr][Ee][Dd][Ii][Rr][Ee][Cc][Tt]\s*\[\[(.+)\]\]"

(def redirect_strs_regexes (map make_redirectstr_regex redirect_strs))


(defn make_titlens_key
  ([page_data] (str (page_data :ns) "@_-_@" (page_data :title)))
  ([title ns]  (str ns "@_-_@" title)))


;wikipedia info       ;note: for redirect strings, if more than 1, optimized order is from unlikeliest to likeliest, see ja, リダイレクト is there just in case, whereas 転送 is the one that's in the documentation
(def wikilang_info {
"aa" {}
"ab" {}
"ace" {}
"ady" {}
"af" {}
"ak" {}
"als" {}
"am" {}
"an" {}
"ang" {}
"ar" {}
"arc" {}
"arz" {}
"as" {}
"ast" {}
"av" {}
"ay" {}
"az" {}
"azb" {}
"ba" {}
"bar" {}
"bat-smg" {}
"bcl" {}
"be" {}
"be-tarask" {}
"bg" {}
"bh" {}
"bi" {}
"bjn" {}
"bm" {}
"bn" {}
"bo" {}
"bpy" {}
"br" {}
"bs" {}
"bug" {}
"bxr" {}
"ca" {}
"cbk-zam" {}
"cdo" {}
"ce" {}
"ceb" {}
"ch" {}
"cho" {}
"chr" {}
"chy" {}
"ckb" {}
"co" {}
"cr" {}
"crh" {}
"cs" {}
"csb" {}
"cu" {}
"cv" {}
"cy" {}
"da" {}
"de" {:english_name "German" :name "Deutsch" :redirect_words "WEITERLEITUNG" :category "Kategorie:" :file "Datei:"}  ;note: for file, "Bild:" too?
"diq" {}
"dsb" {}
"dv" {}
"dz" {}
"ee" {}
"el" {}
"eml" {}
"en" {:english_name "English" :name "English" :redirect_words "REDIRECT" :category "Category:" :file "File:"}
"eo" {}
"es" {:english_name "Spanish" :name "Español" :redirect_words ["REDIRECCION" "REDIRECCIÓN"] :category nil :file nil}
"et" {}
"eu" {}
"ext" {}
"fa" {}
"ff" {}
"fi" {}
"fiu-vro" {}
"fj" {}
"fo" {}
"fr" {:english_name "French" :name "Français" :redirect_words "REDIRECTION" :category nil :file nil}
"frp" {}
"frr" {}
"fur" {}
"fy" {}
"ga" {}
"gag" {}
"gan" {}
"gd" {}
"gl" {}
"glk" {}
"gn" {}
"gom" {}
"got" {}
"gu" {}
"gv" {}
"ha" {}
"hak" {}
"haw" {}
"he" {}
"hi" {}
"hif" {}
"ho" {}
"hr" {}
"hsb" {}
"ht" {}
"hu" {}
"hy" {}
"hz" {}
"ia" {}
"id" {}
"ie" {}
"ig" {}
"ii" {}
"ik" {}
"ilo" {}
"io" {}
"is" {}
"it" {}
"iu" {}
"ja" {:english_name "Japanese" :name "日本語" :redirect_words ["リダイレクト" "転送"] :category "カテゴリ:" :file "ファイル:"}
"jam" {}
"jbo" {}
"jv" {}
"ka" {}
"kaa" {}
"kab" {}
"kbd" {}
"kg" {}
"ki" {}
"kj" {}
"kk" {}
"kl" {}
"km" {}
"kn" {}
"ko" {:english_name "Korean" :name "한국어" :redirect_words "넘겨주기" :category "분류:" :file "파일:"}
"koi" {}
"kr" {}
"krc" {}
"ks" {}
"ksh" {}
"ku" {}
"kv" {}
"kw" {}
"ky" {}
"la" {}
"lad" {}
"lb" {}
"lbe" {}
"lez" {}
"lg" {}
"li" {}
"lij" {}
"lmo" {}
"ln" {}
"lo" {}
"lrc" {}
"lt" {}
"ltg" {}
"lv" {}
"mai" {}
"map-bms" {}
"mdf" {}
"mg" {}
"mh" {}
"mhr" {}
"mi" {}
"min" {}
"mk" {}
"ml" {}
"mn" {}
"mo" {}
"mr" {}
"mrj" {}
"ms" {}
"mt" {}
"mus" {}
"mwl" {}
"my" {}
"myv" {}
"mzn" {}
"na" {}
"nah" {}
"nap" {}
"nds" {}
"nds-nl" {}
"ne " {}
"new" {}
"ng" {}
"nl" {}
"nn" {}
"no" {}
"nov" {}
"nrm" {}
"nso" {}
"nv" {}
"ny" {}
"oc" {}
"om" {}
"or" {}
"os" {}
"pa" {}
"pag" {}
"pam" {}
"pap" {}
"pcd" {}
"pdc" {}
"pfl" {}
"pi" {}
"pih" {}
"pl" {:english_name "Polish" :name "Polski" :redirect_words "PATRZ" :category "Kategoria:" :file "Plik:"} ;note: need to test that PATRZ is indeed the redirect word, but very likely yes
"pms" {}
"pnb" {}
"pnt" {}
"ps" {}
"pt" {}
"qu" {}
"rm" {}
"rmy" {}
"rn" {}
"ro" {}
"roa-rup" {}
"roa-tara" {}
"ru" {:english_name "Russian" :name "Русский" :redirect_words "перенаправление" :category "Категория:" :file "файл:"}
"rue" {}
"rw" {}
"sa" {}
"sah" {}
"sc" {}
"scn" {}
"sco" {}
"sd" {}
"se" {}
"sg" {}
"sh" {}
"si" {}
"simple" {}
"sk" {}
"sl" {}
"sm" {}
"sn" {}
"so" {}
"sq" {}
"sr" {}
"srn" {}
"ss" {}
"st" {}
"stq" {}
"su" {}
"sv" {}
"sw" {}
"szl" {}
"ta" {}
"te" {}
"tet" {}
"tg" {}
"th" {}
"ti" {}
"tk" {}
"tl" {}
"tn" {}
"to" {}
"tpi" {}
"tr" {}
"ts" {}
"tt" {}
"tum" {}
"tw" {}
"ty" {}
"tyv" {}
"udm" {}
"ug" {}
"uk" {}
"ur" {}
"uz" {}
"ve" {}
"vec" {}
"vep" {}
"vi" {}
"vls" {}
"vo" {}
"wa" {}
"war" {}
"wo" {}
"wuu" {}
"xal" {}
"xh" {}
"xmf" {}
"yi" {}
"yo" {}
"za" {}
"zea" {}
"zh" {:english_name "Chinese" :name "中文" :redirect_words "重定向" :category nil :file nil}
"zh-classical" {}
"zh-min-nan" {}
"zh-yue" {}
"zu" {}
})


;de
;redirect	https://de.wikipedia.org/wiki/Hilfe:Weiterleitung
;category	https://de.wikipedia.org/wiki/Hilfe:Kategorien
;file		https://de.wikipedia.org/wiki/Hilfe:Bilder

;ja
;redirect	https://ja.wikipedia.org/wiki/Help:%E3%83%AA%E3%83%80%E3%82%A4%E3%83%AC%E3%82%AF%E3%83%88
;category	
;file		


;es
;redirect	https://es.wikipedia.org/wiki/Ayuda:Redirecci%C3%B3n

;ru
;redirect	https://ru.wikipedia.org/wiki/%D0%92%D0%B8%D0%BA%D0%B8%D0%BF%D0%B5%D0%B4%D0%B8%D1%8F:%D0%9F%D0%B5%D1%80%D0%B5%D0%BD%D0%B0%D0%BF%D1%80%D0%B0%D0%B2%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F

;pl
;namespaces	https://pl.wikipedia.org/wiki/Pomoc:Przestrze%C5%84_nazw
;redirect	click Edit on any article, search "Wikikod", you'll find the words for redirect, category, file, etc
