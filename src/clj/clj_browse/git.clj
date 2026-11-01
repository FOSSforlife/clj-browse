(ns clj-browse.git
  "Thin wrapper over the git CLI. Objects are read through one long-running
  `git cat-file --batch` process per repo; tree objects are parsed directly."
  (:require [babashka.process :as p]
            [clojure.string :as str])
  (:import (java.io BufferedInputStream ByteArrayOutputStream InputStream OutputStream)
           (java.security MessageDigest)))

(defn git-available? []
  (try (zero? (:exit (p/shell {:out :string :err :string :continue true} "git" "--version")))
       (catch Exception _ false)))

(defn git
  "Run git in `repo`, return trimmed stdout. Throws ex-info on failure."
  [repo & args]
  (let [{:keys [exit out err]} (apply p/shell {:dir (str repo) :out :string :err :string
                                                :continue true}
                                      "git" (map str args))]
    (if (zero? exit)
      (str/trimr out)
      (throw (ex-info (str "git " (str/join " " args) " failed: " (str/trim err))
                      {:repo (str repo) :args args :exit exit :err err})))))

(defn git* "Like git but returns nil on failure." [repo & args]
  (try (apply git repo args) (catch Exception _ nil)))

;; ---------------------------------------------------------------------------
;; hashing

(defn blob-sha
  "Git blob SHA-1 of bytes (same as `git hash-object`)."
  ^String [^bytes bs]
  (let [md (MessageDigest/getInstance "SHA-1")]
    (.update md (.getBytes (str "blob " (alength bs) "\u0000") "UTF-8"))
    (.update md bs)
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md)))))

(defn sha1-hex ^String [^String s]
  (let [md (MessageDigest/getInstance "SHA-1")]
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md (.getBytes s "UTF-8"))))))

;; ---------------------------------------------------------------------------
;; cat-file --batch

(defrecord Batch [proc ^OutputStream in ^InputStream out lock])

(defn open-batch [repo]
  (let [proc (p/process {:dir (str repo) :err :discard} "git" "cat-file" "--batch")]
    (->Batch proc (:in proc) (BufferedInputStream. (:out proc) 65536) (Object.))))

(defn close-batch [^Batch b]
  (when b
    (try (.close ^OutputStream (:in b)) (catch Exception _))
    (try (p/destroy (:proc b)) (catch Exception _))))

(defn- read-line-bytes ^String [^InputStream in]
  (let [buf (ByteArrayOutputStream.)]
    (loop []
      (let [c (.read in)]
        (cond
          (= c -1) (when (pos? (.size buf)) (.toString buf "UTF-8"))
          (= c 10) (.toString buf "UTF-8")
          :else (do (.write buf c) (recur)))))))

(defn- read-n ^bytes [^InputStream in n]
  (let [bs (byte-array n)]
    (loop [off 0]
      (when (< off n)
        (let [r (.read in bs off (- n off))]
          (when (neg? r) (throw (ex-info "Unexpected EOF from cat-file" {})))
          (recur (+ off r)))))
    bs))

(defn cat-file
  "Returns {:type \"blob\"|\"tree\"|\"commit\"|\"tag\" :bytes byte[]} or nil when missing."
  [^Batch b obj]
  (locking (:lock b)
    (let [^OutputStream in (:in b) ^InputStream out (:out b)]
      (.write in (.getBytes (str obj "\n") "UTF-8"))
      (.flush in)
      (let [header (read-line-bytes out)]
        (when-not header (throw (ex-info "cat-file process died" {:obj obj})))
        (if (str/ends-with? header " missing")
          nil
          (let [[_sha type size] (str/split header #" ")
                bs (read-n out (parse-long size))]
            (.read out) ; trailing LF
            {:type type :bytes bs}))))))

(defn cat-blob-string [b sha]
  (some-> (cat-file b sha) :bytes (String. "UTF-8")))

(defn- hex [^bytes bs off len]
  (let [sb (StringBuilder.)]
    (dotimes [i len] (.append sb (format "%02x" (bit-and (aget bs (+ off i)) 0xff))))
    (str sb)))

(defn parse-tree
  "Parse a raw tree object into [{:mode \"100644\" :name \"x.clj\" :sha \"...\" :type :blob|:tree|:commit}]."
  [^bytes bs]
  (let [n (alength bs)]
    (loop [i 0 acc (transient [])]
      (if (>= i n)
        (persistent! acc)
        (let [sp (loop [j i] (if (= 32 (aget bs j)) j (recur (inc j))))
              nul (loop [j sp] (if (zero? (aget bs j)) j (recur (inc j))))
              mode (String. bs i (- sp i) "UTF-8")
              name (String. bs (inc sp) (- nul sp 1) "UTF-8")
              sha (hex bs (inc nul) 20)]
          (recur (+ nul 21)
                 (conj! acc {:mode mode :name name :sha sha
                             :type (cond (str/starts-with? mode "40") :tree
                                         (= mode "160000") :commit
                                         :else :blob)})))))))

(defn read-tree [b sha]
  (some-> (cat-file b sha) :bytes parse-tree))

(defn parse-commit [^bytes bs]
  (let [s (String. bs "UTF-8")
        [headers _] (str/split s #"\n\n" 2)
        lines (str/split-lines headers)]
    {:tree (some #(when (str/starts-with? % "tree ") (subs % 5)) lines)
     :parents (vec (keep #(when (str/starts-with? % "parent ") (subs % 7)) lines))}))

;; ---------------------------------------------------------------------------
;; refs and history

(defn refs
  "All refs: [{:name \"refs/heads/main\" :kind :branch|:remote|:tag :commit sha}]"
  [repo]
  (let [out (git* repo "for-each-ref"
                  "--format=%(refname)%00%(objectname)%00%(*objectname)%00%(objecttype)"
                  "refs/heads" "refs/remotes" "refs/tags")]
    (->> (str/split-lines (or out ""))
         (remove str/blank?)
         (keep (fn [line]
                 (let [[name obj peeled type] (str/split line #"\u0000" -1)
                       kind (cond (str/starts-with? name "refs/heads/") :branch
                                  (str/starts-with? name "refs/remotes/") :remote
                                  :else :tag)
                       commit (if (= type "tag") (not-empty peeled) obj)]
                   (when (and commit (not (str/ends-with? name "/HEAD"))
                              (or (not= type "tag") (not-empty peeled))
                              (not (and (= kind :tag) (not= type "commit") (empty? peeled))))
                     {:name name :kind kind :commit commit}))))
         vec)))

(defn current-branch [repo]
  (git* repo "symbolic-ref" "-q" "HEAD"))

(defn head-sha [repo]
  (git* repo "rev-parse" "-q" "--verify" "HEAD^{commit}"))

(defn default-branch
  "refs/heads/<x> for origin/HEAD's target, else main, else master, else current branch."
  [repo]
  (let [all (set (map :name (refs repo)))
        origin (some-> (git* repo "symbolic-ref" "-q" "refs/remotes/origin/HEAD")
                       (str/replace #"^refs/remotes/origin/" ""))]
    (or (some #(when (contains? all %) %)
              (cond-> [] origin (conj (str "refs/heads/" origin))
                      true (conj "refs/heads/main" "refs/heads/master")))
        (when origin (str "refs/remotes/origin/" origin))
        (current-branch repo))))

(defn remote-url [repo] (git* repo "config" "--get" "remote.origin.url"))

(defn log-commits
  "Commits reachable from `rev`, newest first in topo order. `n` nil = all.
   Returns [{:sha :parents [..] :author :email :time ms :subject}]"
  [repo rev n]
  (let [args (cond-> ["log" "--topo-order" "--format=%H%x00%P%x00%an%x00%ae%x00%ct%x00%s%x1e"]
               n (conj (str "-n" n))
               true (conj rev "--"))
        out (or (apply git* repo args) "")]
    (->> (str/split out #"\u001e")
         (map str/trim)
         (remove str/blank?)
         (mapv (fn [rec]
                 (let [[sha parents author email ct subject] (str/split rec #"\u0000" -1)]
                   {:sha sha
                    :parents (if (str/blank? parents) [] (str/split parents #" "))
                    :author author :email email
                    :time (* 1000 (or (parse-long (str/trim ct)) 0))
                    :subject subject}))))))

(defn git-dir
  "Absolute path of the repo's git dir (handles worktrees where .git is a file)."
  [repo]
  (some-> (git* repo "rev-parse" "--absolute-git-dir") str/trim))
