## lzscr 型システム 設計・実装計画（MVP: 暗黙HM推論）

本ドキュメントは、注釈なし（暗黙）の Hindley–Milner 風（ランク1）型推論を導入するための仕様案・実装計画・検討事項をまとめる。

### 1) 目的とスコープ

- 目的: 実行前に型誤りを検出し、エラーメッセージ品質を高める。既存の言語機能（例外、パターン、LetRec、記号コンストラクタ、AltLambda/OrElse）を型整合させる。
- スコープ: ランク1のHM推論（多相 let を含む）。注釈は不要。Kind/効果（Pure/IO）は後続フェーズで統合。

### 2) 主要データ構造

- Type（式の型）
  - Unit | Int | Float | Bool | Str
  - Var(TvId)
  - Fun(Box<Type>, Box<Type>)
  - List(Box<Type>) | Tuple(Vec<Type>) | Record(Vec<(String, Type)>)
  - Ctor { tag: String, payload: Vec<Type> }（同一 tag 同士のみ単一化可能）
  - Dyn（未確定/将来拡張用の逃げ道。MVPでは原則使わない）
- Scheme（多相スキーム）: Forall(Vec<TvId>, Type)
- Subst: HashMap<TvId, Type>
- Constraint: (Type, Type, Span) — 発生位置を保持
- 型環境: HashMap<Name, Scheme>

注: Record は MVP では「閉じたレコード」（キー集合は一致必須）。将来 row-polymorphism を検討。

### 3) 単一化（unify）

- 発生チェック（occurs check）あり。
- 基本型/関数/タプル/リスト/レコードは構造一致で単一化。
- Ctor は tag と payload 長一致で単一化。tag 不一致は即エラー。
- エラー時には両型と発生 Span を報告。

### 4) 推論アルゴリズム（Algorithm W 準拠）

- infer_expr(env, e) -> (Type, Subst)
- instantiate: Scheme → Type（型変数を新鮮化）
- generalize: Env × Type → Scheme（Env に自由でない型変数を束縛）
- LetGroup（LetRec）:
  - 同一グループの全束縛に新鮮な仮型 Var を割当て環境へ先置。
  - 各 RHS を推論→単一化して仮型を具体化。
  - 一般化して環境へ再登録。
  - ただし「非関数の自己再帰」は occurs check により拒否される可能性あり（MVP の制約、要検討）。

### 5) 構文ごとの型規則（要点）

- リテラル: 既知の具体型。
- 参照 ~x: 環境から Scheme を取り出して instantiate。
- ラムダ \\pat -> body:
  - pat からパターン型制約を生成（下記「パターン推論」）。
  - 本体を推論して Fun(param, body) を構成。
- 適用 f a: f: α -> β, a: α を要求し β。
- リスト [e1,..,en]: 要素型を単一化。[] は List α（未定）として導入し、利用側で解決。未解決なら曖昧性エラー。
- cons h : t: h: a, t: List a ⇒ List a。
- タプル/レコード: 構造的に型構成。レコードはキー集合一致必須。
- 記号/コンストラクタ:
  - Symbol S はコンストラクタ関数として扱う: Scheme S: ∀a1..an. a1 -> .. -> an -> Ctor<S,[a1..an]>
  - arity は analyzer/設定の ctor_arity（もし提供されるなら）と整合性検証。
- 例外:
  - Raise e: e: γ、式の戻りは r（新鮮）として扱う（ボトム相当）。
  - Catch: left: r, right: γ -> r。
- OrElse ‘||’: left: r, right: r ⇒ r。
- AltLambda ‘|’: left/right: a -> r ⇒ 全体 a -> r。

#### パターン推論（Pattern ⇒ (Bindings, Constraints)）

- ~x は新鮮型 α を x に束縛。
- Ctor(name, args): scrutinee を Ctor<name, [α1..αn]> に単一化、各 arg を再帰推論。
- Cons(h:t): scrutinee は List a、h:a, t:List a。
- List, Tuple, Record: 形状一致で再帰推論。
- As(a @ b): 同一 scrutinee で a と b の制約を合流。

### 6) ビルトインの型（例）

- add/sub/mul/div: Int -> Int -> Int
- fadd/fsub/fmul/fdiv: Float -> Float -> Float
- eq/ne: ∀a. a -> a -> Bool
- lt/le/gt/ge: Int -> Int -> Bool、flt/fle/fgt/fge: Float -> Float -> Bool
- cons: ∀a. a -> List a -> List a
- to_str: ∀a. a -> Str
- seq: ∀a b. a -> b -> b
- alt: ∀a r. (a->r) -> (a->r) -> a -> r

（実装は `types::builtins` に集約、環境初期化時に登録。）

### 7) コンパイルパイプライン統合

- 既存: parse → analyzer → eval
- 変更: parse → analyzer → typecheck（本ドキュメントの推論器）→ OK なら eval
- CLI: 既定で有効。`--no-typecheck` で無効化可能。`--types=pretty|json` の出力オプションを将来追加。

### 8) エラーと診断

- 型不一致（期待/実際）: 位置（Span）、型のダンプ（可能なら短文化）
- 発生チェック違反（無限型）: 自己参照の箇所を示す
- 未解決型（曖昧）: 例）空リストの要素型が特定不能
- レコードキー不一致/重複
- コンストラクタ tag 不一致/arity 不整合
- OrElse/AltLambda/例外まわりの整合失敗（期待関数型など）

### 9) テスト計画（最小）

- 算術/比較/文字列/リスト/タプル/レコードのハッピーパス
- λ/適用/let/letrec（id, const, compose, map）
- 例外: Raise/Catch の型整合、AltLambda/OrElse の規則
- コンストラクタ: 同一 tag の統一、異種 tag のエラー
- 曖昧性/発生チェックのエラーメッセージ

### 10) 実装フェーズ

- Phase 0: クレート雛形（`lzscr-types`）、Type/Scheme/Subst/Unify、型表示/短縮プリンタ
- Phase 1: 式推論（リテラル/Ref/λ/適用/タプル/リスト/cons/OrElse/AltLambda）
- Phase 1.5: LetGroup（プレースホルダ→単一化→一般化）。自己再帰は関数に限定（MVP）。
- Phase 2: Record/Constructor 型、ctor_arity 連携
- Phase 3: Raise/Catch の型付け、曖昧性検出
- Phase 4: CLI 連携、JSON 出力、診断改善

### 11) 要検討事項（オープン）

- 値制限（value restriction）: 効果導入時/参照導入時に再検討
- 非関数再帰の型付け: サンクにより実行は可能だが、推論では無限型問題。MVP は「関数に限定」または注釈要求。
- レコードの拡張性: row polymorphism の導入タイミング
- コンストラクタ変数の静的宣言: 暗黙導入 vs 宣言型（将来のADT導入時）
- 空リスト/空レコード/単独シンボルの曖昧性: 注釈/使用点で解消できない場合の扱い
- 過載せ/数値リテラル多相: 既存の分離（.+ 等）で回避済だが、将来的な overloading 方針
- 効果/Kind の統合: Pure/IO の検証を型検査に取り込む段取り
- IR レベルでの推論: 最適化/バックエンド前段での利点とコスト
