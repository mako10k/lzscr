# RCA 運用指示書

この文書はリポジトリ内で発生した問題について、RCA ツールを用いて観測・仮説・検証を記録・追跡するための手順をまとめたものです。

## 基本方針

- 仮説は「仮説」であることを明示して登録します（`provisional` として扱う）。
- 確定（finalize）するのは、十分な観測と検証が揃った後のみとします。
- 仮説や検証方法は積極的に RCA ツールへ記録してください。議論はツール上に残すことで再現性と追跡性が保たれます。

## 推奨ワークフロー（手順）

1. 問題発生時
   - CLI 等で再現コマンドを含む最小限の観測情報（コマンド、出力、作業ディレクトリ、終了コードなど）を収集します。
   - RCA ケースを作成します（例: `mcp_rca_case_create` を使用）。

2. 観測を追加
   - 収集した観測を `mcp_rca_observation_add` を用いて RCA ケースに追加します。ログの抜粋や失敗トレースはそのまま保存してください。

3. 仮説の提案（provisional）
   - 可能性のある原因を `mcp_rca_hypothesis_propose` で案として登録します。必ず「これは仮説であり未確定である」ことを記載してください。
   - 例: “トップレベルの関数定義文法でカンマ/閉括弧が期待されるため構文エラーが出る”

4. 検証計画の作成
   - 各仮説に対して検証方法（手順、期待する指標/出力、再現コマンド）を `mcp_rca_test_plan` で登録します。
   - 優先度を決め、簡単に試せる低コストな検証から実施します。

5. 検証の実行と結果の記録
   - 検証を実行し、結果（成功/失敗・ログ・差分）を RCA に記録します（`mcp_rca_test_plan_update` / ケースの結果フィールドへ）。

6. 仮説の確定
   - 十分な証拠が得られた仮説は `mcp_rca_hypothesis_finalize` を使って確定してください。
   - 根本原因と対処（fix）およびフォローアップを `mcp_rca_conclusion_finalize` で記述します。

7. クリーンアップ
   - 不要になった provisional な仮説/検証案は `mcp_rca_hypothesis_remove` / `mcp_rca_test_plan_remove` で整理できますが、削除前に brief な理由を残してください。

## 具体例（コマンドの雛形）

- RCA ケース作成:
  - `mcp_rca_case_create` (severity=SEV2 など、title を付与)
- 観測追加:
  - `mcp_rca_observation_add` (what: エラーメッセージ、コマンド、cwd)
- 仮説提案:
  - `mcp_rca_hypothesis_propose` (text: 仮説の要約、context: 詳細)
- 検証プラン作成:
  - `mcp_rca_test_plan` (method: コマンド手順、expected: 期待される信号)
- 仮説確定:
  - `mcp_rca_hypothesis_finalize`
- 結論とフォローアップ:
  - `mcp_rca_conclusion_finalize`

## 注意事項

- 記録は簡潔に、しかし再現に十分な情報を含めてください（ログ抜粋、コマンド、環境）。
- 仮説は遠慮なく作成してください — ただし "確定" のフラグを付けるのは検証が終わるまで避けてください。

---

上記の方針に沿って、発生中の `parse error running tests/prelude_basic.lzscr` ケースにも同様の手順で観測・仮説・検証を追加していってください。
