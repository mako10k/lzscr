# Copilot Refactoring Instructions

## 現在のタスク: main関数のリファクタリング

### 背景
- 現在のmain関数: 1105行（巨大すぎる）
- 目標: 200行程度（準備＋ルーティングのみ）
- 既に完了: エラー表示ヘルパー、フォーマットモード抽出

### 次のステップ: パースエラー表示の関数化

#### タスク詳細
ファイル: `crates/lzscr-cli/src/main.rs`

**実施内容:**

1. **新しい関数を追加** (handle_format_mode()の後に配置)

```rust
/// Handle parse error display with detailed caret positioning.
fn display_parse_error(
    code: &str,
    input_name: &str,
    msg: &str,
    span_offset: usize,
    span_len: usize,
) {
    eprintln!("parse error: {}", msg);
    
    // Opening '(' detection logic
    let mut open_paren_block: Option<String> = None;
    if let Some(idx) = msg.find("Opening '(' at line ") {
        let tail = &msg[idx + "Opening '(' at line ".len()..];
        let mut lchars = tail.chars();
        let mut line_s = String::new();
        for ch in lchars.by_ref() {
            if ch.is_ascii_digit() {
                line_s.push(ch);
            } else if ch == ':' {
                break;
            } else {
                line_s.clear();
                break;
            }
        }
        let mut col_s = String::new();
        for ch in lchars.by_ref() {
            if ch.is_ascii_digit() {
                col_s.push(ch);
            } else {
                break;
            }
        }
        if !line_s.is_empty() && !col_s.is_empty() {
            if let (Ok(_line), Ok(_col)) = (line_s.parse::<usize>(), col_s.parse::<usize>()) {
                let offset_match = tail.find("(offset ");
                if let Some(off_idx) = offset_match {
                    let off_tail = &tail[off_idx + "(offset ".len()..];
                    let mut off_s = String::new();
                    for ch in off_tail.chars() {
                        if ch.is_ascii_digit() {
                            off_s.push(ch);
                        } else {
                            break;
                        }
                    }
                    if let Ok(open_offset) = off_s.parse::<usize>() {
                        let open_caret = format_span_caret(code, input_name, open_offset, 1);
                        open_paren_block = Some(open_caret);
                    }
                }
            }
        }
    }
    
    // Main error location
    let caret = format_span_caret(code, input_name, span_offset, span_len);
    eprintln!("{}", caret);
    
    // Opening paren location if found
    if let Some(ob) = open_paren_block {
        eprintln!("\nOpening parenthesis:");
        eprintln!("{}", ob);
    }
}
```

2. **main関数内の置換対象** (約630-780行付近)

**現在のコード:**
```rust
let ast0 = match lzscr_parser::parse_expr(&code) {
    Ok(x) => x,
    Err(e) => {
        use lzscr_parser::ParseError;
        match e {
            ParseError::WithSpan { msg, span_offset, span_len } => {
                eprintln!("parse error: {}", msg);
                // ... 約150行のエラー表示ロジック ...
            }
            other => {
                eprintln!("parse error: {}", other);
            }
        }
        std::process::exit(2);
    }
};
```

**置き換え後:**
```rust
let ast0 = match lzscr_parser::parse_expr(&code) {
    Ok(x) => x,
    Err(e) => {
        use lzscr_parser::ParseError;
        match e {
            ParseError::WithSpan { msg, span_offset, span_len } => {
                display_parse_error(&code, &input_name, &msg, span_offset, span_len);
            }
            other => {
                eprintln!("parse error: {}", other);
            }
        }
        std::process::exit(2);
    }
};
```

3. **テスト実行**
```bash
cargo test --package lzscr-cli
```

4. **コミット**
```bash
git add crates/lzscr-cli/src/main.rs
git commit -m "refactor(cli): extract parse error display function

- Added display_parse_error() to handle ParseError::WithSpan
- Reduces main() by ~150 lines
- Consolidates opening paren detection logic
- All tests passing"
```

### 期待される効果
- main関数: 1105行 → 約950行（155行削減）
- パースエラー処理の一元化
- 可読性の向上

### 注意点
- `format_span_caret()` は既存の関数をそのまま使用
- エラー表示の動作は完全に同一に保つ
- std::process::exit(2) は維持

---

## 次の次のステップ（このタスク完了後）

### Phase 2: ソースレジストリ構築の関数化
- `build_source_registry()` 関数を抽出
- 約60行削減可能

### Phase 3: 型チェック処理の分離
- `handle_type_debug()` と `handle_type_inference()` を抽出
- 約300行削減可能

### Phase 4: サブコマンド分離
- `handle_eval_mode()`, `handle_dump_mode()`, `handle_analyze_mode()`
- 約400行削減可能

---

## 参考情報
- 静的解析レポート: `docs/static-analysis-report.md`
- リファクタリングプラン: 前回の会話で提供済み
- 現在のコミット: `8ac72e2`
