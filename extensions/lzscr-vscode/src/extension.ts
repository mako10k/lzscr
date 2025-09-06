import * as cp from 'child_process';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import * as vscode from 'vscode';

async function runFormatterWithTempFile(cmd: string, input: string, cwd?: string, indent?: number, width?: number): Promise<string> {
  const dir = await fs.promises.mkdtemp(path.join(os.tmpdir(), 'lzscr-fmt-'));
  const tmpPath = path.join(dir, 'input.lzscr');
  await fs.promises.writeFile(tmpPath, input, 'utf8');
  return new Promise((resolve, reject) => {
  const args = ['--format-code', '--file', tmpPath];
  if (typeof indent === 'number') args.push('--fmt-indent', String(indent));
  if (typeof width === 'number') args.push('--fmt-width', String(width));
    const child = cp.spawn(cmd, args, { cwd });
    let stdout = '';
    let stderr = '';
  child.stdout.on('data', (d: any) => (stdout += d.toString()));
  child.stderr.on('data', (d: any) => (stderr += d.toString()));
    child.on('error', reject);
    child.on('close', (code: number | null) => {
      // cleanup tmp dir best-effort
      fs.promises.rm(dir, { recursive: true, force: true }).catch(() => {});
      if (code === 0) resolve(stdout);
      else reject(new Error(stderr || `formatter exited with code ${code}`));
    });
  });
}

export function activate(context: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration('lzscr');
  const formatterPath = config.get<string>('formatterPath', 'lzscr-cli');
  const maxWidth = config.get<number>('maxWidth', 100);

  const formatDoc = async (document: vscode.TextDocument) => {
    const text = document.getText();
    try {
      const cwd = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
  const formatted = await runFormatterWithTempFile(formatterPath, text, cwd, config.get<number>('indent', 2), maxWidth);
      const fullRange = new vscode.Range(
        document.positionAt(0),
        document.positionAt(text.length)
      );
      return [vscode.TextEdit.replace(fullRange, formatted.replace(/\n$/, ''))];
    } catch (err: any) {
      vscode.window.showErrorMessage(`lzscr format failed: ${err?.message || err}`);
      return [];
    }
  };

  context.subscriptions.push(
    vscode.languages.registerDocumentFormattingEditProvider({ language: 'lzscr' }, {
      provideDocumentFormattingEdits: formatDoc,
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('lzscr.formatDocument', async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) return;
      const edits = await formatDoc(editor.document);
      await editor.edit((builder: vscode.TextEditorEdit) => {
        for (const e of edits) builder.replace(e.range, e.newText);
      });
    })
  );
}

export function deactivate() {}
