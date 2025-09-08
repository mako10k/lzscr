# Branch Protection (Recommended)

Configure GitHub branch protection rules for `main` with these settings:

- Require a pull request before merging
  - Require approvals: 1+ (suggested: 2 for larger teams)
  - Dismiss stale reviews on new commits
  - Require review from Code Owners
- Require status checks to pass before merging
  - build (Rust build/tests)
  - vsix (VS Code extension packaging)
  - audit/deny (optional, non-blocking initially)
- Require branches to be up to date before merging
- Restrict who can push to matching branches (maintainers only)
- Do not allow bypassing the above rules
- Require signed commits (optional)

## Apply via GitHub CLI

Replace <owner> and run these once (needs repo admin permissions):

```bash
# 1) Create rule for main
gh api \
  -X PUT \
  -H "Accept: application/vnd.github+json" \
  /repos/<owner>/lzscr/branches/main/protection \
  -f required_status_checks.strict=true \
  -f required_status_checks.contexts[]='build' \
  -f required_status_checks.contexts[]='vsix' \
  -f enforce_admins=true \
  -f required_pull_request_reviews.required_approving_review_count=1 \
  -f required_pull_request_reviews.dismiss_stale_reviews=true \
  -f required_pull_request_reviews.require_code_owner_reviews=true \
  -f restrictions=null

# 2) Enforce linear history
gh api \
  -X PATCH \
  -H "Accept: application/vnd.github+json" \
  /repos/<owner>/lzscr \
  -f allow_merge_commit=false -f allow_rebase_merge=true -f allow_squash_merge=true
```

Notes:
- Update the contexts list with exact job names from CI if different.
- If you prefer UI: Settings → Branches → Branch protection rules → Add rule.
