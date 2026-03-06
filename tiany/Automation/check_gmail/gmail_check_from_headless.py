#!/usr/bin/env python3
"""
Headless-friendly Gmail API search:
- Console-based OAuth flow on first run (prints a URL and asks for a code)
- Stores/uses token.json for subsequent headless runs
- Searches by sender / subject / date / unread flags (Gmail query syntax)
"""

import argparse
import os
import sys
from typing import Dict, List

from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

# Minimal read-only scope
SCOPES = ["https://www.googleapis.com/auth/gmail.readonly"]

def build_query(args) -> str:
    parts = []
    if args.from_addr:
        parts.append(f'from:"{args.from_addr}"')
    if args.subject:
        parts.append(f'subject:"{args.subject}"')
    if args.unread:
        parts.append('is:unread')
    if args.after:
        parts.append(f'after:{args.after.replace("-", "/")}')
    if args.before:
        parts.append(f'before:{args.before.replace("-", "/")}')
    if args.query:
        parts.append(args.query)
    return " ".join(parts).strip()

def get_service(credentials_file: str, token_file: str):
    """
    Returns an authorized Gmail API service object.
    Uses token_file if available; otherwise runs console flow and saves token.
    """
    creds = None
    if os.path.exists(token_file):
        creds = Credentials.from_authorized_user_file(token_file, SCOPES)

    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            try:
                creds.refresh(Request())
            except Exception:
                creds = None  # fall back to full flow
        if not creds:
            if not os.path.exists(credentials_file):
                print(f"Missing credentials file: {credentials_file}", file=sys.stderr)
                sys.exit(2)
            # Console flow (works on headless servers): prints URL, you paste code back
            flow = InstalledAppFlow.from_client_secrets_file(credentials_file, SCOPES)
            # If this fails in your environment (rare), authorize on a machine with a browser
            # using run_local_server() there, then copy token.json to this server.
            #creds = flow.run_console()  # headless-friendly
            creds = flow.run_local_server()  # headless-friendly
        with open(token_file, "w") as f:
            f.write(creds.to_json())
        # Lock down permissions (best effort on Unix)
        try:
            os.chmod(token_file, 0o600)
        except Exception:
            pass

    return build("gmail", "v1", credentials=creds)

def header_map(headers: List[Dict[str, str]]) -> Dict[str, str]:
    return {h.get("name", ""): h.get("value", "") for h in headers or []}

def main():
    parser = argparse.ArgumentParser(description="Search Gmail using the Gmail API (headless-friendly).")
    parser.add_argument("--from", dest="from_addr", help='Sender email or domain (e.g., "user@example.com" or "example.com")')
    parser.add_argument("--subject", help='Subject contains (quoted if spaces)')
    parser.add_argument("--unread", action="store_true", help="Only unread messages")
    parser.add_argument("--after", help="After date (YYYY-MM-DD or YYYY/MM/DD)")
    parser.add_argument("--before", help="Before date (YYYY-MM-DD or YYYY/MM/DD)")
    parser.add_argument("--query", help="Extra Gmail query (e.g., 'has:attachment label:important')")
    parser.add_argument("--max", type=int, default=25, help="Max messages to list (1..500)")
    parser.add_argument("--credentials", default="credentials.json", help="Path to OAuth client file (default: credentials.json)")
    parser.add_argument("--token", default="token.json", help="Path to saved token (default: token.json)")
    args = parser.parse_args()

    if not args.from_addr and not args.query:
        print("Provide --from or --query (or both). Try --help for examples.", file=sys.stderr)
        sys.exit(2)

    query = build_query(args)
    print(f"Query: {query}\n")

    service = get_service(args.credentials, args.token)

    resp = service.users().messages().list(
        userId="me",
        q=query,
        maxResults=min(max(1, args.max), 500)
    ).execute()

    msgs = resp.get("messages", [])
    if not msgs:
        print("No matching messages found.")
        return

    print(f"Found {len(msgs)} message(s):\n")
    for i, m in enumerate(msgs, 1):
        meta = service.users().messages().get(
            userId="me",
            id=m["id"],
            format="metadata",
            metadataHeaders=["From", "Subject", "Date"]
        ).execute()

        headers = header_map(meta.get("payload", {}).get("headers", []))
        from_h = headers.get("From", "(no From)")
        subj = headers.get("Subject", "(no Subject)")
        date_h = headers.get("Date", "(no Date)")
        snippet = (meta.get("snippet") or "").replace("\n", " ").strip()
        link = f"https://mail.google.com/mail/u/0/#inbox/{m['id']}"

        print(f"{i:2d}. Date:    {date_h}")
        print(f"    From:    {from_h}")
        print(f"    Subject: {subj}")
        if snippet:
            print(f"    Snippet: {snippet[:400]}")
        print(f"    Link:    {link}")
        print("-" * 70)

if __name__ == "__main__":
    main()
