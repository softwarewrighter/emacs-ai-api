#!/usr/bin/env python3
"""Remote pair programming with shared Emacs session"""

from flask import Flask, render_template, request, jsonify
import subprocess
import json

app = Flask(__name__)
MCP_SERVER = "../target/release/emacs-mcp-server"


@app.route("/")
def index():
    """Pair programming interface"""
    return render_template("index.html")


@app.route("/open", methods=["POST"])
def open_file():
    """Open a file in shared Emacs session"""
    file_path = request.json.get("path")

    try:
        result = mcp_call(
            "tools/call", {"name": "open-file", "arguments": {"path": file_path}}
        )
        return jsonify({"status": "success", "result": result})
    except Exception as e:
        return jsonify({"error": str(e)}), 500


@app.route("/insert", methods=["POST"])
def insert_text():
    """Insert text at cursor"""
    text = request.json.get("text")

    try:
        result = mcp_call("tools/call", {"name": "insert", "arguments": {"text": text}})
        return jsonify({"status": "success"})
    except Exception as e:
        return jsonify({"error": str(e)}), 500


@app.route("/split", methods=["POST"])
def split_window():
    """Split window for side-by-side viewing"""
    direction = request.json.get("direction", "horizontal")

    try:
        result = mcp_call(
            "tools/call", {"name": "split-window", "arguments": {"direction": direction}}
        )
        return jsonify({"status": "success"})
    except Exception as e:
        return jsonify({"error": str(e)}), 500


def mcp_call(method, params, request_id=1):
    """Make an MCP call"""
    req = {"jsonrpc": "2.0", "method": method, "params": params, "id": request_id}
    result = subprocess.run(
        [MCP_SERVER], input=json.dumps(req), capture_output=True, text=True
    )
    return json.loads(result.stdout)


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5001, debug=True)
