#!/usr/bin/env python3
"""Simple web frontend for Emacs log analysis"""

from flask import Flask, render_template, request, jsonify
import subprocess
import json
import os

app = Flask(__name__)
MCP_SERVER = "../target/release/emacs-mcp-server"


def mcp_call(method, params, request_id=1):
    """Make an MCP call to the Emacs server"""
    req = {"jsonrpc": "2.0", "method": method, "params": params, "id": request_id}
    result = subprocess.run(
        [MCP_SERVER], input=json.dumps(req), capture_output=True, text=True
    )
    return json.loads(result.stdout)


@app.route("/")
def index():
    """Main upload page"""
    return render_template("index.html")


@app.route("/analyze", methods=["POST"])
def analyze():
    """Analyze uploaded log file"""
    if "logfile" not in request.files:
        return jsonify({"error": "No file uploaded"}), 400

    file = request.files["logfile"]
    temp_path = f"/tmp/log_{os.urandom(8).hex()}.txt"
    file.save(temp_path)

    try:
        # Open log file in Emacs
        mcp_call("tools/call", {"name": "open-file", "arguments": {"path": temp_path}})

        # Insert a marker comment at the beginning
        mcp_call(
            "tools/call",
            {
                "name": "insert",
                "arguments": {
                    "text": ";; Log Analysis Report\n;; File analyzed by Emacs MCP Server\n;;\n"
                },
            },
        )

        return jsonify(
            {
                "status": "success",
                "message": "Log file opened in Emacs with analysis markers.",
                "file": temp_path,
            }
        )
    except Exception as e:
        return jsonify({"error": str(e)}), 500


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000, debug=True)
