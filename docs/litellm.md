Skip to content
Chat history

You said:
I want a command line AI coding tool that can use both local LLMs and cloud LLMs.  Specifically I'd like to use Deepseek and Kimi-K2 on local hardware and use GPT-5 via the cloud.  I've been using Claude Code mainly but that is limited to Claude's own LLMs.   I've tried Google's Gemini CLI but it is not powerful enough.  I've tried opencode (and in fact I forked it and converted it to Rust) so I can use local and cloud LLMs via that.  However, I'm concerned about my monthly budget and also the complexity of managing so many cloud providers and their different usage plans (low, medium, high usage limits billed monthly) and the API token approach paying per million tokens or equivalent.   I already have some IDE integration going on with JetStream's offerings but I prefer working in Emacs and at the CLI.   So, I need a way to compare the different choices that work with local and cloud providers mostly in the CLI (TUI, or preferably in a way that works "inside" emacs).   I looked at CodeLLM but that seems like an add-on $10/month charge to do what I can do for free with Opencode or my Rust clone of it.   I'm losing track of which plans I've signed up for, which APIs I have credits for, etc.  I believe that GPT-5 is best of my choices, but I don't have a good understanding of how much it will end up using of my monthly budget if I'm paying for the API at a usage-based rate.  I need recommendations of the GPT-5 using plans and offerings and a clear estimate of costs of using it for hours every day.
ChatGPT said:

Thought for 5m 15s
Here’s a pragmatic, Emacs-first plan you can actually run with today.

Restate the goal
You want a CLI/Emacs coding companion that:

uses local LLMs (DeepSeek, Kimi-K2) for cheap/fast/offline work,

uses cloud LLMs (GPT-5) when quality matters,

keeps spend visible and capped, and

stays simple to operate (no provider sprawl or token/key chaos).

Architecture (keep it boring + observable)
One gateway, many models. Run a small local LLM gateway that exposes a single OpenAI-compatible endpoint and routes to:

OpenAI API → GPT-5 / GPT-5 mini for best results, with per-project caps. 
OpenAI

llama.cpp server (or Ollama) for DeepSeek Coder V2 and Kimi-K2 quantized GGUFs. 
GitHub
+1
Hugging Face
+1

Use LiteLLM Proxy as the gateway because it’s OpenAI-format compatible, supports OpenAI, Ollama, llama.cpp, exposes one base URL, and adds budgets/limits & spend tracking. (Provider/model/team budgets + per-key rate limits are supported; you can set daily/monthly caps.) 
LiteLLM
+6
LiteLLM
+6
LiteLLM
+6

For Emacs, use gptel—it speaks to OpenAI, OpenRouter, Ollama, llama.cpp, Deepseek, etc., so your Emacs commands don’t change when you switch models. 
Gptel
NonGNU ELPA

Optional: If you want a hosted aggregator to avoid juggling keys, OpenRouter also presents a single OpenAI-compatible endpoint (and has GPT-5), but adds ~5% fee on usage—only worth it if managing keys is the bottleneck. 
OpenRouter
+1
The Wall Street Journal

High-level design
Gateway (LiteLLM Proxy)

Models: gpt-5, gpt-5-mini, deepseek-coder-v2 (ollama or llama.cpp), kimi-k2 (llama.cpp).

Budgets: set provider and model budgets; create virtual keys with per-key caps. 
LiteLLM
+1

Local inference

DeepSeek Coder V2 GGUF (great code model), Kimi-K2 GGUF for reasoning/coding, both via llama.cpp server (OpenAI-compatible). Kimi-K2 support landed upstream / via Unsloth; follow their note if you need updated chat template. 
Hugging Face
GitHub
+1
Unsloth Docs

Editor & CLI

Emacs gptel points at the gateway base URL; your opencode-rs CLI uses the same. 
Gptel

Observability

Use LiteLLM’s spend tracking & per-team budgets; if you want a richer dashboard, Helicone plugs into OpenAI-format APIs for cost monitoring/comparison. 
LiteLLM
+1
Helicone.ai

Cloud spend guardrails

In OpenAI Billing, set a monthly budget + alert thresholds; note ChatGPT subscriptions don’t include API credits. 
OpenAI
+1

Non-functional requirements
Cost control: hard caps (OpenAI billing), soft caps (LiteLLM budgets/keys). 
OpenAI
LiteLLM

Simplicity: single endpoint for all models; one Emacs backend. 
LiteLLM
Gptel

Privacy/offline: default to local models for refactors/grep-like tasks.

Latency: use local for short iterations; escalate to GPT-5 selectively.

Repeatability: enable prompt caching so repeated system/context tokens are billed at the cached input rate. 
OpenAI

Low-level design (minimal config you can drop in)
1) LiteLLM config.yaml (snippet)
model_list:
  - model_name: gpt-5
    litellm_params:
      model: openai/gpt-5
  - model_name: gpt-5-mini
    litellm_params:
      model: openai/gpt-5-mini

  # Local DeepSeek via Ollama OR llama.cpp (pick one)
  - model_name: deepseek-coder-v2
    litellm_params:
      model: ollama/deepseek-coder-v2
      api_base: http://127.0.0.1:11434

  # Local Kimi-K2 via llama.cpp server
  - model_name: kimi-k2
    litellm_params:
      # llama.cpp exposes OpenAI-compatible /v1 endpoints
      model: openai/kimi-k2
      api_base: http://127.0.0.1:8080  # llama.cpp --server -p 8080
general_settings:
  master_key: "sk-<your-proxy-admin-key>"
LiteLLM budgets you’ll likely want on day one:

Provider budget (e.g., OpenAI ≤ $3/day),

Model budget (e.g., gpt-5 ≤ $2/day),

Team/key budget (per-project cap). 
LiteLLM
+1

2) Emacs (gptel) pointing at the proxy
;; gptel from GNU ELPA / MELPA
(require 'gptel)
;; Gateway (LiteLLM) as default
(gptel-make-openai "gateway"
  :host "127.0.0.1:4000" :endpoint "/v1" :protocol "http"
  :key (getenv "LITELLM_MASTER_KEY") :stream t)
(setq gptel-backend (gptel-get-backend "gateway"))
(setq gptel-model "gpt-5") ; C-c C-m to switch models interactively
gptel supports OpenAI, Ollama, llama.cpp, DeepSeek, OpenRouter, etc., so switching models is trivial. 
Gptel

3) Local models
DeepSeek Coder V2 Instruct (GGUF) → proven quantizations exist. Run with llama.cpp or Ollama. 
Hugging Face

Kimi-K2 Instruct (GGUF) → Unsloth provides quants and a “how to run locally” with updated llama.cpp support. 
Hugging Face
Unsloth Docs

llama.cpp server exposes OpenAI-compatible /v1—no client code changes. 
GitHub

GPT-5 plans, pricing & what it really costs per day
API pricing (pay-as-you-go):

GPT-5: $1.25 / 1M input tokens, $10 / 1M output tokens; cached input: $0.125 / 1M; 400k context, 128k max output. 
OpenAI
+1

GPT-5 mini: $0.25 / 1M input, $2 / 1M output (use for cheap/light work). 
OpenAI

Web-search tool calls: for GPT-5, search content is billed at model rates and has a $10/1k calls line item—avoid auto-search if you’re budget-sensitive. 
OpenAI

Important: ChatGPT Plus/Pro/Team do not include API credits—API is billed separately. Set a monthly API budget in the dashboard. 
OpenAI

Rough, realistic coding usage (no search tool, no images):
Cost per hour = (input_tokens/1e6)*1.25 + (output_tokens/1e6)*10.

Profile (per hour)	Input tokens	Output tokens	$/hour (GPT-5)
Typical (20 turns × 2k in / 1.5k out)	40,000	30,000	$0.35
Heavy (10 turns × 10k in / 5k out)	100,000	50,000	$0.625
Very heavy (6 turns × 25k in / 15k out)	150,000	90,000	$1.0875
Monthly ballpark (30 days):

2 h/day → $21 – $65 (typical → very heavy).

4 h/day → $42 – $131.

8 h/day → $84 – $261.

If you default to GPT-5 mini for light edits and only escalate to GPT-5 for tricky diffs/tool-use, your effective $/hour often drops by 3–5× (because mini’s output is $2/M vs $10/M). 
OpenAI

Using prompt caching for stable system/repo context can cut input costs by 10× (to $0.125/M). Outputs still dominate for long answers, so also train your prompts/agents to keep outputs tight unless you explicitly ask for verbose diffs. 
OpenAI

If you route GPT-5 via OpenRouter for convenience, add ~5% to the above numbers. 
The Wall Street Journal

Approach & tools (with pros/cons)
A. LiteLLM Proxy (recommended single endpoint)

Pros: one base URL; supports OpenAI + Ollama/llama.cpp; budgets/keys/rate-limits; works great with Emacs/gptel and your Rust CLI. 
LiteLLM

Cons: one more daemon to run; some “advanced” budget features are enterprise-gated (basic budgets exist via API). 
LiteLLM

B. Direct OpenAI API only

Pros: fewer moving parts; set hard monthly cap in OpenAI dashboard. 
OpenAI

Cons: you still need separate code paths for local models; no unified observability across providers.

C. Hosted aggregator (OpenRouter)

Pros: key management and model catalog in one place; Emacs works via OpenAI format. 
OpenRouter

Cons: fee overhead; still configure locals separately.

D. Emacs packages

gptel (flexible, model-agnostic). 
Gptel

ellama, Ollama-buddy for pure local flows. 
GitHub
elpa.gnu.org
dyerdwelling.family

Step-by-step: what I’d do in your shoes
Stand up llama.cpp server for DeepSeek Coder V2 GGUF and Kimi-K2 GGUF (per Unsloth guide; update chat template if needed). 
Hugging Face
+1
Unsloth Docs

Run LiteLLM Proxy with a config.yaml like above; export OPENAI_API_KEY for GPT-5. 
LiteLLM

Set budgets:

OpenAI dashboard → monthly cap + alerts.

LiteLLM → provider/model/team budgets + a dev virtual key with a daily cap. 
OpenAI
LiteLLM
+1

Point Emacs gptel to the proxy, default model = gpt-5-mini, bind a key to switch to gpt-5 for “hard mode”. 
Gptel

Teach your CLI/agents a routing policy:

small edits/search → local DeepSeek/Kimi,

refactor w/ tests or tool-use → GPT-5 mini,

failing tests/ambiguous bugs/critical merges → GPT-5.

turn on prompt caching for stable context. 
OpenAI

(Optional) Add Helicone for a live cost dashboard across providers. 
Helicone.ai

Final recommendations (short list)
Use OpenAI API (pay-as-you-go) for GPT-5/GPT-5 mini; set a monthly cap in Billing. 
OpenAI

Front everything with LiteLLM Proxy to unify clouds + locals and enforce budgets per model/provider/key. 
LiteLLM
+1

In Emacs, use gptel with the proxy; keep your default model = GPT-5 mini, escalate to GPT-5 on demand. 
Gptel

For locals, run DeepSeek Coder V2 (GGUF) and Kimi-K2 (GGUF) via llama.cpp server for drop-in OpenAI compatibility. 
Hugging Face
GitHub
+1
Unsloth Docs

Expect ~$0.35–$1.09 per hour for typical-to-heavy GPT-5 coding sessions; $42–$261 per 4–8h/day month depending on verbosity/turn size. Use mini + caching to cut this by multiples. 
OpenAI

If you want, I can draft:

a drop-in config.yaml with budgets and routes tuned to your boxes,

a gptel snippet with convenient model-switch keybindings, and

a tiny Rust “router” module for your opencode fork that sends work to the right model given size/risk.

You said:
This is helpful and I see actionable items, but it hasn't demystified what I am currently paying for (e.g. I'm on the Pro plan) and how I can leverage that for CLI coding agent like Claude Code does (especially with regards to MCP tool use, like playwright and my own custom Rust-based MCP servers.   Here's my take: 
1. Claude Code gives pretty good results at Max plan $200/month with higher caps that I don't run into frequently like I did at Pro plan $100/month.  But I want better results, so I wouldn't have to iterate so often to correct its mistakes
2. Gemini CLI is "unlimited" free (no plan) but it actually throtles or downgrades quite quickly and often, and requires much more hand-holding, and repitition, and correction than Claude Code  (but is free--though they should be paying me to essentially help them improve it).
3. Google Jules works great, is free, but not the worfflow I want.
4. Opencode (or my Rust clone) can burn through credits at a higher rate than you estimated.  I've spent $75 in one day just paying as you go for tokens (I had to reload my credits with $25 three times in one day).  This is not cost-effective nor low-friction.
5. Claude Desktop, ChatGPT web., Claude web, Deepseek Web, Kimi-K2 web are all useful for architecture, PRDs, designs, coding, testing, documentation, etc., but the wrong UI for me.  I do use Gemini Web a lot for non-coding tasks, as it is convenient and low-cost/free-ish
6. Ollama - my take is that any model that runs on Ollama and fits in a 8GB to 12GB GPU is not good enough to generate working code without a lot of iterations corrections.
7. llama.cpp - running Deepseek, Qwen3. or Kimi K2 small-to-medium quants on larger but older GPUs is adequate and helpful using the approach you recommend, but these are slow (not too slow, just slower than paid alternatives).  They are cheaper when you consider my high electric rates, but not very much cheaper.
8. likewise for vllm, docker-based TTS models, etc. these work for non-coding tasks (haven't used vllm much yet).
So, what I'm looking for is the most cost-effective per month plan based on high/heavy daily usage.  I get the impression that Deepseek and KimiK2 are cheaper than ChatGPT (but also not as good, but better than the alternatives or on par with some Claude models).
I hate that (a) providers often switch me to cheaper/slower/dumber models mid project or that (b) I have to carefully switch my target LLM based on the task that I'm trying to accomplish.  LLMs should be good at managing this for me.
ChatGPT said:

Thought for 3m 4s
Got it — let’s demystify what you’re paying for, map a Claude-Code-style CLI/Emacs setup that speaks MCP, and pin down a monthly cost plan that won’t surprise you.

Restated requirements
Heavy, daily coding at the CLI/inside Emacs, with local (DeepSeek/Kimi-K2) + cloud (GPT-5).

MCP tool use (Playwright + your Rust MCP servers).

Minimal micromanagement of models; routing should be automatic.

Predictable monthly spend (you’ve seen $75/day spikes and want guardrails).

What your current OpenAI plan actually buys you
ChatGPT Pro ($200/mo): “unlimited” GPT-5 inside the ChatGPT apps (subject to abuse guardrails) — good for browser/desktop use, but Pro does not include API credits. API usage is separate, pay-as-you-go. 
OpenAI
+1
OpenAI Help Center

ChatGPT Team ($25–30/user/mo): team workspace; also “virtually unlimited” GPT-5 in ChatGPT (again, UI only). Still no API credits. (You need 2+ seats, so not ideal solo.) 
OpenAI Help Center
+1
OpenAI
+1

API pricing (pay-as-you-go): GPT-5 $1.25/M input, $10/M output, cached input $0.125/M; GPT-5-mini is $0.25/M in, $2/M out. These rates drive your CLI/Emacs agent costs. 
OpenAI Platform
+1
OpenAI

Implication: Your Pro sub is great for the ChatGPT UI, but all CLI/Emacs/automation costs come from the API meter. If your CLI work dominates, plan around API budgets, not Pro.

Design: Claude-Code feel on the CLI/inside Emacs (with MCP)
Architecture (one endpoint, auto-routed):

LiteLLM Proxy as a local “gateway” exposing an OpenAI-compatible /v1 endpoint. It routes, rate-limits, and enforces provider/model budgets; supports cost-based routing + fallbacks so you don’t hand-switch models. 
LiteLLM
+4
LiteLLM
+4
LiteLLM
+4

Local inference behind the gateway: llama.cpp --server for DeepSeek Coder V2 and Kimi-K2 (GGUF) — both appear as OpenAI-style models to clients. (They’re slower than cloud, but ideal for cheap iterations.)

Cloud behind the gateway: OpenAI GPT-5 / GPT-5-mini (Responses API). 
OpenAI

Emacs: gptel as the LLM client + mcp.el as your MCP client; both speak to the same base URL. gptel already documents MCP integration. 
GitHub
NonGNU ELPA

MCP tools: add the Playwright MCP server (browser automation) and point your Rust MCP servers at the same client. 
GitHub

(If you ever want an agent harness outside Emacs) OpenAI Agents SDK ships MCP support; it can host an agent that calls your MCP servers and talks to GPT-5. 
OpenAI GitHub Pages

Why this works for you
You get one endpoint inside Emacs/CLI. LiteLLM handles auto-routing (cheap local by default; escalate to GPT-5 when needed), budgets, spend tracking, and fallbacks (e.g., if local chokes, jump to mini, then GPT-5). You stop juggling keys and models. 
LiteLLM
+2
LiteLLM
+2

Cost model you can bank on (heavy use)
The reality: output tokens dominate. GPT-5 costs
$ = 1.25 * (input_M) + 10 * (output_M) [cached input = $0.125/M]. 
OpenAI Platform

Observed hourly ranges for coding (no images/search):

Typical: ~40k in / 30k out ⇒ $0.35/hr

Heavy: ~100k in / 50k out ⇒ $0.63/hr

Very heavy: ~150k in / 90k out ⇒ $1.09/hr
Monthly (30 days) at 8h/day: $84 → $261. If you’re seeing $75/day, you’re likely producing ~7–8M output tokens/day or lots of retries/long traces — the burn comes from output, not input. Cap it. 
OpenAI Platform

How to keep API costs civilized (and fast):

Default to GPT-5-mini; auto-escalate to GPT-5 only on test failure/ambiguity. (Mini’s $2/M output is 5× cheaper.) 
OpenAI Platform

Enable prompt caching for your stable system/repo context → 10× cheaper input. 
OpenAI Platform

Set max_output_tokens aggressively; long code dumps are where dollars go.

Use LiteLLM budgets: provider/day ($X), model/day ($Y), plus virtual keys with per-key caps for each project. 
LiteLLM
+1

Cost-based routing: prefer the cheapest model that meets latency/quality constraints; fallback upward only when needed. 
LiteLLM

The “don’t make me switch models” setup
Auto-router policy (LiteLLM) you can start with:

Route by size/complexity:

small edits / grep / quick diffs → local DeepSeek/Kimi

medium refactors / unit-test loops → GPT-5-mini

failing tests / ambiguous specs / tool-heavy (Playwright, DB) → GPT-5

Budget routing: if daily GPT-5 spend hits $N, automatically degrade to GPT-5-mini until reset.

Fallbacks: if locals timeout, jump to mini; if mini errors or exceeds retries, jump once to GPT-5.

Cost guardrails: cap max_output_tokens; enable caching; stream responses.
All of that is built-in knobs: budget routing, auto-routing, fallbacks, spend tracking. 
LiteLLM
+3
LiteLLM
+3
LiteLLM
+3

MCP in your Emacs/CLI agent
Client: mcp.el gives gptel MCP support; your agent can call Playwright MCP server (official) for UI automation and your Rust MCP servers for custom tasks. 
GitHub
+1

OpenAI Agents SDK also documents using MCP with GPT-5 if you prefer a standalone CLI agent/service that your Emacs client talks to. 
OpenAI GitHub Pages

Concrete monthly plan (high/heavy usage)
Option A — Keep Pro + budgeted API (my pick if you use ChatGPT UI a lot):

ChatGPT Pro = $200/mo for unlimited GPT-5 in ChatGPT (great for brainstorming/PRDs). No API credits. 
OpenAI
+1
OpenAI Help Center

API budget cap: $150–$300/mo (hard cap in OpenAI billing; mirrored as LiteLLM provider/model caps). At your “8h/day heavy” clip with the router (mini first + caching + caps), expect $120–$220/mo API in practice; when you really hammer tool use or request long diffs, it may drift toward your cap. 
OpenAI Platform

Total predictable: ~$350–$500/mo (Pro + API), but no nasty $75/day spikes because of caps + max_output_tokens.

Option B — API-first, skip Pro (if you live in Emacs/CLI):

Drop Pro; set API cap ~$250–$400/mo; same routing/caps. You lose ChatGPT’s “unlimited GPT-5” UI, but if you seldom use the web/desktop app, this is more cost-efficient. 
OpenAI Platform

Option C — Team (2+ seats): Not ideal solo, but if you need the workspace, $25–$30/user/mo; still no API credits. 
OpenAI

Practical setup notes (short)
Budget controls: Use OpenAI Billing for a hard monthly API cap; mirror with LiteLLM provider/model budgets + per-key daily caps. 
OpenAI
LiteLLM
+1

Router knobs: turn on cost-based routing, latency-based fallback, auto-router; define one alias “coding-auto” so your Emacs/CLI always calls the same model name. 
LiteLLM
+1

MCP tooling: add Playwright MCP (there are multiple implementations; Microsoft’s is solid). 
GitHub

Emacs: gptel + mcp.el talk to your LiteLLM endpoint; model switching is a key-chord, but you shouldn’t need it once router rules are in place. 
GitHub

Token hygiene: enforce max_output_tokens (biggest money lever), use prompt caching for repo/system context, and bias the model toward edits/patches over full rewrites. 
OpenAI Platform

TL;DR recommendations
Decide UI vs CLI first. If you still value the ChatGPT app a lot, keep Pro ($200) — but treat it as separate from your CLI costs. 
OpenAI

For CLI/Emacs, run LiteLLM Proxy with budget + cost-based routing: local (DeepSeek/Kimi) → GPT-5-mini → GPT-5 escalations. Cap output, enable caching. 
LiteLLM
+1
OpenAI Platform

Wire MCP: gptel + mcp.el; add Playwright MCP + your Rust servers. If you want a service agent, the OpenAI Agents SDK supports MCP as well. 
GitHub
+1
OpenAI GitHub Pages

Budget line items to stop bill shock:

OpenAI API hard cap (e.g., $250/mo) + LiteLLM daily caps ($10–$15/day GPT-5, $5–$10/day GPT-5-mini).

max_output_tokens defaults (e.g., 800–1200 for code).

Prompt caching on for repo/system prompts. 
OpenAI Platform

If you want, I can sketch a ready-to-drop LiteLLM config (router + budgets + aliases) and a minimal Emacs gptel/mcp.el snippet that points at it, plus an example Playwright MCP stanza so your agent can run UI tests from Emacs without you ever picking a model manually.

You said:
Please proceed with the config while I consider the implications and next actions based on your response.
ChatGPT said:

Thought for 5m 5s
Here’s a complete, drop-in config pack to get you a CLI/Emacs coding workflow that:

talks to GPT-5 in the cloud,

can fall back to (or explicitly use) local LLMs (DeepSeek/Kimi-K2 via llama.cpp),

puts all billing, routing, budgets, and caps in one place (LiteLLM proxy),

supports MCP tools (Playwright + your Rust MCP servers) inside Emacs.

I’ll start with a tight architecture recap (per your requested style), then all the files/commands you can paste in.

Requirements recap → Architecture → NFRs → High-level → Low-level
Your requirements (condensed): heavy daily usage from the CLI/Emacs, best-possible code quality (prefer GPT-5), predictable spend with hard caps, easy switching (or auto-routing) between cloud and local models, MCP tool use (Playwright + custom Rust), minimal friction.

Architecture (what you’ll run):

LiteLLM proxy (single endpoint, OpenAI-compatible): centralizes keys, per-provider & per-model budgets/caps, routing & fallbacks, a tiny UI, and virtual keys you’ll use from the CLI/Emacs. 
LiteLLM
+2
LiteLLM
+2

OpenAI (GPT-5) via LiteLLM (API usage is billed separately from ChatGPT Pro—Pro doesn’t include API credits). 
OpenAI Help Center
OpenAI

Local models via llama.cpp OpenAI-compatible server (so LiteLLM can talk to it just like any OpenAI provider). 
GitHub

Emacs with gptel (points at LiteLLM), plus mcp.el so GPT-5 (or your chosen model) can call MCP tools—Playwright and your Rust servers—from inside Emacs. 
GitHub
+1

Optional aider or your own CLI client pointed at LiteLLM for TUI sessions.

Non-functional priorities: cost ceilings (daily/monthly), speed (latency-based routing), quality (explicit “best” alias that prefers GPT-5), reliability (fallbacks), minimal context switching (one base URL everywhere).

High-level design:

One model alias per workflow:

coding-best → GPT-5 first, GPT-5-mini fallback (cloud-only best quality).

coding-cheap → local first, then GPT-5-mini (budget mode).

Provider budgets (e.g., $200/30d on OpenAI) + model budgets (e.g., $10/day on GPT-5) enforced by LiteLLM, visible via /provider/budgets + LiteLLM UI. 
LiteLLM

Latency-based routing globally (lowest latency among the deployments you include per alias). If you later prefer pure cost routing, flip one line. 
LiteLLM

CONFIG PACK (files & commands)
Directory layout (suggested)

llm-gateway/
├─ docker-compose.yml
├─ .env.example
├─ litellm/
│  └─ config.yaml
├─ scripts/
│  ├─ gen-virtual-key.sh
│  └─ show-budgets.sh
└─ emacs/
   └─ init-snippets.el
1) docker-compose.yml
version: "3.8"

services:
  postgres:
    image: postgres:16
    container_name: litellm-postgres
    restart: unless-stopped
    environment:
      POSTGRES_DB: litellm
      POSTGRES_USER: litellm
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD:?set in .env}
    ports:
      - "5432:5432"
    volumes:
      - pgdata:/var/lib/postgresql/data

  litellm:
    image: ghcr.io/berriai/litellm:main-latest
    container_name: litellm
    restart: unless-stopped
    ports:
      - "4000:4000"
    environment:
      # Admin / auth + DB
      LITELLM_MASTER_KEY: ${LITELLM_MASTER_KEY:?set in .env}
      DATABASE_URL: postgresql://litellm:${POSTGRES_PASSWORD}@postgres:5432/litellm
      # Provider keys
      OPENAI_API_KEY: ${OPENAI_API_KEY:?set in .env}
      # Optional: enable UI
      LITELLM_UI: "True"
      # Logging
      LITELLM_LOG: INFO
    volumes:
      - ./litellm/config.yaml:/app/config.yaml:ro
    depends_on:
      - postgres
    command: >
      bash -lc "litellm --config /app/config.yaml --host 0.0.0.0 --port 4000"

  llama:
    image: ghcr.io/ggml-org/llama.cpp:server
    container_name: llama-server
    restart: unless-stopped
    ports:
      - "8080:8080"
    # Mount your GGUFs under ./models
    volumes:
      - ./models:/models:ro
    environment:
      LLAMA_ARG_MODEL: ${LLAMA_MODEL_PATH:-/models/your-model.gguf}
      LLAMA_ARG_CTX_SIZE: ${LLAMA_CTX_SIZE:-16384}
      LLAMA_ARG_N_PARALLEL: 2
      LLAMA_ARG_PORT: 8080
      LLAMA_ARG_ENDPOINT_METRICS: 1
    # For CUDA build use ghcr.io/ggml-org/llama.cpp:server-cuda and e.g.:
    # deploy:
    #   resources:
    #     reservations:
    #       devices:
    #         - capabilities: [gpu]

volumes:
  pgdata:
LiteLLM’s Docker image/tags & Compose approach are canonical and documented. 
GitHub
LiteLLM

llama.cpp exposes OpenAI-compatible /v1/chat/completions so LiteLLM can treat it like any provider. 
GitHub

2) .env.example (copy to .env and fill)
# Postgres
POSTGRES_PASSWORD=change-me

# LiteLLM admin master key (used to create virtual keys, check budgets, etc.)
LITELLM_MASTER_KEY=sk-admin-REDACTED

# OpenAI (GPT-5) - note: ChatGPT Pro does NOT include API usage
OPENAI_API_KEY=sk-openai-REDACTED

# Local model path (GGUF). Put your DeepSeek/Kimi-K2 GGUF under ./models
LLAMA_MODEL_PATH=/models/deepseek-coder-X.gguf
LLAMA_CTX_SIZE=16384
Reminder: API usage is billed separately from ChatGPT subscriptions. 
OpenAI Help Center
OpenAI

3) litellm/config.yaml
# Single gateway for CLI & Emacs. OpenAI-compatible in/out.
# LiteLLM config reference and budgets/routing docs:
# - https://docs.litellm.ai/docs/proxy/config_settings
# - https://docs.litellm.ai/docs/routing
# - https://docs.litellm.ai/docs/proxy/provider_budget_routing

general_settings:
  # admin key for /key/*, /user/*, budgets, etc.
  master_key: os.environ/LITELLM_MASTER_KEY
  database_url: os.environ/DATABASE_URL
  # tiny built-in UI at /ui
  ui: true

# sensible defaults; router_settings overrides when duplicated
litellm_settings:
  timeout: 120
  num_retries: 2
  drop_params: true
  # stream is opt-in per request; gptel/aider can set it
  # stream: true

router_settings:
  # Global routing: pick the lowest-latency healthy deployment.
  # (You can swap to: cost-based-routing or usage-based-routing later.)
  routing_strategy: latency-based-routing
  # Provider-level hard budgets (resets on period)
  provider_budget_config:
    openai:
      budget_limit: 200.0     # $/period
      time_period: 1mo        # 1d / 30d / 1mo / etc.

# Model groups via duplicate model_name values.
# We'll define three aliases: coding-best, coding-cheap, and direct-gpt5.

model_list:
  # === "BEST QUALITY" alias (cloud-first, GPT-5 -> GPT-5-mini) ===
  - model_name: coding-best
    litellm_params:
      model: gpt-5               # OpenAI GPT-5
      api_key: os.environ/OPENAI_API_KEY
      # (Optional) per-model budget guardrail
      max_budget: 10.0           # $ cap for this deployment
      budget_duration: 1d
  - model_name: coding-best
    litellm_params:
      model: gpt-5-mini
      api_key: os.environ/OPENAI_API_KEY
      max_budget: 5.0
      budget_duration: 1d

  # === "CHEAP" alias (local-first, free-ish; then gpt-5-mini) ===
  - model_name: coding-cheap
    litellm_params:
      # local OpenAI-compatible server (llama.cpp)
      model: local-deepseek      # llama.cpp ignores value; needs *some* model string
      api_base: http://llama:8080/v1
      api_key: sk-local          # placeholder; most OAI-compatible servers ignore it
      # Tell LiteLLM cost is ~0 so latency decides; budgets still govern OpenAI below
      input_cost_per_token: 0.0
      output_cost_per_token: 0.0
  - model_name: coding-cheap
    litellm_params:
      model: gpt-5-mini
      api_key: os.environ/OPENAI_API_KEY
      max_budget: 5.0
      budget_duration: 1d

  # === Direct model names (for explicit selection) ===
  - model_name: gpt-5
    litellm_params:
      model: gpt-5
      api_key: os.environ/OPENAI_API_KEY
  - model_name: gpt-5-mini
    litellm_params:
      model: gpt-5-mini
      api_key: os.environ/OPENAI_API_KEY
Provider budgets and per-model budgets syntax is per LiteLLM docs (time periods like 1d/1mo are supported). You’ll be able to query /provider/budgets. 
LiteLLM

Latency-based routing is a built-in strategy; swap to cost-based in router_settings later if you like. 
LiteLLM

4) Bring-up
cd llm-gateway
cp .env.example .env    # fill values
docker compose up -d
# test health:
curl -s http://localhost:4000/health/readiness && echo
# tiny UI (spend logs, models):
#   http://localhost:4000/ui
5) Scripts
scripts/gen-virtual-key.sh
Generate a user key you’ll use from Emacs/CLI instead of the admin key.

#!/usr/bin/env bash
set -euo pipefail

BASE="http://localhost:4000"
ADMIN="Authorization: Bearer ${LITELLM_MASTER_KEY:?set in env}"

# Create a user (idempotent) and key limited to the model aliases you want:
USER_EMAIL="${1:-you@example.com}"

USER_ID=$(curl -sf -X POST "$BASE/user/new" \
  -H "$ADMIN" -H "Content-Type: application/json" \
  -d "{\"user_email\":\"$USER_EMAIL\"}" | jq -r '.user_id')

echo "User ID: $USER_ID"

KEY=$(curl -sf -X POST "$BASE/key/generate" \
  -H "$ADMIN" -H "Content-Type: application/json" \
  -d "{\"models\":[\"coding-best\",\"coding-cheap\",\"gpt-5\",\"gpt-5-mini\"],\"user_id\":\"$USER_ID\"}" \
  | jq -r '.key')

echo "Virtual key: $KEY"
scripts/show-budgets.sh
#!/usr/bin/env bash
set -euo pipefail
BASE="http://localhost:4000"
ADMIN="Authorization: Bearer ${LITELLM_MASTER_KEY:?set in env}"
curl -sf "$BASE/provider/budgets" -H "$ADMIN" | jq
(Provider budgets & the /provider/budgets endpoint are part of the budget routing feature.) 
LiteLLM

6) Emacs integration (emacs/init-snippets.el)
gptel → LiteLLM (one base URL, one key)
;; gptel (install from GNU ELPA/MELPA)
(require 'gptel)

;; Point gptel to your LiteLLM proxy (OpenAI-compatible)
(setq gptel-openai-base-url "http://localhost:4000/v1")
(setq gptel-api-key (getenv "LITELLM_VIRTUAL_KEY")) ;; from gen-virtual-key.sh

;; Backends for the three “models” (really aliases + direct)
(gptel-make-openai "LLM-Gateway"
  :host (url-host (url-generic-parse-url gptel-openai-base-url))
  :endpoint "/v1/chat/completions"  ;; LiteLLM supports OAI routes
  :key gptel-api-key
  :models '("coding-best" "coding-cheap" "gpt-5" "gpt-5-mini"))

;; Defaults: make the “BEST” alias primary
(setq gptel-backend (gptel-backend "LLM-Gateway"))
(setq gptel-model "coding-best")
gptel configuration to a custom OpenAI-compatible base is standard. 
GitHub

MCP tools in Emacs (Playwright + your Rust servers)
;; mcp.el + gptel integration
;; https://github.com/lizqwerscott/mcp.el
(require 'mcp)
(require 'gptel-mcp) ;; provided by mcp.el repo; integrates tools with gptel

;; Start MCP servers (Playwright + your Rust MCP)
;; Playwright MCP (Node): install with `npm i -g @microsoft/mcp-playwright` (repo README).
;; https://github.com/microsoft/prompt-foo/tree/main/packages/mcp-servers/playwright
(mcp-register
 :name "playwright"
 :command "npx"
 :args '("@microsoft/mcp-playwright@latest")
 :env nil)

;; Your Rust MCP binary (adjust path/flags)
(mcp-register
 :name "rust-mcp"
 :command "/usr/local/bin/your-rust-mcp"
 :args '()
 :env nil)

;; Bridge MCP tools into gptel sessions
(add-hook 'gptel-post-response-hook #'gptel-mcp-attach)  ;; lazily attach tools
;; Or attach up-front:
(add-hook 'gptel-mode-hook #'gptel-mcp-attach)
mcp.el provides Emacs client & gptel integration; Microsoft’s Playwright MCP server is supported and installable via npm. 
GitHub

Tip: With MCP attached, ask GPT-5 in Emacs to “run Playwright to click X and screenshot Y”—the tool execution will be delegated via MCP.

7) CLI usage (curl / aider / your Rust fork)
Sanity check (curl):

# Use a virtual key generated earlier (not the admin key)
export OPENAI_API_KEY="$(./scripts/gen-virtual-key.sh you@host | tail -1 | awk '{print $3}')"

curl http://localhost:4000/v1/chat/completions \
  -H "Authorization: Bearer $OPENAI_API_KEY" -H "Content-Type: application/json" \
  -d '{"model":"coding-best","messages":[{"role":"user","content":"In Rust, write a function that ..."}]}'
Aider (example) pointed at LiteLLM:

export OPENAI_API_BASE="http://localhost:4000/v1"
export OPENAI_API_KEY="$OPENAI_API_KEY"
aider --model coding-best
Aider supports OpenAI-compatible endpoints, so it will route through LiteLLM.

Your opencode (Rust clone): set the same OPENAI_API_BASE/OPENAI_API_KEY and default model coding-best (fallback to coding-cheap when you want to keep costs near zero).

How this addresses your pain points
One base URL & one key in Emacs and CLI. LiteLLM handles which provider/model is used; you don’t. 
LiteLLM

Caps where you want them: $200/mo at the provider level (OpenAI) + per-model daily caps (e.g., $10/day on GPT-5). If you hit a cap, requests fail fast or route to cheaper models depending on which alias you call. Query /provider/budgets anytime. 
LiteLLM

Best vs Cheap knobs:

coding-best (GPT-5 first) for quality/speed,

coding-cheap (local first) when you’re grinding or offline.

MCP tool use in Emacs: Playwright + your Rust MCP servers available right inside chats via mcp.el + gptel-mcp. 
GitHub

Local models are “first-class”: llama.cpp serves an OpenAI-compatible API, so routing/fallbacks “just work.” 
GitHub

Risks / knobs you may tune next
If you prefer automatic cost minimization over latency, flip:

router_settings:
  routing_strategy: cost-based-routing
(When using cost-based with a free local model, LiteLLM will prefer local almost always—keep coding-best cloud-only to preserve quality.) 
Rasa

For multi-instance LiteLLM or strict provider-budget accounting across replicas, add Redis (LiteLLM uses it for some routing/budget synchronization). 
LiteLLM

ChatGPT Pro ≠ API credits. To use GPT-5 via LiteLLM you must have API billing set on the OpenAI platform. 
OpenAI

Quick next steps (actionable)
Fill .env, docker compose up -d.

Run scripts/gen-virtual-key.sh you@host and export LITELLM_VIRTUAL_KEY in your shell/Emacs.

Drop emacs/init-snippets.el into your init and M-x gptel → pick LLM-Gateway and coding-best.

Attach MCP tools and try a Playwright-assisted coding task.

Watch spend at http://localhost:4000/ui and with scripts/show-budgets.sh. 
LiteLLM

If you want, I can also generate:

a systemd unit for each service instead of Compose,

a pre-tuned coding-auto alias that classifies prompts and routes (e.g., short edits → local, refactors/tests → GPT-5),

a small Rust client that wraps LiteLLM with your preferred defaults.

You said:
What are the Pros/Cons of LiteLLM vs OpenRouter?
ChatGPT said:

Thought for 2m 4s
Here’s the short, engineering-grade comparison you can trust.

Framing
You’re choosing between:

LiteLLM (self-hosted gateway/proxy): you run it; it can route to any cloud (OpenAI, Anthropic, Bedrock, etc.) and your local models (llama.cpp/Ollama/vLLM), with cost controls, routing, and a UI. 
LiteLLM

OpenRouter (hosted aggregator): one hosted API for hundreds of models across many providers; handles fallbacks/provider selection and offers extras (web-search, PDF processing). 
OpenRouter
+2
OpenRouter
+2

LiteLLM — Pros
Local + cloud under one roof (works with OpenAI-compatible local servers like llama.cpp); single endpoint for Emacs/CLI. 
LiteLLM

Tight cost control: provider/model/team budgets, virtual keys, spend tracking; enforce daily/monthly caps yourself. 
LiteLLM
+4
LiteLLM
+4
LiteLLM
+4

Smart routing you choose: latency-based, rate-limit-aware, lowest-cost, etc.; load-balance multiple backends. 
LiteLLM
+1

Caching options (exact + semantic; Redis/S3/etc.) to reduce cost/latency; prompt-caching pass-through for providers that support it. 
LiteLLM
+3
LiteLLM
+3
LiteLLM
+3

Observability & UI with per-request cost/usage, logging hooks (Langfuse/OTel/etc.). 
LiteLLM
+1

LiteLLM — Cons / risks
You operate it (DB, Redis optional, upgrades, HA).

Data retention is your policy; you must configure what to store (prompts/responses) and secure the DB. 
LiteLLM
DEV Community

Features evolve quickly (occasional breaking changes—keep an eye on release notes). 
LiteLLM

OpenRouter — Pros
Zero infra: one key, 400+ models, multi-provider fallback/selection; OpenAI-like schema. 
OpenRouter
+2
OpenRouter
+2

Provider routing/fallbacks and knobs (disable fallbacks; order preferred providers). 
OpenRouter

Extras you might use:
– Web search plugin (priced per results, default ≈ $0.02/call). 
OpenRouter
+1

– PDF parsing engines, including OCR, with clear per-page pricing. 
OpenRouter

Latency overhead is modest (doc says ~40 ms typical; keep credits topped to avoid extra checks). 
OpenRouter

Privacy controls: ZDR (zero-data-retention) routing, provider policy filters. 
OpenRouter
+1

OpenRouter — Cons / caveats
Fee model: you buy credits and pay a 5.5% fee (min $0.80) on purchases (crypto 5%); pricing to providers is pass-through (no markup). 
OpenRouter

Note: they’ve said the legacy 5% “BYOK” usage fee will be replaced by a fixed subscription in the future (TBD). Verify current status if you plan BYOK. 
OpenRouter

Hosted middleman: data still transits OpenRouter; you must review their privacy/retention and each provider’s policy. 
OpenRouter
+1

Rate-limits/credit health matter (free-tier model limits; keep credits ≥$10–20 to avoid slow paths). 
OpenRouter
+2
OpenRouter
+2

Feature add-ons can add cost (web-search, OCR), so watch totals. 
OpenRouter
+1

Which fits your workflow?
You want one endpoint that includes your local DeepSeek/Kimi-K2 + GPT-5 and hard daily caps → LiteLLM. You control routing (cheap/local by default; escalate to GPT-5), and enforce spend with provider/model budgets & virtual keys. 
LiteLLM
+1

You want the simplest hosted key for many clouds/models, don’t need locals, and value built-in fallbacks + web/PDF add-ons → OpenRouter. 
OpenRouter
+1

Hybrid that works well for power users like you
Run LiteLLM as your single endpoint and add OpenRouter as just another upstream (plus OpenAI direct, plus your local llama.cpp). You still get budgets/keys/UI locally, and you can toggle OpenRouter’s catalog when you need its models/plugins—accepting the credit fee for that slice only. 
LiteLLM

If you want, I can drop a tiny config.yaml diff that adds OpenRouter as an upstream to the LiteLLM pack I gave you, plus an Emacs gptel model alias (coding-or) so you can A/B it against coding-best.

You said:
yes, please show an approach that lets me easily switch emacs access to LLMs between direct local Ollama and llama.cpp access, local LiteLLM access to Anthropic, Deepseek, Gemini, Kimi, and OpenAI keys (possibly others like Open Router).  Show elisp code, litellm installation and configuration, test scripts to try each kind of LLM, each provider, each key.
ChatGPT said:

Thought for 1m 56s
Awesome—here’s a clean, Emacs-first setup that lets you flip between:

Direct local (Ollama, llama.cpp), and

Local LiteLLM proxy (routing to OpenAI, Anthropic, DeepSeek, Gemini, OpenRouter, plus your locals)

…with one or two key chords in Emacs, and simple CLI test scripts for each path.

High-level design
Two access modes

Direct: Emacs/CLI → Ollama (:11434/v1) or llama.cpp server (:8080/v1).

Proxy: Emacs/CLI → LiteLLM (:4000/v1) → upstreams (OpenAI/Anthropic/DeepSeek/Gemini/OpenRouter) and your locals.

Switching: Emacs gptel provides multiple backends + models. You can switch backend (direct vs proxy) and model (GPT-5, Claude, DeepSeek, Gemini, local Kimi-K2, etc.) interactively.

Budget control: you can keep all the cost caps and routing rules in LiteLLM (even while still having the “Direct” path available for emergencies/offline work).

Non-functional goals (how the config meets them)
Low friction: one Emacs command to switch backend/model; one probe.sh to test any base/model/key.

Cost visibility: LiteLLM logs and per-model aliases; your “Direct” path bypasses any proxy if you want.

Performance: direct local for small edits; cloud via proxy when quality/speed matters.

Extensibility: add/remove providers by editing a single YAML and reloading the proxy.

Low-level: install & run LiteLLM (two options)
Option A: Docker (recommended)
files

llm-gateway/
├─ docker-compose.yml
├─ .env
└─ litellm/
   └─ config.yaml
docker-compose.yml

version: "3.8"
services:
  litellm:
    image: ghcr.io/berriai/litellm:main-latest
    container_name: litellm
    restart: unless-stopped
    ports: ["4000:4000"]
    environment:
      LITELLM_MASTER_KEY: ${LITELLM_MASTER_KEY}
      OPENAI_API_KEY: ${OPENAI_API_KEY}
      ANTHROPIC_API_KEY: ${ANTHROPIC_API_KEY}
      DEEPSEEK_API_KEY: ${DEEPSEEK_API_KEY}
      GOOGLE_API_KEY: ${GOOGLE_API_KEY}        # Gemini
      OPENROUTER_API_KEY: ${OPENROUTER_API_KEY}
      LITELLM_LOG: INFO
      LITELLM_UI: "True"
    volumes:
      - ./litellm/config.yaml:/app/config.yaml:ro
    command: >
      bash -lc "litellm --config /app/config.yaml --host 0.0.0.0 --port 4000"
.env (example)
(fill what you have; unset ones are fine)

LITELLM_MASTER_KEY=sk-admin-CHANGE-ME
OPENAI_API_KEY=sk-openai-...
ANTHROPIC_API_KEY=sk-ant-...
DEEPSEEK_API_KEY=sk-deepseek-...
GOOGLE_API_KEY=AIza...
OPENROUTER_API_KEY=or-...
litellm/config.yaml (models + handy aliases)

general_settings:
  master_key: os.environ/LITELLM_MASTER_KEY
  ui: true

router_settings:
  routing_strategy: latency-based-routing

# Define explicit "direct" names per provider,
# plus a few "alias groups" that route among multiple backends.
model_list:
  # --- CLOUD DIRECTS (via LiteLLM) ---
  - model_name: openai-gpt5
    litellm_params:
      model: openai/gpt-5
      api_key: os.environ/OPENAI_API_KEY

  - model_name: openai-gpt5-mini
    litellm_params:
      model: openai/gpt-5-mini
      api_key: os.environ/OPENAI_API_KEY

  - model_name: anthropic-sonnet
    litellm_params:
      # replace with current Claude model id you use
      model: anthropic/claude-3-7-sonnet
      api_key: os.environ/ANTHROPIC_API_KEY

  - model_name: deepseek-coder
    litellm_params:
      # pick your preferred DeepSeek API model id
      model: deepseek/deepseek-coder
      api_key: os.environ/DEEPSEEK_API_KEY

  - model_name: gemini-1.5-pro
    litellm_params:
      model: google/gemini-1.5-pro
      api_key: os.environ/GOOGLE_API_KEY

  # --- OPENROUTER (optional) ---
  - model_name: openrouter-gpt5
    litellm_params:
      # Use a model id available on OpenRouter (example)
      model: openai/gpt-5
      api_base: https://openrouter.ai/api/v1
      api_key: os.environ/OPENROUTER_API_KEY

  # --- LOCAL DIRECTS (through LiteLLM; also usable "direct" below) ---
  - model_name: local-llamacpp-kimi
    litellm_params:
      model: kimi-k2          # llama.cpp uses this name; adjust to your GGUF id
      api_base: http://127.0.0.1:8080/v1
      api_key: sk-local

  - model_name: local-ollama-deepseek
    litellm_params:
      model: deepseek-coder:latest   # must match your pulled model tag
      api_base: http://127.0.0.1:11434/v1
      api_key: sk-local

  # --- ALIASES (same name repeated -> router picks a healthy/fast one) ---
  - model_name: coding-best        # "Best available" cloud path
    litellm_params: { model: openai/gpt-5,       api_key: os.environ/OPENAI_API_KEY }
  - model_name: coding-best
    litellm_params: { model: anthropic/claude-3-7-sonnet, api_key: os.environ/ANTHROPIC_API_KEY }

  - model_name: coding-cheap       # Local first, then cheap cloud
    litellm_params: { model: kimi-k2,     api_base: http://127.0.0.1:8080/v1, api_key: sk-local }
  - model_name: coding-cheap
    litellm_params: { model: deepseek-coder:latest, api_base: http://127.0.0.1:11434/v1, api_key: sk-local }
  - model_name: coding-cheap
    litellm_params: { model: openai/gpt-5-mini,    api_key: os.environ/OPENAI_API_KEY }

  - model_name: coding-auto        # a “mixed” alias to let the router choose
    litellm_params: { model: openai/gpt-5,         api_key: os.environ/OPENAI_API_KEY }
  - model_name: coding-auto
    litellm_params: { model: google/gemini-1.5-pro, api_key: os.environ/GOOGLE_API_KEY }
  - model_name: coding-auto
    litellm_params: { model: anthropic/claude-3-7-sonnet, api_key: os.environ/ANTHROPIC_API_KEY }
  - model_name: coding-auto
    litellm_params: { model: deepseek/deepseek-coder,     api_key: os.environ/DEEPSEEK_API_KEY }
run

cd llm-gateway
docker compose up -d
curl -s http://localhost:4000/health/readiness && echo
curl -s http://localhost:4000/v1/models | jq '.data[].id'  # see models/aliases
Option B: Python (pipx)
pipx install "litellm[proxy]"
export LITELLM_MASTER_KEY=sk-admin-CHANGE-ME
export OPENAI_API_KEY=sk-openai-...
# (…others if you have them)
litellm --config ./litellm/config.yaml --host 0.0.0.0 --port 4000
Local servers (direct mode)
Ollama

# install per your distro
ollama serve
ollama pull deepseek-coder:latest
# OpenAI-compatible endpoint: http://127.0.0.1:11434/v1
curl -s http://127.0.0.1:11434/v1/models | jq
llama.cpp server (CUDA build recommended on your boxes)

./server -m /path/to/kimi-k2.gguf -c 16384 -ngl 33 -p 8080
# OpenAI-compatible endpoint: http://127.0.0.1:8080/v1
curl -s http://127.0.0.1:8080/v1/models | jq
Emacs: multiple backends + fast switching (gptel)
Drop this into (or load-file) something like ~/.emacs.d/gptel-llm-switching.el.

;;; gptel-llm-switching.el --- One-key switching among locals/proxy/providers

(require 'gptel)

;; ---------- Backends ----------
;; 1) Direct: Ollama (OpenAI-compatible)
(gptel-make-openai "Ollama-Direct"
  :host "127.0.0.1:11434"
  :endpoint "/v1"
  :key "sk-local"                       ;; ignored by Ollama
  :stream t
  :models '("deepseek-coder:latest" "qwen2.5-coder:7b" "llama3.1:8b"))

;; 2) Direct: llama.cpp (OpenAI-compatible)
(gptel-make-openai "LlamaCpp-Direct"
  :host "127.0.0.1:8080"
  :endpoint "/v1"
  :key "sk-local"                       ;; ignored by llama.cpp
  :stream t
  :models '("kimi-k2" "deepseek-coder-v2" "qwen2.5-coder-7b"))

;; 3) Proxy: LiteLLM gateway (cloud+locals via one URL)
;; Make sure LITELLM_USER_KEY is exported if you use per-user keys; otherwise reuse admin for testing.
(defvar my/litellm-url "http://127.0.0.1:4000")
(defvar my/litellm-key (or (getenv "LITELLM_VIRTUAL_KEY")
                           (getenv "LITELLM_MASTER_KEY")
                           "sk-admin-CHANGE-ME"))

(gptel-make-openai "LiteLLM"
  :host "127.0.0.1:4000"
  :endpoint "/v1"
  :protocol "http"
  :key my/litellm-key
  :stream t
  :models '(
            ;; aliases
            "coding-best" "coding-cheap" "coding-auto"
            ;; directs
            "openai-gpt5" "openai-gpt5-mini"
            "anthropic-sonnet"
            "deepseek-coder"
            "gemini-1.5-pro"
            "openrouter-gpt5"
            "local-llamacpp-kimi" "local-ollama-deepseek"))

;; ---------- Quick switching ----------
(defvar my/gptel-backend-names '("LiteLLM" "Ollama-Direct" "LlamaCpp-Direct"))

(defun my/gptel-select-backend ()
  "Interactively select a gptel backend and make it default."
  (interactive)
  (let* ((choice (completing-read "Backend: " my/gptel-backend-names nil t))
         (backend (gptel-backend choice)))
    (setq gptel-backend backend)
    (message "gptel backend -> %s" choice)))

(defun my/gptel-select-model ()
  "Interactively select a model for the current backend."
  (interactive)
  (let* ((backend gptel-backend)
         (models (gptel-backend-models backend))
         (choice (completing-read "Model: " models nil t)))
    (setq gptel-model choice)
    (message "gptel model -> %s" choice)))

;; Defaults: use LiteLLM + coding-best; bind easy keys
(setq gptel-backend (gptel-backend "LiteLLM"))
(setq gptel-model "coding-best")
(global-set-key (kbd "C-c M-b") #'my/gptel-select-backend)
(global-set-key (kbd "C-c M-m") #'my/gptel-select-model)

(provide 'gptel-llm-switching)
;;; gptel-llm-switching.el ends here
Usage in Emacs

M-x my/gptel-select-backend → pick LiteLLM, Ollama-Direct, or LlamaCpp-Direct

M-x my/gptel-select-model → pick any model/alias exposed by that backend

M-x gptel → chat in any buffer; C-c M-m to change model on the fly

You can also pin backends/models per project by setting gptel-backend / gptel-model in .dir-locals.el.

CLI test scripts
Create a tests/ directory and drop these in. All scripts accept env overrides and show what they’re calling. Each returns the first assistant message to stderr/stdout for quick validation.

tests/probe.sh (generic OpenAI-compatible probe)
#!/usr/bin/env bash
set -euo pipefail
BASE="${BASE:-http://127.0.0.1:4000/v1}"    # LiteLLM by default
KEY="${KEY:-$LITELLM_MASTER_KEY}"
MODEL="${MODEL:-coding-auto}"
PROMPT="${PROMPT:-Say the string 'OK' and your provider name.}"

echo "→ BASE=$BASE MODEL=$MODEL" >&2
curl -sS "$BASE/chat/completions" \
  -H "Authorization: Bearer ${KEY:-sk-local}" \
  -H "Content-Type: application/json" \
  -d "$(jq -nc --arg m "$MODEL" --arg p "$PROMPT" \
        '{model:$m, messages:[{role:"user", content:$p}], max_tokens:128}')" \
| jq -r '.choices[0].message.content // .error.message'
Provider-specific wrappers (one-liners)
LiteLLM / OpenAI GPT-5

#!/usr/bin/env bash
BASE=http://127.0.0.1:4000/v1 KEY="${LITELLM_MASTER_KEY}" MODEL=openai-gpt5 \
PROMPT="Rust: print the sum of [1,2,3] using iterators only." \
bash tests/probe.sh
LiteLLM / Anthropic

BASE=http://127.0.0.1:4000/v1 KEY="${LITELLM_MASTER_KEY}" MODEL=anthropic-sonnet \
PROMPT="Write a property-based test for a ring-buffer in Rust." \
bash tests/probe.sh
LiteLLM / DeepSeek API

BASE=http://127.0.0.1:4000/v1 KEY="${LITELLM_MASTER_KEY}" MODEL=deepseek-coder \
PROMPT="Refactor this function for readability; return a patch diff." \
bash tests/probe.sh
LiteLLM / Gemini

BASE=http://127.0.0.1:4000/v1 KEY="${LITELLM_MASTER_KEY}" MODEL=gemini-1.5-pro \
PROMPT="Explain lifetime elision rules in Rust with examples." \
bash tests/probe.sh
LiteLLM / OpenRouter (optional)

BASE=http://127.0.0.1:4000/v1 KEY="${LITELLM_MASTER_KEY}" MODEL=openrouter-gpt5 \
PROMPT="Summarize how to write an Emacs major-mode." \
bash tests/probe.sh
Direct / Ollama

BASE=http://127.0.0.1:11434/v1 KEY=sk-local MODEL=deepseek-coder:latest \
PROMPT="Implement a small parser combinator in Rust." \
bash tests/probe.sh
Direct / llama.cpp

BASE=http://127.0.0.1:8080/v1 KEY=sk-local MODEL=kimi-k2 \
PROMPT="Show a minimal TDD red/green loop in Rust." \
bash tests/probe.sh
List models per base

#!/usr/bin/env bash
set -euo pipefail
BASE="${1:-http://127.0.0.1:4000/v1}"
KEY="${2:-$LITELLM_MASTER_KEY}"
curl -sS "$BASE/models" -H "Authorization: Bearer ${KEY:-sk-local}" | jq
Try everything (small smoke test)

#!/usr/bin/env bash
set -euo pipefail
echo "LiteLLM: GPT-5"; BASE=http://127.0.0.1:4000/v1 KEY="$LITELLM_MASTER_KEY" MODEL=openai-gpt5 PROMPT="Say OK" bash tests/probe.sh
echo "LiteLLM: Claude"; BASE=http://127.0.0.1:4000/v1 KEY="$LITELLM_MASTER_KEY" MODEL=anthropic-sonnet PROMPT="Say OK" bash tests/probe.sh
echo "LiteLLM: DeepSeek"; BASE=http://127.0.0.1:4000/v1 KEY="$LITELLM_MASTER_KEY" MODEL=deepseek-coder PROMPT="Say OK" bash tests/probe.sh
echo "LiteLLM: Gemini"; BASE=http://127.0.0.1:4000/v1 KEY="$LITELLM_MASTER_KEY" MODEL=gemini-1.5-pro PROMPT="Say OK" bash tests/probe.sh
echo "Direct: Ollama"; BASE=http://127.0.0.1:11434/v1 KEY=sk-local MODEL=deepseek-coder:latest PROMPT="Say OK" bash tests/probe.sh
echo "Direct: llama.cpp"; BASE=http://127.0.0.1:8080/v1 KEY=sk-local MODEL=kimi-k2 PROMPT="Say OK" bash tests/probe.sh
Notes / tips
Model IDs vary—swap the placeholders for what you actually use (e.g., Claude model id, the exact DeepSeek API id, your Kimi-K2 GGUF name).

Ollama & llama.cpp must have matching model names to what you pass in model: for /v1/chat/completions.

Gemini direct: gptel can talk to Gemini directly, but standardizing via LiteLLM means all your Emacs traffic shares one schema and can leverage routing/cost controls.

Per-user keys (optional): LiteLLM can mint virtual keys so your CLI, Emacs, and agents don’t use the admin key; if you want, I can add a tiny script to generate them and an Emacs helper to read from a .envrc per project.

If you want me to tailor the YAML to your exact cloud model ids and add your MCP tool attach in Emacs (Playwright + Rust MCP), say the word and I’ll drop a ready-to-paste variant.



No file chosenNo file chosen
ChatGPT can make mistakes. Check important info.
